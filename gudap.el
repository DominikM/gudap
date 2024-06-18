;;; gudap.el --- Debug Adapter Protocol client for Emacs -*- lexical-binding: t -*-

;; Author: Dominik Martinez <dominikmartinez@pm.me>
;; URL:
;; Version: 0.1.0
;; Package-Requires:
;; Keywords: languages, debug, dap

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; gudap.el is a Debug Adapter Protocol client for Emacs.
;;
;; To use, add the following to your init file:
;;
;;   (require 'gudap)
;;
;; Code ~heavily~ adapted from Eglot.

;;; Code:

(require 'jsonrpc)
(require 'gud)

(defgroup gudap nil
  "Interaction with Debug Adapter Protocol servers."
  :prefix "gudap-"
  :group 'applications)

(defclass gudap-server ()
  ((name
    :initarg :name)
   (dap-process
    :initarg :dap-process
    :accessor gudap-dap-process)
   (gud-buffer
    :initarg :gud-buffer
    :accessor gudap-gud-buffer)
   (events-buffer
    :initarg :events-buffer
    :accessor gudap-events-buffer)
   (launch-config
    :initarg :launch-config
    :accessor gudap-launch-config)
   (expected-bytes
    :initform nil
    :accessor gudap-expected-bytes)
   (seq
    :initform 1
    :accessor gudap-seq)
   (initialized
    :initform nil
    :accessor gudap-initialized)
   (breakpoints
    :initform (make-hash-table :test 'equal)
    :accessor gudap-breakpoints)
   (capabilities
    :accessor gudap-capabilities)
   (callbacks
    :initform (make-hash-table :test 'equal)
    :accessor gudap-callbacks)
   (current-thread
    :initform nil
    :accessor gudap-current-thread)
   (current-frame
    :initform nil
    :accessor gudap-current-frame)
   (stopped
    :initform t
    :accessor gudap-stopped)))

(defclass breakpoint ()
  ((id
    :initarg :id
    :accessor gudap-bp-id)
   (verified
    :initarg :verified
    :accessor gudap-bp-verified)
   (source
    :initarg :source
    :accessor gudap-bp-source)
   (line
    :initarg :line
    :accessor gudap-bp-line)))

(defclass source-breakpoint (breakpoint)
  ((condition
    :initarg :condition
    :initform nil
    :accessor gudap-source-bp-condition)))

(cl-defmethod gudap-bp-update ((bp source-breakpoint) server-bp)
  (cl-destructuring-bind (&key id verified source line &allow-other-keys) server-bp
    (make-instance 'source-breakpoint
		   :id id
		   :verified verified
		   :source source
		   :line line
		   :condition (gudap-source-bp-condition bp))))

(cl-defmethod gudap-bp-params ((bp source-breakpoint))
  (with-slots (line condition) bp
    (list :line line
	  :condition condition)))

(defun gudap-bps-params (bps)
  (mapcar 'gudap-bp-params bps))

(defvar gudap-server-programs-and-launch
  '(((c++-mode c-mode rust-mode) "lldb-vscode" (lambda ()
						  (list :name "gudap-lldb"
							:type "lldb-vscode"
							:request "launch"
							:program (read-file-name "Program to debug? "))))))
(defvar gudap-active-server nil)

(defun gudap--buffer-line ()
  (save-restriction
    (widen)
    (+ (count-lines (point-min) (point))
       (if (bolp) 1 0))))

(defmacro gudap-with-initialized (server-var &rest body)
  (declare (indent defun))
  `(let* ((server ,server-var)
	  (initialized (gudap-initialized server)))
     (if initialized
	 (progn ,@body))))

(defun gudap--gud-run (arg)
  (gudap-with-initialized gudap-active-server
    (gudap-send-request
     gudap-active-server
     'configurationDone
     '())))

(defun gudap--gud-cont (arg)
  (gudap-with-initialized gudap-active-server
    (gudap-send-request
     server
     'continue
     (list :threadId (gudap-current-thread server)))))

(defun gudap-update-breakpoints (server file-path bps server-bps)
  (puthash file-path
	   (cl-loop for bp across bps
		    for server-bp across server-bps
		    collect (gudap-bp-update bp server-bp))
	   (gudap-breakpoints server)))

(defun gudap--gud-break (arg)
  (gudap-with-initialized gudap-active-server
    (let* ((file-path (buffer-file-name))
	   (current-bps (gethash file-path (gudap-breakpoints server)))
	   (new-bp (make-instance 'source-breakpoint
				  :line (line-number-at-pos)
				  :condition (if (> arg 1) (read-string "Expression: "))))
	   (bps (vconcat (cons new-bp current-bps))))
      (gudap-send-request
       server
       'setBreakpoints
       (list :source (gudap--dap-type-source file-path)
	     :breakpoints (vconcat (mapcar 'gudap-bp-params bps)))
       (lambda (server success body)
	 (gudap-update-breakpoints server file-path bps (plist-get body :breakpoints)))))))

(defun gudap--gud-break-cond (arg)
  (gudap--gud-break (read-string "Expression: ")))

(defun gudap--gud-remove (arg)
  (gudap-with-initialized gudap-active-server
    (let* ((file-path (buffer-file-name))
	   (line (line-number-at-pos))
	   (current-breakpoints (gethash file-path (gudap-breakpoints server))))
      (if current-breakpoints
	  (let* ((filtered-breakpoints (seq-filter
					(lambda (bp) (/= line (gudap-bp-line bp)))
					current-breakpoints))
		 (source-breakpoints (vconcat (gudap-bps-params filtered-breakpoints))))
	    (if (= (length current-breakpoints) (length source-breakpoints))
		(gudap--message "No breakpoints at location!")
	      (remhash file-path (gudap-breakpoints server))
	      (gudap-send-request
	       server
	       'setBreakpoints
	       (list :source (gudap--dap-type-source file-path)
		     :breakpoints source-breakpoints)
	       (lambda (server success body)
		 (gudap-update-breakpoints server file-path source-breakpoints (plist-get body :breakpoints))))))
	(gudap--message "No breakpoints at location!")))))

(defun gudap--gud-next (arg)
  (gudap-with-initialized gudap-active-server
    (if (gudap-stopped server)
	(gudap-send-request
	 server
	 'next
	 (list :threadId (gudap-current-thread server))))))

(defun gudap--gud-stepi (arg)
  (gudap-with-initialized gudap-active-server
    (if (gudap-stopped server)
	(gudap-send-request
	 server
	 'stepIn
	 (list :threadId (gudap-current-thread server))))))
	
(defun gudap--dap-type-source (file-path)
  (list :path file-path))

(defmacro def-gudap-cmd (gud-cmd gudap-fn key)
  `(progn
     (defalias ',gud-cmd (lambda (arg)
			   (interactive "p")
			   (,gudap-fn arg)))
     ,(if key `(local-set-key ,(concat "\C-c" key) #',gud-cmd))
     ,(if key `(define-key gud-global-map ,key #',gud-cmd))))

(defun gudap ()
  (interactive)
  (let ((program-and-launch-config (gudap--guess-contact))
	(gud-comint-buffer (gud-common-init "cat"
					    (lambda (_file args) args)
					    #'identity)))
    (setq comint-prompt-regexp "^>")
    (gudap--connect (car program-and-launch-config) (cdr program-and-launch-config) gud-comint-buffer)

    (def-gudap-cmd gud-break      gudap--gud-break      "\C-b")
    (def-gudap-cmd gud-cont       gudap--gud-cont       "\C-r")
    (def-gudap-cmd gud-run        gudap--gud-run        nil)
    (def-gudap-cmd gud-remove     gudap--gud-remove     "\C-d")
    (def-gudap-cmd gud-next       gudap--gud-next       "\C-n")
    (def-gudap-cmd gud-stepi      gudap--gud-stepi      "\C-i")

    (setq gdb-first-prompt t)
    (setq gud-running nil)
    (add-hook 'comint-input-filter-functions
	      (lambda (input) (gudap-gud-input gudap-active-server input)))
    (gudap-gud-prompt gudap-active-server)))

    ;; (setq-local gud-minor-mode 'gdbmi)
    ;; (setq-local gdb-control-level 0)

    ;; (gdb-setup-windows))

(defun gudap--gdb-update ()
  (when gdb-first-prompt
    (gdb-force-mode-line-update
     (propertize "initializing..." 'face font-lock-variable-name-face))
    (gudap--gdb-init-1)
    (setq gdb-first-prompt nil))

  (gdb-get-buffer-create 'gdb-threads-buffer)
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)

  ;;(gdb-get-changed-registers)
  (unless no-proc
    (gdb-emit-signal gdb-buf-publisher 'update))

  ;; (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
  ;;   (dolist (var gdb-var-list)
  ;;     (setcar (nthcdr 5 var) nil))
  ;;   (gdb-var-update))
  )

(defun gdb-init-1 ()
  ;; (Re-)initialize.
  (setq gdb-selected-frame nil
	gdb-frame-number nil
        gdb-thread-number nil
	gdb-var-list nil
	gdb-output-sink 'user
	gdb-location-alist nil
	gdb-source-file-list nil
	gdb-last-command nil
	gdb-token-number 0
	gdb-handler-list '()
	gdb-prompt-name nil
	gdb-first-done-or-error t
	gdb-target-async-checked nil
	gdb-buffer-fringe-width (car (window-fringes))
	gdb-debug-log nil
	gdb-source-window-list nil
	gdb-inferior-status nil
	gdb-continuation nil
        gdb-buf-publisher '()
        gdb-threads-list '()
        gdb-breakpoints-list '()
        gdb-register-names '()
        gdb-supports-non-stop nil
        gdb-non-stop nil
        gdb-debuginfod-enable gdb-debuginfod-enable-setting)
  (gdb-force-mode-line-update
   (propertize "initializing..." 'face font-lock-variable-name-face)))

(defun gudap-shutdown ()
  (interactive)
  (kill-buffer (process-buffer (gudap-dap-process gudap-active-server)))
  (kill-buffer (gudap-gud-buffer gudap-active-server))
  (kill-buffer (gudap-events-buffer gudap-active-server))
  (setq gudap-active-server nil))

(defun gudap--connect (server-command launch-config gud-buffer)
  (let* ((nickname server-command)
	 (readable-name (format "GUDAP (%s)" nickname))
	 (dap-proc  (make-process :name            readable-name
				  :buffer          (get-buffer-create (format " *%s events*" readable-name))
				  :command         (list server-command)
				  :filter          'gudap--process-filter
				  :noquery         t
				  :connection-type 'pipe
				  :coding          'utf-8-emacs-unix))
	 (server (make-instance 'gudap-server
				:name        readable-name
				:dap-process dap-proc
				:gud-buffer  gud-buffer
				:events-buffer (get-buffer-create (format "*%s log*" readable-name))
				:launch-config launch-config)))
    (process-put dap-proc 'gudap-server server)
    (setq gudap-active-server server)
    (gudap-send-request server 'initialize (gudap--initialize-arguments))))

(defun gudap--guess-contact ()
  (let* ((guessed-mode (if buffer-file-name major-mode))
	 (program-and-launch (gudap--lookup-program-and-launch guessed-mode))
	 (server-program (cadr program-and-launch))
	 (launch-config-uneval (caddr program-and-launch))
	 (launch-config (or (and (functionp launch-config-uneval)
				 (funcall launch-config-uneval))
			    (and (listp launch-config-uneval)
				 launch-config-uneval))))
    (cons server-program launch-config)))

(defun gudap--lookup-program-and-launch (mode-name)
  (cl-loop for p-l in gudap-server-programs-and-launch
	   if (or
	       (and (listp (car p-l)) (member mode-name (car p-l)))
	       (and (symbolp (car p-l)) (equal mode-name (car p-l))))
	   return p-l))

;;; REQUESTS

(cl-defgeneric gudap-send-request (server command arguments))

(cl-defmethod gudap-send-request (server command arguments &optional callback-fn)
  (if callback-fn
      (puthash (gudap-seq server) callback-fn (gudap-callbacks server)))
  (gudap--connection-send
   server
   (list :type "request"
	 :command (symbol-name command)
	 :arguments arguments)))

;;; RESPONSES

(cl-defgeneric gudap-handle-response (server command success body err-msg)
  "Handler for DAP response.")

(cl-defmethod gudap-handle-response (server (_type (eql initialize)) _success _body _err-msg)
  (gudap-send-request server 'launch (gudap-launch-config server)))

(cl-defmethod gudap-handle-response (server (type (eql evaluate)) (success (eql t)) body _err-msg)
  (gudap-gud-output server (plist-get body :result)))

(cl-defmethod gudap-handle-response (server (type (eql evaluate)) _success _body err-msg)
  (gudap-gud-output server err-msg))

(cl-defmethod gudap-handle-response (server (type (eql continue)) (success (eql t)) _body err-msg)
  (setf (gudap-stopped server) nil))

(cl-defmethod gudap-handle-response (server command success body err-msg)
  (gudap--message "unknown response: %s" body))

;;; EVENTS

(cl-defgeneric gudap-handle-event (server event body))

(cl-defmethod gudap-handle-event (server event body)
  (gudap--message "%s" body))

(cl-defmethod gudap-handle-event (server (event (eql output)) body)
  (let ((gud-process (get-buffer-process (gudap-gud-buffer server))))
    (cl-destructuring-bind
	(&key category output group variablesReference source line column data)
	body
      (gudap-gud-output server output))))

(cl-defmethod gudap-handle-event (server (event (eql initialized)) body)
  (setf (gudap-initialized server) t))

(cl-defmethod gudap-handle-event (server (event (eql terminated)) body)
  (setf (gudap-initialized server) nil
	(gudap-stopped server) t))

(defun gudap-gud-goto-frame-from-stack-trace (server success stack-trace-body)
  (when success
    (let* ((stack-frames (plist-get stack-trace-body :stackFrames))
	   (top-frame (aref stack-frames 0))
	   (path (plist-get (plist-get top-frame :source) :path))
	   (line (plist-get top-frame :line)))
      (setf (gudap-current-frame server) (plist-get top-frame :id))
      (setq gud-last-frame (cons path line)))
    (gud-display-frame)))

(cl-defmethod gudap-handle-event (server (event (eql stopped)) body)
  (cl-destructuring-bind (&key reason threadId &allow-other-keys) body
    (cond
     ( ;; breakpoint
      (or (equal reason "breakpoint") (equal reason "step"))
      (setf (gudap-current-thread server) threadId
	    (gudap-stopped server) t)
      (gudap-send-request server
			  'stackTrace
			  (list :threadId threadId
				:levels 1)
			  'gudap-gud-goto-frame-from-stack-trace)))))

;;; REVERSE REQUESTS

(cl-defgeneric gudap-handle-request (server command arguments))

;;; UTILS

(cl-defgeneric gudap-print-received (server received))

(cl-defmethod gudap-print-received (server received)
  (with-current-buffer (gudap-events-buffer server)
    (goto-char (point-max))
    (insert (format "received:\n%s\n\n" (pp-to-string received)))))

(cl-defgeneric gudap-print-sent (server sent))

(cl-defmethod gudap-print-sent (server sent)
  (with-current-buffer (gudap-events-buffer server)
    (goto-char (point-max))
    (insert (format "sent:\n%s\n\n" (pp-to-string sent)))))



;;; GUD

(cl-defmethod gudap-gud-output (server output)
  (let* ((gud-buffer (gudap-gud-buffer server))
	 (gud-process (get-buffer-process gud-buffer)))
    (gud-filter gud-process (format "\n%s\n> " output))))

(cl-defmethod gudap-gud-prompt (server)
  (let ((gud-process (get-buffer-process (gudap-gud-buffer server))))
    (gud-filter gud-process "> ")))

(cl-defmethod gudap-gud-input (server input)
  (let ((gud-process (get-buffer-process (gudap-gud-buffer server))))
    (gudap-send-request server
			'evaluate
			(list :expression input
			      :frameId (gudap-current-frame server)
			      :context "repl"))))

(defun gudap--initialize-arguments ()
  (list :clientId "gudap"
	:clientName "gudap"
	:adapterId "gudap"
	:locale "en-US"
	:linesStartAt1 t
	:columnsStartAt1 t
	:pathFormat "path"
	:supportsVariableType nil
	:supportsVariablePaging nil
	:supportsRunInTerminalRequest nil
	:supportsMemoryReferences nil
	:supportsProgressReporting nil
	:supportsInvalidatedEvent nil
	:supportsMemoryEvent nil
	:supportsArgsCanBeInterpretedByShell nil
	:supportsStartDebuggingRequest nil))

(defun gudap--path (source)
  (plist-get source :path))

;; Utils

(defun gudap--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[gudap] %s" (apply #'format format args)))

;; JSONRPC-ish handling

(defvar gudap--in-process-filter nil
  "Non-nil if inside `dap--process-filter'.")

;; adapted from jsonrpc.el
(cl-defun gudap--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when gudap--in-process-filter
    ;; Problematic recursive process filters may happen if
    ;; `dap--connection-receive', called by us, eventually calls
    ;; client code which calls `process-send-string' (which see) to,
    ;; say send a follow-up message.  If that happens to writes enough
    ;; bytes for pending output to be received, we will lose JSONRPC
    ;; messages.  In that case, remove recursiveness by re-scheduling
    ;; ourselves to run from within a timer as soon as possible
    ;; (bug#60088)
    (run-at-time 0 nil #'gudap--process-filter proc string)
    (cl-return-from gudap--process-filter))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((gudap--in-process-filter t)
	     (server (process-get proc 'gudap-server))
             (expected-bytes (gudap-expected-bytes server)))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (let ((inhibit-read-only t)) (insert string))
          (set-marker (process-mark proc) (point)))
        ;; Loop (more than one message might have arrived)
        ;;
        (unwind-protect
            (let (done)
              (while (not done)
                (cond
                 ((not expected-bytes)
                  ;; Starting a new message
                  ;;
                  (setq expected-bytes
                        (and (search-forward-regexp
                              "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
			      nil
                              t)
                             (string-to-number (match-string 1))))
                  (unless expected-bytes
                    (setq done :waiting-for-new-message)))
                 (t
                  ;; Attempt to complete a message body
                  ;;
                  (let ((available-bytes (- (position-bytes (process-mark proc))
                                            (position-bytes (point)))))
                    (cond
                     ((>= available-bytes
                          expected-bytes)
                      (let* ((message-end (byte-to-position
                                           (+ (position-bytes (point))
                                              expected-bytes))))
                        (unwind-protect
                            (save-restriction
                              (narrow-to-region (point) message-end)
                              (let* ((json-message
                                      (condition-case-unless-debug oops
                                          (jsonrpc--json-read)
                                        (error
                                         (jsonrpc--warn "Invalid JSON: %s %s"
                                                        (cdr oops) (buffer-string))
                                         nil))))
                                (when json-message
                                  ;; Process content in another
                                  ;; buffer, shielding proc buffer from
                                  ;; tamper
                                  (with-temp-buffer
                                    (gudap--connection-receive server
                                                               json-message)))))
                          (goto-char message-end)
			  (let ((inhibit-read-only t))
                            (delete-region (point-min) (point)))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter
          ;;
          (setf (gudap-expected-bytes server) expected-bytes))))))

(defun gudap--connection-receive (conn message)
  (gudap-print-received conn message)
  (cl-destructuring-bind (&key seq request_seq type command event body arguments success message &allow-other-keys) message
    (cond
     (;; callback present
      (gethash request_seq (gudap-callbacks conn))
      (funcall (gethash request_seq (gudap-callbacks conn)) conn success body)
      (remhash request_seq (gudap-callbacks conn)))
     (;; event
      (string-equal type "event")
      (gudap-handle-event conn (intern event) body))
     (;; response
      (string-equal type "response")
      (gudap-handle-response conn (intern command) success body message))
     (;; reverse request
      (string-equal type "request")
      (gudap-handle-request conn (intern command) arguments)))))

(defun gudap--connection-send (conn message)
  (setq message (plist-put message :seq (gudap-seq conn)))
  (gudap-print-sent conn message)
  (let* ((json-object-type 'plist)
	 (json (jsonrpc--json-encode message))
	 (headers `(("Content-Length" . ,(format "%d" (string-bytes json)))))
	 (content (cl-loop for (header . value) in headers
			   concat (concat header ": " value "\r\n") into header-section
			   finally return (format "%s\r\n%s" header-section json))))
    (process-send-string (gudap-dap-process conn) content))
  (setf (gudap-seq conn) (1+ (gudap-seq conn))))

;;;###autoload
(provide 'gudap)

;;; gudap.el ends here
