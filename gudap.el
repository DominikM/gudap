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

(require 'eglot)
(require 'jsonrpc)

(defgroup gudap nil
  "Interaction with Debug Adapter Protocol servers."
  :prefix "gudap-"
  :group 'applications)

(defclass gudap-connection ()
  ((name
    :initarg :name)
   (-process
    :initarg :process
    :accessor gudap--process)
   (-expected-bytes
    :accessor gudap--expected-bytes)
   (-sent-requests
    :initform (make-hash-table)
    :accessor gudap--sent-requests) ;; seq to request command
   (next-seq
    :initform 0
    :accessor gudap--next-seq)))

(defun gudap--test ()
  (let* ((dap-process (make-process :name "lldb-vscode"
				    :buffer (generate-new-buffer "lldb-vscode")
				    :command '("lldb-vscode")
				    :connection-type 'pipe
				    :filter 'gudap--process-filter
				    :coding 'utf-8-emacs-unix))
	 (conn (make-instance gudap-connection
			      :name "lldb-vscode-conn"
			      :process dap-process)))
    (process-put dap-process 'gudap-connection conn)
    (setq gudap--current-connection conn)
    (gudap-send-request conn 'initialize nil)))

(defun gudap--cleanup ()
  (kill-process ((gudap--process gudap--current-connection)))
  (setq gudap--current-connection nil))
    
(cl-defgeneric gudap-send-request (server command arguments))

(cl-defmethod gudap-send-request (server (command (eql initialize)) arguments)
  (puthash (gudap--next-seq server) 'initialize (gudap--sent-requests server))
  (gudap--connection-send
   server
   (list :type "request"
	 :command "initialize"
	 :arguments (gudap--initialize-arguments))))
	   

(cl-defgeneric gudap-handle-response (server type body))

(cl-defmethod gudap-handle-response (server type body)
  (gudap--message "%s" body))

(cl-defmethod gudap-handle-response (server (type (eql initialize)) body)
  (gudap--message "initialized: %s" body))

(cl-defgeneric gudap-handle-event (server event body))

(cl-defmethod gudap-handle-event (server event body)
  (gudap--message "%s" message))

(cl-defgeneric gudap-handle-request (server command arguments))

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

;; Utils    

(defun gudap--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[gudap] %s" (apply #'format format args)))


;; JSONRPC-ish handling

(defvar gudap--in-process-filter nil
  "Non-nil if inside `dap--process-filter'.")

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
	     (connection (process-get proc 'gudap-connection))
             (expected-bytes (gudap--expected-bytes connection)))
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
                              (+ (point) 200)
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
                                    (gudap--connection-receive connection
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
          (setf (gudap--expected-bytes connection) expected-bytes))))))

(defun gudap--connection-receive (conn message)
  (cl-destructuring-bind (&key seq type event body arguments &allow-other-keys) message
    (cond
     (;; event
      (string-equal type "event")
      (gudap-handle-event conn (intern event) body))
     (;; response
      (string-equal type "response")
      (let ((request-type (gethash seq (gudap--sent-requests conn))))
	(if request-type
	    (gudap-handle-response conn request-type body)
	  (gudap--message "received response to a nonexistent request. ignoring")))
      (remhash seq (gudap--sent-requests conn)))
     (;; reverse request
      (string-equal type "request")
      (gudap-handle-request conn (intern command) arguments)))))

(defun gudap--connection-send (conn message)
  (setq message (plist-put message :seq (gudap--next-seq conn)))
  (let* ((json-object-type 'plist)
	 (json (jsonrpc--json-encode message))
	 (headers `(("Content-Length" . ,(format "%d" (string-bytes json)))))
	 (content (cl-loop for (header . value) in headers
			   concat (concat header ": " value "\r\n") into header-section
			   finally return (format "%s\r\n%s" header-section json))))
    (process-send-string (gudap--process conn) content))
  (setf (gudap--next-seq conn) (1+ (gudap--next-seq conn))))

;; (defvar gudap-server-programs
;;   '(((elixir-mode elixir-ts-mode) . ("elixir-ls-debug"))))

;; (defvar gudap-launch-args
;;   `(((elixir-mode elixir-ts-mode) . (("phx" . ,(lambda (project)
;; 						 (list :type "mix_task"
;; 						       :name "phx.server"
;; 						       :request "launch"
;; 						       :task "phx.server"
;; 						       :projectDir (file-truename (project-root project)))))))))

;; (defvar gudap--connections-by-project (make-hash-table :test #'equal)
;;   "Keys are projects.  Values are dap connecections.")

;; (defvar gudap--cached-connection nil)

;; (defun gudap-reset ()
;;   (interactive)
;;   (clrhash gudap--connections-by-project)
;;   (setq gudap--cached-connection nil))

;; (defun gudap (managed-major-modes project _class contact language-id &optional interactive)
;;   (interactive (gudap--guess-contact))
;;   (let* ((current-conn (gudap-current-connection))
;; 	 (live-p (and current-conn (connection-live-p current-conn))))
;;     (if (and live-p
;; 	     interactive
;; 	     (y-or-n-p "[gudap] Live process found, reconnect instead? "))
;; 	(gudap-reconnect current-conn interactive)
;;       (when live-p (ignore-errors (gudap-shutdown current-conn)))
;;       (gudap--connect managed-major-modes project 'dap-connection contact language-id))))

;; (defun gudap--guess-contact ()
;;   (let ((eglot-server-programs gudap-server-programs))
;;     (eglot--guess-contact t)))

;; (defun gudap--lookup-launch-args (mode)
;;   (let ((eglot-server-programs gudap-launch-args))
;;     (eglot--lookup-mode mode)))

;; (defun gudap-shutdown ()
;;   (interactive)
;;   (let* ((conn (gudap-current-connection)))
;;     (if (dap-process conn)
;; 	(kill-buffer (process-buffer (dap-process conn))))
;;     (if (comint-process conn)
;; 	(kill-buffer (process-buffer (comint-process conn))))
;;     (remhash (eglot--current-project) gudap--connections-by-project)
;;     (setq gudap--cached-connection nil)))

;; (defun gudap--connect (managed-modes project class contact language-id)
;;   (let* ((nickname (project-name project))
;; 	 (readable-name (format "GUDAP (%s/%s)" nickname managed-modes))
;; 	 (dap-proc (if (integerp (cadr contact))
;; 		       (open-network-stream
;; 			readable-name
;; 			(generate-new-buffer readable-name)
;; 			(car contact)
;; 			(cadr contact)
;; 			:sentinel 'dap--server-sentinel
;; 			:noquery t)
;; 		     (make-process :name     readable-name
;; 				   :buffer   (generate-new-buffer readable-name)
;; 				   :command  contact
;; 				   :sentinel 'dap--server-sentinel
;; 				   :noquery  t
;; 				   :connection-type 'pipe
;; 				   :coding 'utf-8-emacs-unix)))
;; 	 (comint-name (format "%s SHELL" readable-name))
;; 	 (comint-proc (make-process :name    comint-name
;; 				    :buffer  (generate-new-buffer comint-name)
;; 				    :command nil
;; 				    :noquery t))
;; 	 (conn (make-instance class
;; 			      :name readable-name
;; 			      :dap-process dap-proc
;; 			      :event-dispatcher 'gudap-event-dispatcher
;; 			      :response-dispatcher 'gudap-response-dispatcher
;; 			      :comint-process comint-proc
;; 			      :launch-args (alist-get managed-modes gudap-launch-args nil nil #'equal))))
;;     (set-process-filter dap-proc 'dap--process-filter)
;;     (setq gudap--cached-connection conn)
;;     (process-put dap-proc 'dap-connection conn)
;;     (process-put comint-proc 'dap-connection conn)
;;     (puthash project conn gudap--connections-by-project)
;;     (gudap--init-comint conn)
;;     (dap--send-initialize conn)))

;; (defun gudap--init-comint (conn)
;;   (let ((buffer (process-buffer (comint-process conn))))
;;     (with-current-buffer buffer
;;       (comint-mode)
;;       (setq-local comint-input-sender #'gudap-comint-receive)
;;       (setq-local comint-prompt-regexp "^> ")
;;       (setq-local comint-use-prompt-regexp t))))

;; (defun gudap-comint-receive (proc message)
;;   (message message))

;; (defun gudap-comint-send (conn message)
;;   (comint-output-filter
;;    (comint-process conn)
;;    (format "%s\n" message)))

;; (defun gudap-event-dispatcher (conn message)
;;   (cl-destructuring-bind (&key event body &allow-other-keys)
;;       message
;;     (cond
;;      (
;;       (equal event "output")
;;       (gudap-comint-send conn (plist-get body :output)))
;;      (
;;       (equal event "initialized")
;;       (dap--send-all-breakpoints conn)
;;       (dap--send-config-done conn)))))

;; (defun gudap-response-dispatcher (conn message)
;;   (cl-destructuring-bind (&key command request_seq body &allow-other-keys)
;;       message
;;     (cond
;;      (
;;       (equal command "setBreakpoints")
;;       (gudap--verify-breakpoints conn message)))))

;; (defun gudap--verify-breakpoints (conn message)
;;   (cl-destructuring-bind (&key request_seq body &allow-other-keys)
;;       message
;;     (let* ((bp-results-vector (plist-get body :breakpoints))
;; 	   (bp-results (append bp-results-vector nil)) ;; convert to list
;; 	   (bp-path (gethash request_seq (breakpoints-req-seq conn)))
;; 	   (pending-bps (gethash bp-path (breakpoints conn)))
;; 	   (valid-bps (gudap--validate-breakpoints pending-bps bp-results)))
;;       (puthash bp-path valid-bps (breakpoints conn))
;;       (remhash request_seq (breakpoints-req-seq conn))
;;       (message "breakpoints validated"))))

;; (defun gudap--validate-breakpoints (bps bp-results)
;;   (if (/= (length bps) (length bp-results))
;;       (error "Breakpoints and breakpoints results must be the same length."))
;;   (let ((bp (car bps))
;; 	(bp-result (car bp-results)))
;;     (cond
;;      (
;;       (null bp)
;;       '())
;;      (
;;       (plist-get bp-result :verified)
;;       (cons bp (gudap--validate-breakpoints (cdr bps) (cdr bp-results))))
;;      (
;;       (not (plist-get bp-result :verified))
;;       (gudap--validate-breakpoints (cdr bps) (cdr bp-results))))))

;; (defun dap--send-breakpoints (conn path breakpoints)
;;   (puthash (next-seq conn) path (breakpoints-req-seq conn))
;;   (dap-connection-send
;;    conn
;;    (list :seq (next-seq conn)
;; 	 :type "request"
;; 	 :command "setBreakpoints"
;; 	 :arguments
;; 	 (list :source `(:path ,path)
;; 	       :breakpoints (vconcat breakpoints)))))

;; (defun dap--send-all-breakpoints (conn)
;;   (maphash
;;    (lambda (path bps)
;;      (dap--send-breakpoints conn path bps))
;;    (breakpoints conn)))

;; (defun gudap-current-connection ()
;;   "Return logical Gudap server for current buffer, nil if none."
;;   (setq gudap--cached-connection
;;         (or gudap--cached-connection
;;             (gethash (eglot--current-project) gudap--connections-by-project))))

;; (defun gudap-launch (conn)
;;   (interactive (list (gudap-current-connection)))
;;   (let* ((launch-keys (mapcar (lambda (elem) (car elem)) (launch-args conn)))
;; 	 (launch-key (read-from-minibuffer "What launch config to use? "
;; 					   launch-keys))
;; 	 (launch-args (funcall (alist-get launch-key (launch-args conn) nil nil #'equal) (project-current))))
;;     (dap--send-launch conn launch-args)))

;; (defun dap--send-initialize (conn)
;;   (let ((message (list :seq (next-seq conn)
;; 		       :type "request"
;; 		       :command "initialize"
;; 		       :arguments (list :adapterID "dummy"
;; 					:supportsProgressReporting t))))
;;     (dap-connection-send conn message)))

;; (defun dap--send-launch (conn launch-args)
;;   (let ((message (list :seq (next-seq conn)
;; 		       :type "request"
;; 		       :command "launch"
;; 		       :arguments launch-args)))
;;     (dap-connection-send conn message)))

;; (defun dap--send-config-done (conn)
;;   (let ((message (list :seq (next-seq conn)
;; 		       :type "request"
;; 		       :command "configurationDone")))
;;     (dap-connection-send conn message)))

;; (defun dap--server-sentinel (proc event)
;;   (let ((conn (process-get proc 'dap-connection)))
;;     (remhash (eglot--current-project) gudap--connections-by-project))
;;   (setq gudap--cached-connection nil))

;; (defun dap-connection-send (conn message)
;;   ;; (message "\nSending message:\n")
;;   ;; (pp message)
;;   ;; (message "\n")
;;   (let* ((json-object-type 'plist)
;; 	 (json (jsonrpc--json-encode message))
;; 	 (headers `(("Content-Length" . ,(format "%d" (string-bytes json)))))
;; 	 (content (cl-loop for (header . value) in headers
;; 			   concat (concat header ": " value "\r\n") into header-section
;; 			   finally return (format "%s\r\n%s" header-section json))))
;;     (process-send-string (dap-process conn) content))
;;   (setf (next-seq conn) (1+ (next-seq conn))))


;;;###autoload
(provide 'gudap)

;;; gudap.el ends here
