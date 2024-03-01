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
   (expected-bytes
    :accessor gudap-expected-bytes)
   (gud-buffer
    :initarg :gud-buffer
    :accessor gudap-gud-buffer)
   (next-seq
    :initform 1
    :accessor gudap-next-seq)
   (initialized
    :initform nil
    :accessor gudap-initialized)
   (breakpoints ;; file path to list of plists (:line lineNum)
    :initform (make-hash-table :test 'equal)
    :accessor gudap-breakpoints)
   (capabilities
    :accessor gudap-capabilities)))

(defvar gudap-server-programs-and-launch
  '(((elixir-mode elixir-ts-mode) . ("elixir-ls-debug"))
    (c++-mode . "lldb-vscode")))

(defvar gudap-active-server nil)

(defun gudap--buffer-line ()
  (save-restriction
    (widen)
    (+ (count-lines (point-min) (point))
       (if (bolp) 1 0))))

(defun gudap--gud-break ()
  (interactive)
  (let* ((server gudap-active-server)
	 (file-path (buffer-file-name))
	 (old-breakpoints (gethash file-path (gudap-breakpoints server)))
	 (new-breakpoints (cons (list :line (gudap--buffer-line)) old-breakpoints)))
    (gudap-send-request
     server
     'setBreakpoints
     (list :source (gudap--dap-type-source file-path)
	   :breakpoints (vconcat new-breakpoints)
	   :lines (vconcat (mapcar (lambda (bp) (plist-get bp :line)) new-breakpoints))))))

(defun gudap--dap-type-sourcebreak (line)
  (list :line line))

(defun gudap--dap-type-source (&optional path)
  (let ((path (or path
		  (buffer-file-name))))
    (list :path path)))

(defun gudap (server-command)
  (gudap--connect server-command)
  (defalias 'gud-break 'gudap--gud-break))

(defun gudap--connect (server-command)
  (let* ((nickname server-command)
	 (readable-name (format "GUDAP (%s)" nickname))
	 (gud-buffer (gud-common-init "cat"
				      (lambda (_file args) args)
				      #'identity))
	 (dap-proc  (make-process :name            readable-name
				  :buffer          (generate-new-buffer readable-name)
				  :command         (list server-command)
				  :filter          'gudap--process-filter
				  :noquery         t
				  :connection-type 'pipe
				  :coding          'utf-8-emacs-unix))
	 (server (make-instance 'gudap-server
			      :name        readable-name
			      :dap-process dap-proc
			      :gud-buffer  gud-buffer)))
    (process-put dap-proc 'gudap-server server)
    (setq gudap-active-server server)
    (gudap-send-request server 'initialize (gudap--initialize-arguments))))

(cl-defgeneric gudap-send-request (server command arguments))

(cl-defmethod gudap-send-request (server command arguments &optional extra)
  (gudap--connection-send
   server
   (list :type "request"
	 :command (symbol-name command)
	 :arguments arguments)))


(cl-defgeneric gudap-handle-response (server command success body)
  "Handler for DAP response.")

(cl-defmethod gudap-handle-response (_server (_type (eql initialize)) _success _body)
  (gudap--message "initialized"))

(cl-defmethod gudap-handle-response (server (type (eql setBreakpoints)) (success (eql t)) body))

(cl-defmethod gudap-handle-response (server command success body)
  (gudap--message "unknown response: %s" body))

(cl-defgeneric gudap-handle-event (server event body))

(cl-defmethod gudap-handle-event (server event body)
  (gudap--message "%s" body))

(cl-defmethod gudap-handle-event (server (event (eql output)) body)
  (let ((gud-process (get-buffer-process (gudap-gud-buffer server))))
    (cl-destructuring-bind
	(&key category output group variablesReference source line column data)
	body
      (pcase category
	("console" (gud-filter gud-process output))))))

(cl-defmethod gudap-handle-event (server (event (eql initialized)) body)
  (setf (gudap-initialized server) t))

(cl-defmethod gudap-handle-event (server (event (eql stopped)) body))

(cl-defgeneric gudap-handle-request (server command arguments))

(cl-defgeneric gudap-gud-output (server output))

(cl-defmethod gudap-gud-output (server output)
  (let ((gud-process (get-buffer-process (gudap-gud-buffer server))))
    (gud-filter gud-process (format "%s\n" output))))

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

;; taken from jsonrpc.el
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
  (gudap--message "received message: %s" message)
  (cl-destructuring-bind (&key seq request_seq type command event body arguments success &allow-other-keys) message
    (cond
     (;; event
      (string-equal type "event")
      (gudap-handle-event conn (intern event) body))
     (;; response
      (string-equal type "response")
      (gudap-handle-response conn (intern command) success body))
     (;; reverse request
      (string-equal type "request")
      (gudap-handle-request conn (intern command) arguments)))))

(defun gudap--connection-send (conn message)
  (setq message (plist-put message :seq (gudap-next-seq conn)))
  (gudap--message "sent message: %s" message)
  (let* ((json-object-type 'plist)
	 (json (jsonrpc--json-encode message))
	 (headers `(("Content-Length" . ,(format "%d" (string-bytes json)))))
	 (content (cl-loop for (header . value) in headers
			   concat (concat header ": " value "\r\n") into header-section
			   finally return (format "%s\r\n%s" header-section json))))
    (process-send-string (gudap-dap-process conn) content))
  (setf (gudap-next-seq conn) (1+ (gudap-next-seq conn))))

;;;###autoload
(provide 'gudap)

;;; gudap.el ends here
