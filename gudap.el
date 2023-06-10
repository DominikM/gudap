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

(defconst dap-buffer-size 200)

(defclass dap-connection ()
  ((name
    :initarg :name)
   (process
    :initarg :process
    :accessor process)
   (event-dispatcher
    :initform #'ignore
    :initarg :event-dispatcher)
   (request-dispatcher
    :initform #'ignore
    :initarg :request-dispatcher)
   (response-dispatcher
    :initform #'ignore
    :initarg :response-dispatcher)
   (buffer
    :initform '()
    :accessor buffer)
   (expected-seq
    :initform 1
    :accessor expected-seq)
   (expected-bytes
    :accessor expected-bytes)))

(cl-defgeneric connection-live-p (connection))

(cl-defmethod connection-live-p ((connection dap-connection))
  (process-live-p (process connection)))

(defvar gudap-server-programs '(((elixir-mode elixir-ts-mode) . ("elixir-ls-debug"))))

(defvar gudap--connections-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of dap connecections.")

(defvar gudap--cached-connection nil)

(defun gudap-reset ()
  (interactive)
  (clrhash gudap--connections-by-project)
  (setq gudap--cached-connection nil))

(defun gudap (managed-major-modes project _class contact language-id &optional interactive)
  (interactive (let ((eglot-server-programs gudap-server-programs))
		 (eglot--guess-contact t)))
  (let* ((current-conn (gudap-current-connection))
	 (live-p (and current-conn (connection-live-p current-conn))))
    (if (and live-p
	     interactive
	     (y-or-n-p "[gudap] Live process found, reconnect instead? "))
	(gudap-reconnect current-conn interactive)
      (when live-p (ignore-errors (gudap-shutdown current-conn)))
      (gudap--connect managed-major-modes project 'dap-connection contact language-id))))

(defun gudap--connect (managed-modes project class contact language-id)
  (let* ((nickname (project-name project))
	 (readable-name (format "GUDAP (%s/%s)" nickname managed-modes))
	 (process (make-process :name     readable-name
				:buffer   readable-name
				:command  contact
				:filter   'dap--process-filter
				:sentinel 'dap--server-sentinel))
	 (server (make-instance class
				:name readable-name
				:process process
				:event-dispatcher 'gudap-event-dispatcher)))
    (process-put process 'dap-connection server)
    (puthash project
	     (cons server
		   (gethash project gudap--connections-by-project '()))
	     gudap--connections-by-project)))

(defun gudap-event-dispatcher (conn message)
  (pp message))

(defun gudap-current-connection ()
  "Return logical Gudap server for current buffer, nil if none."
  (setq gudap--cached-connection
        (or gudap--cached-connection
            (gethash (eglot--current-project) gudap--connections-by-project))))

(defun dap--server-sentinel (proc event)
  (message "gudap sentinel event: %s" event)
  (let ((server (process-get proc 'dap-connection)))
    (puthash (eglot--current-project)
	     (remove server (gethash (eglot--current-project) gudap--connections-by-project))
	     gudap--connections-by-project))
  (setq gudap--cached-connection nil))

(defvar dap--in-process-filter nil
  "Non-nil if inside `dap--process-filter'.")

(cl-defun dap--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when dap--in-process-filter
    ;; Problematic recursive process filters may happen if
    ;; `dap--connection-receive', called by us, eventually calls
    ;; client code which calls `process-send-string' (which see) to,
    ;; say send a follow-up message.  If that happens to writes enough
    ;; bytes for pending output to be received, we will lose JSONRPC
    ;; messages.  In that case, remove recursiveness by re-scheduling
    ;; ourselves to run from within a timer as soon as possible
    ;; (bug#60088)
    (run-at-time 0 nil #'dap--process-filter proc string)
    (cl-return-from dap--process-filter))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((inhibit-read-only t)
             (dap--in-process-filter t)
	     (connection (process-get proc 'dap-connection))
             (expected-bytes (expected-bytes connection)))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
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
                              (+ (point) 100)
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
                                    (dap-connection-receive connection
                                                            json-message)))))
                          (goto-char message-end)
                          (delete-region (point-min) (point))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter
          ;;
          (setf (expected-bytes connection) expected-bytes))))))

(defun dap-connection-receive (conn message)
  (if (dap-buffer-full (buffer conn))
      (dap-connection-process-messages conn t))
  (setf (buffer conn) (dap-buffer-insert (buffer conn) message))
  (dap-connection-process-messages conn))

(defun dap-buffer-insert (buffer message)
  (if (dap-buffer-full-p buffer)
      (error "Buffer full."))
  (let ((seq (plist-get :seq message))
	(next-seq (plist-get :seq (cadr buffer))))
    (cond
     (
      (not next-seq)
      (list message))
     (
      (> seq next-seq)
      (cons (car buffer) (dap-message-buffer-insert (cdr buffer) message)))
     (
      t
      (cons message buffer)))))

(defun dap-buffer-full-p (buffer)
  (length= buffer dap-buffer-size))

(defun dap-connection-process-messages (conn &optional all)
  (let ((next-message (car (buffer conn))))
    (while (and next-message
		(or all
		    (= (plist-get next-message :seq) (expected-seq conn))))
      (dap-process-message conn (pop (buffer conn)))
      (setq next-message (car (buffer conn))))))

(defun dap-process-message (conn message)
  (cl-destructuring-bind (&key seq type &allow-other-keys)
      message
    (setf (expected-seq conn) (1+ seq))
    (cond
     (;; event
      (equal type "event")
      (funcall (slot-value conn 'event-dispatcher) conn message))
     (;; request
      (equal type "request")
      (funcall (slot-value conn 'request-dispatcher) conn message))
     (;; response
      (equal type "response")
      (funcall (slot-value conn 'response-dispatcher) conn message)))))	   

;;;###autoload
(provide 'gudap)

;;; gudap.el ends here
