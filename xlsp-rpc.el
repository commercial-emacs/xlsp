;;; xlsp-rpc.el --- JSON-RPC library                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: processes, languages, extensions
;; Version: 1.0.20
;; Package-Requires: ((emacs "25.2"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements the JSONRPC 2.0 specification as described
;; in https://www.jsonrpc.org/.  As the name suggests, JSONRPC is a
;; generic Remote Procedure Call protocol designed around JSON
;; objects.  To learn how to write JSONRPC programs with this library,
;; see Info node `(elisp)JSONRPC'."
;;
;; This library was originally extracted from eglot.el, an Emacs LSP
;; client, which you should see for an example usage.
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)
(eval-when-compile (require 'subr-x))
(require 'warnings)
(require 'pcase)

;;; Public API
;;;

(declare-function xlsp--test-p "xlsp")

(defclass xlsp-rpc-connection ()
  ((name
    :accessor xlsp-rpc-name
    :initarg :name
    :documentation "A name for the connection")
   (-request-dispatcher
    :accessor xlsp-rpc--request-dispatcher
    :initform #'ignore
    :initarg :request-dispatcher
    :documentation "Dispatcher for remotely invoked requests.")
   (-notification-dispatcher
    :accessor xlsp-rpc--notification-dispatcher
    :initform #'ignore
    :initarg :notification-dispatcher
    :documentation "Dispatcher for remotely invoked notifications.")
   (last-error
    :initform nil
    :accessor xlsp-rpc-last-error
    :documentation "Last JSONRPC error message received from endpoint.")
   (-request-continuations
    :initform (make-hash-table)
    :accessor xlsp-rpc--request-continuations
    :documentation "A hash table of request ID to continuation lambdas.")
   (-events-buffer
    :initform nil
    :accessor xlsp-rpc--events-buffer
    :documentation "A buffer pretty-printing the JSONRPC events")
   (-events-buffer-scrollback-size
    :initarg :events-buffer-scrollback-size
    :accessor xlsp-rpc--events-buffer-scrollback-size
    :documentation "Max size of events buffer.  0 disables, nil means infinite.")
   (-deferred-actions
    :initform (make-hash-table :test #'equal)
    :accessor xlsp-rpc--deferred-actions
    :documentation "Map (DEFERRED BUF) to (FN TIMER ID).
FN is a deferred request from BUF, to be sent not later than TIMER as ID.")
   (-next-request-id
    :initform 0
    :accessor xlsp-rpc--next-request-id
    :documentation "Next number used for a request"))
  :documentation "Base class representing a JSONRPC connection.
The following initargs are accepted:

:NAME (mandatory), a string naming the connection

:REQUEST-DISPATCHER (optional), a function of three
arguments (CONN METHOD PARAMS) for handling JSONRPC requests.
CONN is a `xlsp-rpc-connection' object, METHOD is a symbol, and
PARAMS is a plist representing a JSON object.  The function is
expected to return a JSONRPC result, a plist of (:result
RESULT) or signal an error of type `xlsp-rpc-error'.

:NOTIFICATION-DISPATCHER (optional), a function of three
arguments (CONN METHOD PARAMS) for handling JSONRPC
notifications.  CONN, METHOD and PARAMS are the same as in
:REQUEST-DISPATCHER.")

;;; API mandatory
(cl-defgeneric xlsp-rpc-connection-send (conn &key id method params result error)
  "Send a JSONRPC message to connection CONN.
ID, METHOD, PARAMS, RESULT and ERROR.")

;;; API optional
(cl-defgeneric xlsp-rpc-shutdown (conn)
  "Shutdown the JSONRPC connection CONN.")

;;; API optional
(cl-defgeneric xlsp-rpc-running-p (conn)
  "Tell if the JSONRPC connection CONN is still running.")

;;; API optional
(cl-defgeneric xlsp-rpc-connection-ready-p (connection what)
  "Tell if CONNECTION is ready for WHAT in current buffer.
If it isn't, a request which was passed a value to the
:deferred keyword argument will be deferred to the future.
WHAT is whatever was passed the as the value to that argument.

By default, all connections are ready for sending all requests
immediately."
  (:method (_s _what)   ;; by default all connections are ready
           t))


;;; Convenience
;;;
(cl-defmacro xlsp-rpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "xlsp-rpc-lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(defun xlsp-rpc-events-buffer (connection)
  "Get or create JSONRPC events buffer for CONNECTION."
  (let ((probe (xlsp-rpc--events-buffer connection)))
    (if (buffer-live-p probe)
        probe
      (with-current-buffer
          (get-buffer-create (format "*%s events*" (xlsp-rpc-name connection)))
        (special-mode)
        (setf (xlsp-rpc--events-buffer connection)
              (current-buffer))))))

(defun xlsp-rpc-forget-pending-continuations (connection)
  "Stop waiting for responses from the current JSONRPC CONNECTION."
  (clrhash (xlsp-rpc--request-continuations connection)))

(defvar xlsp-rpc-inhibit-debug-on-error nil
  "Inhibit `debug-on-error' when answering requests.
Some extensions, notably ert.el, set `debug-on-error' to non-nil,
which makes it hard to test the behavior of catching the Elisp
error and replying to the endpoint with an JSONRPC-error.  This
variable can be set around calls like `xlsp-rpc-request' to
circumvent that.")

(defun xlsp-rpc-connection-receive (connection message)
  "Process MESSAGE just received from CONNECTION.
This function will destructure MESSAGE and call the appropriate
dispatcher in CONNECTION."
  (cl-destructuring-bind (&key method id error params result &allow-other-keys
                               &aux continuations)
      message
    (xlsp-rpc--log-event connection message 'server)
    (setf (xlsp-rpc-last-error connection) error)
    (cond
     ;; A remote request
     ((and method id)
      (apply #'xlsp-rpc--reply connection
             id (condition-case err
                    `(:result ,(funcall (xlsp-rpc--request-dispatcher connection)
                                        connection (intern method) params))
                  (error
                   (let ((code (alist-get 'xlsp-rpc-error-code (cdr err))))
                     `(:error (:code
                               ,(or code -32603)
                               :message
                               ,(or code "Internal error"))))))))
     ;; A remote notification
     (method
      (funcall (xlsp-rpc--notification-dispatcher connection)
               connection (intern method) params))
     ;; A remote response
     ((setq continuations
            (and id (gethash id (xlsp-rpc--request-continuations connection))))
      (cancel-timer (nth 2 continuations))
      (remhash id (xlsp-rpc--request-continuations connection))
      (if error
          (funcall (nth 1 continuations) error)
        (funcall (nth 0 continuations) result))))
    (xlsp-rpc--call-deferred connection)))

;;; Contacting the remote endpoint
;;;
(defun xlsp-rpc-error (&rest args)
  "Error out with FORMAT and ARGS.
If invoked inside a dispatcher function, this function is suitable
for replying to the remote endpoint with an error message.

ARGS can be of the form (FORMAT-STRING . MOREARGS) for replying
with a -32603 error code and a message formed by formatting
FORMAT-STRING with MOREARGS.

Alternatively ARGS can be plist representing a JSONRPC error
object, using the keywords :code, :message and :data."
  (if (stringp (car args))
      (let ((msg
             (apply #'format-message (car args) (cdr args))))
        (signal 'xlsp-rpc-error
                `(,msg
                  (xlsp-rpc-error-code . -32603)
                  (xlsp-rpc-error-message . ,msg))))
    (cl-destructuring-bind (&key code message data) args
      (signal 'xlsp-rpc-error
              `("[xlsp-rpc] error "
                (xlsp-rpc-error-code . ,code)
                (xlsp-rpc-error-message . ,message)
                (xlsp-rpc-error-data . ,data))))))

(cl-defun xlsp-rpc-async-request (connection method params
					     &rest args
					     &key success-fn error-fn
					     timeout-fn timeout deferred)
  "Issue request to CONNECTION, then immediately return.

The keyword arguments SUCCESS-FN, ERROR-FN and TIMEOUT-FN (the
success, error, and timeout callbacks, respectively) default to
logging events into `xlsp-rpc-events-buffer'.

If DEFERRED is non-nil, delay the request until
`xlsp-rpc-connection-ready-p' is true.  Should the function be
called again with identical buffer and DEFERRED value before the
request is issued, the request is dropped."
  (prog1 nil
    (ignore success-fn error-fn timeout-fn timeout deferred)
    (apply #'xlsp-rpc--async-request connection method params args)))

(cl-defun xlsp-rpc-request (connection method params
				       &key deferred timeout
				       cancel-on-input cancel-on-input-retval)
  "Same as `xlsp-rpc-async-request' but awaits reply.

If successful, returns a xlsp-rpc result object.  Otherwise
signals `xlsp-rpc-error'.

If CANCEL-ON-INPUT, returns CANCEL-ON-INPUT-RETVAL immediately
upon user input.  Any subsequent replies to the request are
dropped."
  (let ((deferred-key (list deferred (current-buffer)))
        done-p error-p retval)
    (cl-destructuring-bind (id timer)
        (apply #'xlsp-rpc--async-request
               connection method params
               :success-fn
               (lambda (result)
                 (setq done-p t
                       retval result))
               :error-fn
               (xlsp-rpc-lambda (&key code message data)
				(setq error-p t
				      retval `((xlsp-rpc-error-code . ,code)
					       (xlsp-rpc-error-message . ,message)
					       (xlsp-rpc-error-data . ,data))))
               :timeout-fn
               (lambda ()
                 (setq error-p t
                       retval '((xlsp-rpc-error-message . "Timed out"))))
               `(,@(when deferred `(:deferred ,deferred))
                 ,@(when timeout `(:timeout ,timeout))))
      (unwind-protect
          (cl-loop when error-p
                   do (signal 'xlsp-rpc-error
                              (cons (format "request id=%s failed:" id)
                                    retval))
                   end
                   when done-p
                   return retval
                   if cancel-on-input
                   if (not (sit-for 0.1))
                   return cancel-on-input-retval
                   end
                   else
                   do (accept-process-output nil 0.1)
                   end)
        (remhash deferred-key (xlsp-rpc--deferred-actions connection))
        (when id (remhash id (xlsp-rpc--request-continuations connection)))
        (when timer (cancel-timer timer))))))

(cl-defun xlsp-rpc-notify (connection method params)
  "Notify CONNECTION of something, don't expect a reply."
  (xlsp-rpc-connection-send connection
                            :method method
                            :params params))

(define-obsolete-variable-alias 'jrpc-default-request-timeout
  'xlsp-rpc-default-request-timeout "28.1")

(defconst xlsp-rpc-default-request-timeout 10
  "Time in seconds before timing out a JSONRPC request.")

(defclass xlsp-rpc-process-connection (xlsp-rpc-connection)
  ((-process
    :initarg :process :accessor xlsp-rpc--process
    :documentation "Process object wrapped by the this connection.")
   (-expected-bytes
    :initform nil
    :accessor xlsp-rpc--expected-bytes
    :documentation "How many bytes declared by server.")
   (-on-shutdown
    :accessor xlsp-rpc--on-shutdown
    :initform #'ignore
    :initarg :on-shutdown
    :documentation "Function run when the process dies.")
   (-autoport-inferior
    :initform nil
    :documentation "Used by `xlsp-rpc-autoport-bootstrap'."))
  :documentation "A JSONRPC connection over an Emacs process.
The following initargs are accepted:

:PROCESS (mandatory), a live running Emacs process object or a
function producing one such object.  If a function, it is passed
the `xlsp-rpc-process-connection' object.  The process represents
either a pipe connection to locally running process or a stream
connection to a network host.  The remote endpoint is expected to
understand JSONRPC messages with basic HTTP-style enveloping
headers such as \"Content-Length:\".

:ON-SHUTDOWN (optional), a function of one argument, the
connection object, called when the process dies.

Needs to be rewritten since `initialize-instance' proceeds
to erase whatever the process wrote to its process buffer.")

(cl-defmethod initialize-instance ((conn xlsp-rpc-process-connection) _slots)
  "Compatibility for rogue eglot guy's obfuscated buffer naming.
CONN's extant process buffer, if any, is potentially orphaned and
replaced with an invisible buffer \" *[CONN name] output*\".  If
:stderr of CONN's process is named '*[CONN name] stderr*', it is
modified to replicate messages to CONN's event buffer, and made
invisible by renaming to \" *[CONN name] stderr*\".

Needs to be rewritten."
  (cl-call-next-method)
  (when (functionp (xlsp-rpc--process conn))
    (setf (xlsp-rpc--process conn) (funcall (xlsp-rpc--process conn))))
  (process-put (xlsp-rpc--process conn) 'xlsp-rpc-connection conn)
  (set-process-filter (xlsp-rpc--process conn) #'xlsp-rpc--process-filter)
  (set-process-sentinel (xlsp-rpc--process conn) #'xlsp-rpc--process-sentinel)
  (let ((stdout-buffer (get-buffer-create
                        (format " *%s output*" (xlsp-rpc-name conn)))))
    (set-process-buffer (xlsp-rpc--process conn) stdout-buffer)
    (with-current-buffer stdout-buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (set-marker (process-mark (xlsp-rpc--process conn)) (point-min))))
  (when-let ((stderr-buffer
              (get-buffer (format "*%s stderr*" (xlsp-rpc-name conn))))
             (invisible-name (concat " " (buffer-name stderr-buffer))))
    (with-current-buffer stderr-buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (process-put (xlsp-rpc--process conn) 'xlsp-rpc-stderr stderr-buffer)
    (when-let ((detritus (get-buffer invisible-name)))
      (let (kill-buffer-query-functions)
        (kill-buffer detritus)))
    (with-current-buffer stderr-buffer
      (rename-buffer invisible-name)
      (add-hook
       'after-change-functions
       (lambda (beg _end _pre-change-len)
         "Mimeograph stderr to events."
         (cl-loop initially (goto-char beg)
                  do (forward-line)
                  when (bolp)
                  for line = (buffer-substring
                              (line-beginning-position 0)
                              (line-end-position 0))
                  do (with-current-buffer (xlsp-rpc-events-buffer conn)
                       (goto-char (point-max))
                       (let ((inhibit-read-only t))
                         (insert (format "[stderr] %s\n" line))))
                  until (eobp)))
       nil t))))

(defun xlsp-rpc--stringify-method (args)
  "Normalize :method in ARGS."
  (cl-loop for i below (1- (length args)) by 2
           for keyword = (nth i args)
           while (keywordp keyword)
           do (when (eq :method keyword)
                (let* ((method* (nth (1+ i) args))
                       (method (cond ((keywordp method*)
                                      (substring (symbol-name method*) 1))
                                     ((and method* (symbolp method*))
                                      (symbol-name method*))
                                     ((stringp method*)
                                      method*))))
                  (if method
                      (setcar (nthcdr (1+ i) args) method)
                    (setf (nthcdr i args) (nthcdr (+ 2 i) args)))))
           finally return args))

(cl-defmethod xlsp-rpc-connection-send ((connection xlsp-rpc-process-connection)
					&rest args)
  (setq args (xlsp-rpc--stringify-method args))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (xlsp-rpc--json-encode message))
         (headers `(("Content-Length" . ,(format "%d" (string-bytes json))))))
    (xlsp-rpc--log-event connection message 'client)
    (process-send-string
     (xlsp-rpc--process connection)
     (cl-loop for (header . value) in headers
              concat (concat header ": " value "\r\n") into header-section
              finally return (format "%s\r\n%s" header-section json)))))

(defun xlsp-rpc-process-type (conn)
  "Return the `process-type' of JSONRPC connection CONN."
  (process-type (xlsp-rpc--process conn)))

(cl-defmethod xlsp-rpc-running-p ((conn xlsp-rpc-process-connection))
  "Return non-nil if JSONRPC connection CONN is running."
  (process-live-p (xlsp-rpc--process conn)))

(cl-defmethod xlsp-rpc-shutdown ((conn xlsp-rpc-process-connection)
                                 &optional cleanup)
  "Wait for JSONRPC connection CONN to shutdown.
With optional CLEANUP, kill any associated buffers."
  (unwind-protect
      (cl-loop
       with proc = (xlsp-rpc--process conn) for i from 0
       while (not (process-get proc 'xlsp-rpc-sentinel-cleanup-started))
       unless (zerop i) do
       (xlsp-rpc--warn "Sentinel for %s still hasn't run, deleting it!" proc)
       do
       (delete-process proc)
       (accept-process-output nil 0.1))
    (when cleanup
      (kill-buffer (process-buffer (xlsp-rpc--process conn)))
      (kill-buffer (xlsp-rpc-stderr-buffer conn)))))

(defun xlsp-rpc-stderr-buffer (conn)
  "Get CONN's standard error buffer, if any."
  (process-get (xlsp-rpc--process conn) 'xlsp-rpc-stderr))

(define-error 'xlsp-rpc-error "xlsp-rpc-error")

(defalias 'xlsp-rpc--json-read
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'plist
                           :null-object nil
                           :false-object :json-false))
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    (lambda ()
      (let ((json-object-type 'plist))
        (json-read))))
  "Read JSON object in buffer, move point to end of buffer.")

(defalias 'xlsp-rpc--json-encode
  (if (fboundp 'json-serialize)
      (lambda (object)
        (json-serialize object
                        :false-object :json-false
                        :null-object nil))
    (require 'json)
    (defvar json-false)
    (defvar json-null)
    (declare-function json-encode "json" (object))
    (lambda (object)
      (let ((json-false :json-false)
            (json-null nil))
        (json-encode object))))
  "Encode OBJECT into a JSON string.")

(cl-defun xlsp-rpc--reply
    (connection id &key (result nil result-supplied-p) (error nil error-supplied-p))
  "Reply to CONNECTION's request ID with RESULT or ERROR."
  (apply #'xlsp-rpc-connection-send connection
         `( :id ,id
            ,@(and result-supplied-p `(:result ,result))
            ,@(and error-supplied-p `(:error ,error)))))

(defun xlsp-rpc--call-deferred (connection)
  "Call CONNECTION's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (xlsp-rpc--deferred-actions connection))))
    (xlsp-rpc--debug connection `(:maybe-run-deferred
                                  ,(mapcar (apply-partially #'nth 2) actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defun xlsp-rpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (let ((connection (process-get proc 'xlsp-rpc-connection)))
    (xlsp-rpc--debug connection `(:message "Connection state changed" :change ,change))
    (unless (process-live-p proc)
      (with-current-buffer (xlsp-rpc-events-buffer connection)
        (let ((inhibit-read-only t))
          (insert "\n----------b---y---e---b---y---e----------\n")))
      ;; Cancel outstanding timers
      (maphash (lambda (_id triplet)
                 (pcase-let ((`(,_success ,_error ,timeout) triplet))
                   (when timeout (cancel-timer timeout))))
               (xlsp-rpc--request-continuations connection))
      (process-put proc 'xlsp-rpc-sentinel-cleanup-started t)
      (unwind-protect
          ;; Call all outstanding error handlers
          (maphash (lambda (_id triplet)
                     (pcase-let ((`(,_success ,error ,_timeout) triplet))
                       (funcall error '(:code -1 :message "Server died"))))
                   (xlsp-rpc--request-continuations connection))
        (xlsp-rpc--message "Server exited with status %s" (process-exit-status proc))
        (delete-process proc)
        (when-let (p (slot-value connection '-autoport-inferior)) (delete-process p))
        (funcall (xlsp-rpc--on-shutdown connection) connection)))))

(defun xlsp-rpc--process-filter (proc string)
  "Log STRING and possibly other strings piped in from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (connection (process-get proc 'xlsp-rpc-connection)))
        ;; Insert the text, advancing the process marker.
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (catch 'done
          (while t
            ;; More than one message might have arrived
            (unless (setf (xlsp-rpc--expected-bytes connection)
                          (or (xlsp-rpc--expected-bytes connection)
                              (and (search-forward-regexp
                                    "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                                    (+ (point) 100) t)
                                   (string-to-number (match-string 1)))))
              (throw 'done t))

            ;; Attempt to complete a message body
            (let ((available-bytes (- (position-bytes (process-mark proc))
                                      (position-bytes (point))))
                  (message-end (byte-to-position
                                (+ (position-bytes (point))
                                   (xlsp-rpc--expected-bytes connection)))))
              (if (< available-bytes (xlsp-rpc--expected-bytes connection))
                  (throw 'done t)       ; message still incomplete
                (unwind-protect
                    (save-restriction
                      (narrow-to-region (point) message-end)
                      (when-let ((json-message
                                  (condition-case err
                                      (xlsp-rpc--json-read)
                                    (error
                                     (prog1 nil
                                       (xlsp-rpc--warn "Invalid JSON: %s %s"
                                                       (cdr err) (buffer-string)))))))
                        (with-temp-buffer
                          ;; Calls success-fn and error-fn
                          ;; of xlsp-rpc-async-request, which
                          ;; can arbitrarily pollute or even kill
                          ;; (process-buffer PROC).  Ergo, buffer-live-p
                          ;; check in unwind clause.
                          (xlsp-rpc-connection-receive connection json-message))))
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char message-end)
                      (delete-region (point-min) (point))
                      (setf (xlsp-rpc--expected-bytes connection) nil))))))))))))

(cl-defun xlsp-rpc--async-request (connection method params
					      &rest args
					      &key success-fn error-fn timeout-fn deferred
					      (timeout xlsp-rpc-default-request-timeout)
					      &aux (orig-pt (point))
					      (orig-buffer (current-buffer))
					      (deferred-key (when deferred
							      `(,deferred ,orig-buffer))))
  "Returns a list (ID TIMER)."
  (let (id timer)
    (when deferred
      (let ((extant (gethash deferred-key (xlsp-rpc--deferred-actions connection))))
        (remhash deferred-key (xlsp-rpc--deferred-actions connection))
        ;; (FN TIMER ID)
        (setq timer (cl-second extant)
              id (or (cl-third extant)
                     (cl-incf (xlsp-rpc--next-request-id connection))))
        (if (xlsp-rpc-connection-ready-p connection deferred)
            (when timer (cancel-timer timer))
          ;; Conclude deferment.
          (xlsp-rpc--debug connection `( :deferring ,method
                                         :id ,id
                                         :params ,params))
          (puthash
           deferred-key
           (list (lambda ()
                   (when (buffer-live-p orig-buffer)
                     (with-current-buffer orig-buffer
                       (save-excursion (goto-char orig-pt)
                                       (apply #'xlsp-rpc-async-request
                                              connection
                                              method params args)))))
                 (setq timer
                       (or timer
                           (when timeout
                             (run-with-timer
                              timeout nil
                              (lambda ()
                                (remhash deferred-key (xlsp-rpc--deferred-actions connection))
                                (funcall (or timeout-fn
                                             (apply-partially #'xlsp-rpc--debug
                                                              connection
                                                              `( :timed-out ,method
                                                                 :id ,id
                                                                 :params ,params)))))))))
                 id)
           (xlsp-rpc--deferred-actions connection)))))
    (unless (gethash deferred-key (xlsp-rpc--deferred-actions connection))
      ;; Did not earlier conclude to defer
      (setq id (or id (cl-incf (xlsp-rpc--next-request-id connection))))
      (xlsp-rpc-connection-send connection
				:id id
				:method method
				:params params)
      (puthash id
               (list (or success-fn
                         (xlsp-rpc-lambda (&rest _ignored)
					  (xlsp-rpc--debug
					   connection (list :message "success ignored"
							    :id id))))
                     (or error-fn
                         (xlsp-rpc-lambda (&key code message &allow-other-keys)
					  (xlsp-rpc--debug
					   connection (list
						       :message
						       (format "error ignored, status set (%s)"
							       message)
						       :id id :error code))))
                     (setq timer
                           (when timeout
                             (run-with-timer
                              timeout nil
                              (lambda ()
                                (remhash id (xlsp-rpc--request-continuations connection))
                                (funcall (or timeout-fn
                                             (apply-partially #'xlsp-rpc--debug
                                                              connection
                                                              `( :timed-out ,method
                                                                 :id ,id
                                                                 :params ,params)))))))))
               (xlsp-rpc--request-continuations connection)))
    (list id timer)))

(defun xlsp-rpc--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[xlsp-rpc] %s" (apply #'format format args)))

(defun xlsp-rpc--debug (server format &rest args)
  "Debug message for SERVER with FORMAT and ARGS."
  (xlsp-rpc--log-event
   server (if (stringp format)
              `(:message ,(apply #'format format args))
            format)))

(defun xlsp-rpc--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'xlsp-rpc--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'xlsp-rpc
                     (apply #'format format args)
                     :warning)))

(defun xlsp-rpc--log-event (connection message &optional type)
  "Log a JSONRPC-related event.
CONNECTION is the current connection.  MESSAGE is a JSON-like
plist.  TYPE is a symbol saying if this is a client or server
originated."
  (let ((max (xlsp-rpc--events-buffer-scrollback-size connection)))
    (when (or (null max) (cl-plusp max))
      (with-current-buffer (xlsp-rpc-events-buffer connection)
        (cl-destructuring-bind (&key method id error &allow-other-keys) message
          (let* ((inhibit-read-only t)
                 (subtype (cond ((and method id)       'request)
                                (method                'notification)
                                (id                    'reply)
                                (t                     'message)))
                 (type
                  (concat (format "%s" (or type 'internal))
                          (if type
                              (format "-%s" subtype)))))
            (goto-char (point-max))
            (prog1
                (let ((msg (format "[%s]%s%s %s:\n%s"
                                   type
                                   (if id (format " (id:%s)" id) "")
                                   (if error " ERROR" "")
                                   (current-time-string)
				   (funcall (if (xlsp--test-p)
						#'pp-to-string
					      #'identity)
					    message))))
                  (when error
                    (setq msg (propertize msg 'face 'error)))
                  (insert-before-markers msg))
              ;; Trim the buffer if it's too large
              (when max
                (save-excursion
                  (goto-char (point-min))
                  (while (> (buffer-size) max)
                    (delete-region (point) (progn (forward-line 1)
                                                  (forward-sexp 1)
                                                  (forward-line 2)
                                                  (point)))))))))))))

(defun xlsp-rpc--forwarding-buffer (name prefix conn)
  "Helper for `xlsp-rpc-process-connection' helpers.
Make a stderr buffer named NAME, forwarding lines prefixed by
PREFIX to CONN's events buffer."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (fundamental-mode)
      (erase-buffer)
      (buffer-disable-undo)
      (add-hook
       'after-change-functions
       (lambda (beg _end _pre-change-len)
         (cl-loop initially (goto-char beg)
                  do (forward-line)
                  when (bolp)
                  for line = (buffer-substring
                              (line-beginning-position 0)
                              (line-end-position 0))
                  do (with-current-buffer (xlsp-rpc-events-buffer conn)
                       (goto-char (point-max))
                       (let ((inhibit-read-only t))
                         (insert (format "%s %s\n" prefix line))))
                  until (eobp)))
       nil t))
    (current-buffer)))


;;;; More convenience utils
(cl-defun xlsp-rpc-autoport-bootstrap (name contact
                                            &key connect-args)
  "Use CONTACT to start network server, then connect to it.

Return function suitable for the :PROCESS initarg of
`xlsp-rpc-process-connection' (which see).

CONTACT is a list where all the elements are strings except for
one, which is usuallky the keyword `:autoport'.

When the returned function is called it will start a program
using a command based on CONTACT, where `:autoport' is
substituted by a locally free network port.  Thereafter, a
network is made to this port.

Instead of the keyword `:autoport', a cons cell (:autoport
FORMAT-FN) is also accepted.  In that case FORMAT-FN is passed
the port number and should return a string used for the
substitution.

The internal processes and control buffers are named after NAME.

CONNECT-ARGS are passed as additional arguments to
`open-network-stream'."
  (lambda (conn)
    (let* ((port-probe (make-network-process :name "xlsp-rpc-port-probe-dummy"
                                             :server t
                                             :host "localhost"
                                             :service 0))
           (port-number (unwind-protect
                            (process-contact port-probe :service)
                          (delete-process port-probe)))
           (inferior-buffer (xlsp-rpc--forwarding-buffer
                             (format " *%s inferior output*" name)
                             "[inferior]"
                             conn))
           (cmd (cl-loop for e in contact
                         if (eq e :autoport) collect (format "%s" port-number)
                         else if (eq (car-safe e) :autoport)
                         collect (funcall (cdr e) port-number)
                         else collect e))
           inferior np)
      (unwind-protect
          (progn
            (message "[xlsp-rpc] Attempting to start `%s'"
                     (string-join cmd " "))
            (setq inferior
                  (make-process
                   :name (format "inferior (%s)" name)
                   :buffer inferior-buffer
                   :noquery t
                   :command cmd))
            (setq np
                  (cl-loop
                   repeat 10 for i from 0
                   do (accept-process-output nil 0.5)
                   while (process-live-p inferior)
                   do (message
                       "[xlsp-rpc] %sTrying to connect to localhost:%s (attempt %s)"
                       (if (zerop i) "Started.  " "")
                       port-number (1+ i))
                   thereis (ignore-errors
                             (apply #'open-network-stream
                                    (format "autostart (%s)" name)
                                    nil
                                    "localhost" port-number connect-args))))
            (setf (slot-value conn '-autoport-inferior) inferior)
            np)
        (cond ((and (process-live-p np)
                    (process-live-p inferior))
               (message "[xlsp-rpc] Done, connected to %s!" port-number))
              (t
               (when inferior (delete-process inferior))
               (when np (delete-process np))
               (error "[xlsp-rpc] Could not start and/or connect")))))))

(provide 'xlsp-rpc)
;;; xlsp-rpc.el ends here
