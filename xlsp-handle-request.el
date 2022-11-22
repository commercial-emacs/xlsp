;;; xlsp-handle-request.el -*- lexical-binding: t; -*-

(require 'xlsp-utils)
(require 'xlsp-struct)

(declare-function xlsp-message "xlsp")
(declare-function xlsp-connection-register-watched-files "xlsp")
(declare-function xlsp-connection-unregister-watched-files "xlsp")
(declare-function xlsp-conn-files "xlsp")

(cl-defgeneric xlsp-handle-request (conn method params))
(cl-defmethod xlsp-handle-request (_conn method _params)
  "Handle unknown."
  (xlsp-message "Did not handle request %s" method))

(defmacro xlsp-evaluated-case (expr &rest clauses)
  (declare (indent 1) (debug (form &rest (sexp body))))
  `(cl-case ,expr
     ,@(mapcar (lambda (clause)
                 (cons (symbol-value (car clause)) (cdr clause)))
               clauses)))

(xlsp-register-handler request xlsp-request-workspace/configuration
                       (conn params)
  "A .dir-locals.el might look like:
\\((go-mode
    (xlsp-workspace-configuration
     :gopls ((:ui.completion.usePlaceholders t)
             (:codelenses (:generate :json-false)
                          (:gc_details t))))))"
  (apply
   #'xlsp-array
   (seq-map
    (lambda (item)
      (let ((scope-uri (xlsp-struct-configuration-item-scope-uri item))
            (section (xlsp-struct-configuration-item-section item)))
        (when-let ((path (xlsp-unurify scope-uri))
                   (default-directory (when (file-directory-p path)
                                        (file-name-as-directory path)))
                   (buf (ignore-errors
                          (find-buffer-visiting (car (xlsp-conn-files conn))))))
          (with-temp-buffer
            ;; Ad hoc Olsen in 1b21ee0:
            ;; "If the [dir-locals] element is of the form (MAJOR-MODE . ALIST),
            ;; and the buffer's major mode is derived from MAJOR-MODE,
            ;; then all the variables in ALIST are applied."
            (setq-local major-mode
                        (buffer-local-value 'major-mode buf))
            ;; Calls `dir-locals-find-file' to find nearest .dir-locals.el.
            (hack-dir-local-variables-non-file-buffer)
            (or (plist-get (or (bound-and-true-p xlsp-workspace-configuration)
                               (bound-and-true-p eglot-workspace-configuration))
                           (intern (concat ":" section)))
                xlsp-struct-empty)))))
    (xlsp-struct-configuration-params-items params))))

(xlsp-register-handler request xlsp-request-client/register-capability
                       (conn params)
  (prog1 nil ; provisional
    (seq-map
     (lambda (reg)
       (let ((method (intern-soft (xlsp-struct-registration-method reg))))
         (xlsp-evaluated-case method
           (xlsp-notification-workspace/did-change-watched-files
            (xlsp-connection-register-watched-files conn reg)))))
     (xlsp-struct-registration-params-registrations params))))

(xlsp-register-handler request xlsp-request-client/unregister-capability
                       (conn params)
  (prog1 nil ; provisional
    (seq-map
     (lambda (unreg)
       (let ((method (intern-soft (xlsp-struct-unregistration-method unreg))))
         (xlsp-evaluated-case method
           (xlsp-notification-workspace/did-change-watched-files
            (xlsp-connection-unregister-watched-files conn unreg)))))
     (xlsp-struct-unregistration-params-unregisterations params))))

(provide 'xlsp-handle-request)
