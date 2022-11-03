;;; xlsp-handle-request.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'xlsp-utils)
(require 'xlsp-struct)

(declare-function xlsp-message "xlsp")
(declare-function xlsp-connection-register-watched-files "xlsp")
(declare-function xlsp-connection-unregister-watched-files "xlsp")

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

(xlsp-register-handler request xlsp-request-client/register-capability
                       (conn params)
  (prog1 nil
    (seq-map
     (lambda (reg)
       (let ((method (intern-soft (xlsp-struct-registration-method reg))))
         (xlsp-evaluated-case method
           (xlsp-notification-workspace/did-change-watched-files
            (xlsp-connection-register-watched-files conn reg)))))
     (xlsp-struct-registration-params-registrations params))))

(xlsp-register-handler request xlsp-request-client/unregister-capability
                       (conn params)
  (prog1 nil
    (seq-map
     (lambda (unreg)
       (let ((method (intern-soft (xlsp-struct-unregistration-method unreg))))
         (xlsp-evaluated-case method
           (xlsp-notification-workspace/did-change-watched-files
            (xlsp-connection-unregister-watched-files conn unreg)))))
     (xlsp-struct-unregistration-params-unregisterations params))))

(provide 'xlsp-handle-request)
