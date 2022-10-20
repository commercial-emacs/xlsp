;;; xlsp-handle-request.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Commercial Emacs

;; Authors: dick        <dickie.smalls@commandlinesystems.com>
;; URL: https://github.com/commercial-emacs/lsp
;; Version: 0.0.1
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'xlsp-utils)
(require 'xlsp-struct)

(declare-function xlsp-connection-register-watched-files "xlsp")
(declare-function xlsp-connection-unregister-watched-files "xlsp")

(cl-defgeneric xlsp-handle-request (conn method params))

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
