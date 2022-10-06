;;; lsp-handle-notification.el -*- lexical-binding: t; -*-

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

(require 'lsp-struct)

(defun lsp-notification-param-type (method)
  (let* ((struct-type
          (intern
           (lsp-notification
            (list (cons 'method (lsp-hyphenate (symbol-name method)))))))
         (slots (cl-remove-if-not #'cdr (cl-struct-slot-info struct-type))))
    (cl-destructuring-bind (_sym _default &key type &allow-other-keys)
        (assoc 'params slots)
      type)))

(cl-defgeneric lsp-handle-notification (conn method params))

(cl-defmethod lsp-handle-notification (_conn _method _params)
  "Handle unknown.")

(cl-defmethod lsp-handle-notification
  (_conn (method (eql textDocument/publishDiagnostics)) params)
  "Handle it."
  (lsp-unjsonify (lsp-notification-param-type method) params))

(provide 'lsp-handle-notification)
