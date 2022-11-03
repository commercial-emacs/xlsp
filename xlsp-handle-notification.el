;;; xlsp-handle-notification.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'xlsp-utils)
(require 'xlsp-struct)

(declare-function xlsp-message "xlsp")

(cl-defgeneric xlsp-handle-notification (conn method params))

(cl-defmethod xlsp-handle-notification (_conn method _params)
  "Handle unknown."
  (xlsp-message "Did not handle notification %s" method))

(xlsp-register-handler notification xlsp-notification-text-document/publish-diagnostics
                       (_conn _params)
  (ignore))

(provide 'xlsp-handle-notification)
