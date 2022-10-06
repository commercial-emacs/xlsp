;;; lsp.el --- Language Server Protocol client. -*- lexical-binding: t; -*-

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

;; Loitering .dir-locals.el files are a pain in the tuchus.
;; If project-specific config is a must, we recommend this pain in
;; your .emacs instead:
;;
;; (dir-locals-set-class-variables 'my-project-lsp
;;   '((python-mode
;;      (lsp-workspace-configuration
;;       (:pylsp (:plugins (:jedi_completion (:include_params t :fuzzy t)
;;                          :pylint (:enabled :json-false)))))))
;;
;; (dir-locals-set-directory-class "my-project-dir" 'my-project-lsp)

;;; Code:

(require 'json)
(require 'jsonrpc)
(require 'url-util)
(require 'project)
(require 'lsp-handle-request)
(require 'lsp-handle-notification)

(defgroup lsp nil
  "Language Server Protocol."
  :prefix "lsp-"
  :group 'applications)

(defvar lsp--connections nil
  "Global alist of ((MODE . INODE-NUM) . JSONRPC-PROCESS-CONNECTION).
I use inode in case project directory gets renamed.")

(defvar lsp--inode-data (make-hash-table)
  "Every time you `lsp-inode', make a backref to original directory.")

(defun lsp-inode (directory)
  (when-let ((inode (file-attribute-inode-number
                     (file-attributes (expand-file-name directory)))))
    (prog1 inode
      (puthash inode directory lsp--inode-data))))

(defmacro lsp-gv-connection (conn-key)
  `(alist-get ,conn-key lsp--connections nil nil #'equal))

(defmacro lsp-project-root (project)
  "`project-root' only exists in emacs-28"
  (if (fboundp 'project-root)
      `(project-root ,project)
    (with-suppressed-warnings ((obsolete project-roots))
      `(car (project-roots ,project)))))

(defmacro with-lsp-connection (args buffer &rest body)
  (declare (indent 2))
  (cl-destructuring-bind (conn-key project-dir)
      args
    `(when-let ((mode (buffer-local-value 'major-mode ,buffer))
                (file-name (buffer-file-name ,buffer))
                (inode
                 (lsp-inode (expand-file-name
                             (if-let ((project (project-current nil file-name)))
                                 (lsp-project-root project)
                               (if (file-directory-p file-name)
                                   file-name
                                 (file-name-directory file-name))))))
                (,project-dir (gethash inode lsp--inode-data))
                (,conn-key (cons mode inode)))
       ,@body)))

(defun lsp-connection-remove (buffer)
  (with-lsp-connection (conn-key project-dir)
      buffer
    (when-let ((conn (lsp-gv-connection conn-key)))
      (jsonrpc-shutdown conn :cleanup))
    (setq lsp--connections (assoc-delete-all conn-key lsp--connections))))

(defun lsp-connection-get (buffer)
  (with-lsp-connection (conn-key project-dir)
      buffer
    (or (lsp-gv-connection conn-key)
        (setf (lsp-gv-connection conn-key) (lsp--connect buffer project-dir)))))

(defun lsp-connection-reset (buffer)
  (lsp-connection-remove buffer)
  (lsp-connection-get buffer))

(defun lsp--initialization-options (project-dir)
  (ignore project-dir))

(defun lsp-literal (&rest args)
  (apply #'list args))

(defun lsp-array (&rest args)
  (apply #'vector args))

(defconst lsp-made-struct-client-capabilities
  (make-lsp-struct-client-capabilities
   :workspace (make-lsp-struct-workspace-client-capabilities
               :workspace-edit (make-lsp-struct-workspace-edit-client-capabilities
                                :document-changes t)
               :configuration t
               :workspace-folders t)
   :text-document (make-lsp-struct-text-document-client-capabilities
                   :synchronization (make-lsp-struct-text-document-sync-client-capabilities
                                     :will-save t
                                     :will-save-wait-until t
                                     :did-save t)
                   :completion (make-lsp-struct-completion-client-capabilities
                                :completion-item
                                (lsp-literal
                                 :snippetSupport :json-false
                                 :deprecatedSupport t
                                 :tagSupport (lsp-literal
                                              :valueSet (lsp-array 1)))
                                :context-support t)
                   :hover (make-lsp-struct-hover-client-capabilities
                           :content-format (lsp-array lsp-markup-kind/plain-text
                                                      lsp-markup-kind/markdown))
                   :signature-help (make-lsp-struct-signature-help-client-capabilities
                                    :signature-information
                                    (lsp-literal
                                     :parameterInformation
                                     (lsp-literal :labelOffSupport t)
                                     :activeParameterSupport t))
                   :declaration (make-lsp-struct-declaration-client-capabilities
                                 :link-support t)
                   :definition (make-lsp-struct-definition-client-capabilities
                                :link-support t)
                   :type-definition (make-lsp-struct-type-definition-client-capabilities
                                     :link-support t)
                   :implementation (make-lsp-struct-implementation-client-capabilities
                                    :link-support t)
                   :document-symbol (make-lsp-struct-document-symbol-client-capabilities
                                     :hierarchical-document-symbol-support t
                                     :symbol-kind
                                     (lsp-literal
                                      :valueSet
                                      (apply
                                       #'lsp-array
                                       (mapcar
                                        (lambda (x)
                                          (symbol-value
                                           (intern-soft
                                            (lsp-namespace x "lsp-symbol-kind"))))
                                        '("file" "module" "namespace" "package" "class"
                                          "method" "property" "field" "constructor"
                                          "enum" "interface" "function" "variable"
                                          "constant" "string" "number" "boolean" "array"
                                          "object" "key" "null" "enum-member" "struct"
                                          "event" "operator" "type-parameter")))))
                   :code-action (make-lsp-struct-code-action-client-capabilities
                                 :code-action-literal-support
                                 (lsp-literal
                                  :codeActionKind
                                  (lsp-literal
                                   :valueSet
                                   (apply
                                    #'lsp-array
                                    (mapcar
                                     (lambda (x)
                                       (symbol-value
                                        (intern-soft
                                         (lsp-namespace x "lsp-code-action-kind"))))
                                     '("quick-fix" "refactor" "refactor-extract"
                                       "refactor-inline" "refactor-rewrite"
                                       "source" "source-organize-imports"))))))
                   :publish-diagnostics (make-lsp-struct-publish-diagnostics-client-capabilities
                                         :related-information :json-false
                                         :code-description-support :json-false
                                         :tag-support
                                         (lsp-literal
                                          :valueSet
                                          (apply
                                           #'lsp-array
                                           (mapcar
                                            (lambda (x)
                                              (symbol-value
                                               (intern-soft
                                                (lsp-namespace x "lsp-diagnostic-tag"))))
                                            '("unnecessary" "deprecated"))))))))

(defun lsp--capabilities (project-dir)
  (ignore project-dir)
  lsp-made-struct-client-capabilities)

(defcustom lsp-invocations
  (quote ((c-mode . "clangd --header-insertion-decorators=0")
          (c++-mode . "clangd --header-insertion-decorators=0")
          (objc-mode . "clangd --header-insertion-decorators=0")))
  "Alist values must begin with an executable, e.g., clangd.
If you need to set environment variables,
try \"env FOO=foo bash -c \\='echo $FOO\\='\"."
  :type '(alist :key-type symbol :value-type string))

(defcustom lsp-events-buffer-size (truncate 2e6)
  "Events buffer max size.  Zero for no buffer, nil for infinite."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Number of characters")))

(defun lsp-urify (path)
  "RFC3986, like all RFCs, are write-only.
https://microsoft.github.io/language-server-protocol/specifications/\\
lsp/3.17/specification/#uri"
  (let ((url-path-allowed-chars (copy-sequence url-path-allowed-chars)))
    (aset url-path-allowed-chars ?: nil)
    (url-encode-url (concat "file://" (expand-file-name path)))))

(defun lsp-message (format &rest args)
  "Message out with FORMAT with ARGS."
  (let ((lhs (format-time-string "%Y%m%dT%H%M%S [lsp] " (current-time))))
    (apply #'message (concat lhs format) args)))

(defun lsp--connect (buffer project-dir)
  "Return existing or new jsonrpc-process-connection."
  (let* ((mode (buffer-local-value 'major-mode buffer))
         (name (format "%s/%s" mode (file-name-nondirectory
                                    (directory-file-name project-dir))))
         (initialize-params
          (make-lsp-struct-initialize-params
           :process-id (emacs-pid)
           :initialization-options (lsp--initialization-options project-dir)
           :capabilities (lsp--capabilities project-dir)
           :workspace-folders (lsp-array
                               (make-lsp-struct-workspace-folder
                                :uri (lsp-urify project-dir)
                                :name project-dir))))
         (command (or (alist-get mode lsp-invocations) "false"))
         (conn (make-instance
                'jsonrpc-process-connection
                :name name
                :events-buffer (get-buffer-create (format " *%s events*" name))
                :events-buffer-scrollback-size lsp-events-buffer-size
                :notification-dispatcher #'lsp-handle-notification
                :request-dispatcher #'lsp-handle-request
                :process (make-process
                          :name name
                          :command (split-string command)
                          :connection-type 'pipe
                          :coding 'utf-8-emacs-unix
                          :noquery t
                          :stderr (get-buffer-create (format "*%s stderr*" name))
                          :file-handler t))))
    (lsp-message "%s executing: %s" name command)
    (condition-case-unless-debug err
        (jsonrpc-async-request
         conn
         'initialize
         (lsp-jsonify initialize-params)
         :success-fn (apply-partially
                      (lambda (buffer* _result)
                        "RESULT is a plist."
                        (jsonrpc-notify
                         conn 'initialized
                         (lsp-jsonify
                          (make-lsp-struct-initialized-params))) ;; ack
                        (jsonrpc-notify
                         conn 'textDocument/didOpen
                         (lsp-jsonify
                          (make-lsp-struct-did-open-text-document-params
                           :text-document (make-lsp-struct-text-document-item
                                           :uri (lsp-urify (buffer-file-name buffer*))
                                           :language-id ""
                                           :version 0
                                           :text (with-current-buffer buffer*
                                                   (save-restriction
                                                     (widen)
                                                     (buffer-substring-no-properties
                                                      (point-min) (point-max))))))))
                        (when-let ((settings
                                    (bound-and-true-p lsp-workspace-configuration)))
                          (jsonrpc-notify
                           conn 'workspace/didChangeConfiguration
                           (lsp-jsonify
                            (make-lsp-struct-did-change-configuration-params
                             :settings settings)))))
                      buffer)
         :error-fn (apply-partially
                    (lambda (buffer* error)
                      (lsp-message "%s %s (%s)" name (plist-get error :message)
                                   (plist-get error :code))
                      (lsp-connection-remove buffer*))
                    buffer)
         :timeout 10
         :timeout-fn (apply-partially
                      (lambda (buffer*)
                        (lsp-message "%s timed out" name)
                        (lsp-connection-remove buffer*))
                      buffer))
      (error (jsonrpc-shutdown conn :cleanup)
             (setq conn nil)
             (signal (car err) (cdr err))))
    conn))

(put 'lsp-workspace-configuration 'safe-local-variable 'listp) ; see Commentary

(provide 'lsp)

;;; lsp.el ends here
