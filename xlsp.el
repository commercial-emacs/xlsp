;;; xlsp.el --- Language Server Protocol client. -*- lexical-binding: t; -*-

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
;;      (xlsp-workspace-configuration
;;       (:pylsp (:plugins (:jedi_completion (:include_params t :fuzzy t)
;;                          :pylint (:enabled :json-false)))))))
;;
;; (dir-locals-set-directory-class "my-project-dir" 'my-project-lsp)

;;; Code:

(require 'jsonrpc)
(require 'url-util)
(require 'project)
(require 'xlsp-handle-request)
(require 'xlsp-handle-notification)

(defgroup xlsp nil
  "Language Server Protocol."
  :prefix "xlsp-"
  :group 'applications)

(defvar xlsp--connections nil
  "Global alist of ((MODE . INODE-NUM) . JSONRPC-PROCESS-CONNECTION).
I use inode in case project directory gets renamed.")

(defvar xlsp--inode-data (make-hash-table)
  "Every time you `xlsp-inode', make a backref to original directory.")

(defun xlsp-inode (directory)
  (when-let ((inode (file-attribute-inode-number
                     (file-attributes (expand-file-name directory)))))
    (prog1 inode
      (puthash inode directory xlsp--inode-data))))

(defmacro xlsp-gv-connection (conn-key)
  `(alist-get ,conn-key xlsp--connections nil nil #'equal))

(defmacro xlsp-project-root (project)
  "`project-root' only exists in emacs-28"
  (if (fboundp 'project-root)
      `(project-root ,project)
    (with-suppressed-warnings ((obsolete project-roots))
      `(car (project-roots ,project)))))

(defmacro with-xlsp-connection (args buffer &rest body)
  (declare (indent 2))
  (cl-destructuring-bind (conn-key project-dir)
      args
    `(when-let ((mode (buffer-local-value 'major-mode ,buffer))
                (file-name (buffer-file-name ,buffer))
                (inode
                 (xlsp-inode (expand-file-name
                             (if-let ((project (project-current nil file-name)))
                                 (xlsp-project-root project)
                               (if (file-directory-p file-name)
                                   file-name
                                 (file-name-directory file-name))))))
                (,project-dir (gethash inode xlsp--inode-data))
                (,conn-key (cons mode inode)))
       ,@body)))

(defun xlsp-connection-remove (buffer)
  (with-xlsp-connection (conn-key project-dir)
      buffer
    (when-let ((conn (xlsp-gv-connection conn-key)))
      (jsonrpc-shutdown conn :cleanup))
    (setq xlsp--connections (assoc-delete-all conn-key xlsp--connections))))

(defun xlsp-connection-get (buffer)
  (with-xlsp-connection (conn-key project-dir)
      buffer
    (or (xlsp-gv-connection conn-key)
        (setf (xlsp-gv-connection conn-key) (xlsp--connect buffer project-dir)))))

(defun xlsp-connection-reset (buffer)
  (xlsp-connection-remove buffer)
  (xlsp-connection-get buffer))

(defun xlsp--initialization-options (project-dir)
  (ignore project-dir))

(defconst xlsp-made-struct-client-capabilities
  (make-xlsp-struct-client-capabilities
   :workspace (make-xlsp-struct-workspace-client-capabilities
               :workspace-edit (make-xlsp-struct-workspace-edit-client-capabilities
                                :document-changes t)
               :configuration t
               :workspace-folders t)
   :text-document (make-xlsp-struct-text-document-client-capabilities
                   :synchronization (make-xlsp-struct-text-document-sync-client-capabilities
                                     :will-save t
                                     :will-save-wait-until t
                                     :did-save t)
                   :completion (make-xlsp-struct-completion-client-capabilities
                                :completion-item
                                (xlsp-literal
                                 :snippetSupport :json-false
                                 :deprecatedSupport t
                                 :tagSupport (xlsp-literal
                                              :valueSet (xlsp-array 1)))
                                :context-support t)
                   :hover (make-xlsp-struct-hover-client-capabilities
                           :content-format (xlsp-array xlsp-markup-kind/plain-text
                                                      xlsp-markup-kind/markdown))
                   :signature-help (make-xlsp-struct-signature-help-client-capabilities
                                    :signature-information
                                    (xlsp-literal
                                     :parameterInformation
                                     (xlsp-literal :labelOffSupport t)
                                     :activeParameterSupport t))
                   :declaration (make-xlsp-struct-declaration-client-capabilities
                                 :link-support t)
                   :definition (make-xlsp-struct-definition-client-capabilities
                                :link-support t)
                   :type-definition (make-xlsp-struct-type-definition-client-capabilities
                                     :link-support t)
                   :implementation (make-xlsp-struct-implementation-client-capabilities
                                    :link-support t)
                   :document-symbol (make-xlsp-struct-document-symbol-client-capabilities
                                     :hierarchical-document-symbol-support t
                                     :symbol-kind
                                     (xlsp-literal
                                      :valueSet
                                      (apply
                                       #'xlsp-array
                                       (mapcar
                                        (lambda (x)
                                          (symbol-value
                                           (intern-soft
                                            (xlsp-namespace x "xlsp-symbol-kind"))))
                                        '("file" "module" "namespace" "package" "class"
                                          "method" "property" "field" "constructor"
                                          "enum" "interface" "function" "variable"
                                          "constant" "string" "number" "boolean" "array"
                                          "object" "key" "null" "enum-member" "struct"
                                          "event" "operator" "type-parameter")))))
                   :code-action (make-xlsp-struct-code-action-client-capabilities
                                 :code-action-literal-support
                                 (xlsp-literal
                                  :codeActionKind
                                  (xlsp-literal
                                   :valueSet
                                   (apply
                                    #'xlsp-array
                                    (mapcar
                                     (lambda (x)
                                       (symbol-value
                                        (intern-soft
                                         (xlsp-namespace x "xlsp-code-action-kind"))))
                                     '("quick-fix" "refactor" "refactor-extract"
                                       "refactor-inline" "refactor-rewrite"
                                       "source" "source-organize-imports"))))))
                   :publish-diagnostics (make-xlsp-struct-publish-diagnostics-client-capabilities
                                         :related-information :json-false
                                         :code-description-support :json-false
                                         :tag-support
                                         (xlsp-literal
                                          :valueSet
                                          (apply
                                           #'xlsp-array
                                           (mapcar
                                            (lambda (x)
                                              (symbol-value
                                               (intern-soft
                                                (xlsp-namespace x "xlsp-diagnostic-tag"))))
                                            '("unnecessary" "deprecated"))))))))

(defun xlsp--capabilities (project-dir)
  (ignore project-dir)
  xlsp-made-struct-client-capabilities)

(defcustom xlsp-invocations
  (quote ((c-mode . "clangd --header-insertion-decorators=0")
          (c++-mode . "clangd --header-insertion-decorators=0")
          (objc-mode . "clangd --header-insertion-decorators=0")))
  "Alist values must begin with an executable, e.g., clangd.
If you need to set environment variables,
try \"env FOO=foo bash -c \\='echo $FOO\\='\"."
  :type '(alist :key-type symbol :value-type string))

(defcustom xlsp-events-buffer-size (truncate 2e6)
  "Events buffer max size.  Zero for no buffer, nil for infinite."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Number of characters")))

(defun xlsp-urify (path)
  "RFC3986, like all RFCs, are write-only.
https://microsoft.github.io/language-server-protocol/specifications/\\
lsp/3.17/specification/#uri"
  (let ((url-path-allowed-chars (copy-sequence url-path-allowed-chars)))
    (aset url-path-allowed-chars ?: nil)
    (url-encode-url (concat "file://" (expand-file-name path)))))

(defun xlsp-message (format &rest args)
  "Message out with FORMAT with ARGS."
  (let ((lhs (format-time-string "%Y%m%dT%H%M%S [lsp] " (current-time))))
    (apply #'message (concat lhs format) args)))

(defun xlsp--connect (buffer project-dir)
  "Return existing or new jsonrpc-process-connection."
  (let* ((mode (buffer-local-value 'major-mode buffer))
         (name (format "%s/%s" mode (file-name-nondirectory
                                    (directory-file-name project-dir))))
         (initialize-params
          (make-xlsp-struct-initialize-params
           :process-id (emacs-pid)
           :initialization-options (xlsp--initialization-options project-dir)
           :capabilities (xlsp--capabilities project-dir)
           :workspace-folders (xlsp-array
                               (make-xlsp-struct-workspace-folder
                                :uri (xlsp-urify project-dir)
                                :name project-dir))))
         (command (or (alist-get mode xlsp-invocations) "false"))
         (conn (make-instance
                'jsonrpc-process-connection
                :name name
                :events-buffer (get-buffer-create (format " *%s events*" name))
                :events-buffer-scrollback-size xlsp-events-buffer-size
                :notification-dispatcher #'xlsp-handle-notification
                :request-dispatcher #'xlsp-handle-request
                :process (make-process
                          :name name
                          :command (split-string command)
                          :connection-type 'pipe
                          :coding 'utf-8-emacs-unix
                          :noquery t
                          :stderr (get-buffer-create (format "*%s stderr*" name))
                          :file-handler t))))
    (xlsp-message "%s executing: %s" name command)
    (condition-case-unless-debug err
        (jsonrpc-async-request
         conn
         'initialize
         (xlsp-jsonify initialize-params)
         :success-fn (apply-partially
                      (lambda (buffer* _result)
                        "RESULT is a plist."
                        (jsonrpc-notify
                         conn 'initialized
                         (xlsp-jsonify
                          (make-xlsp-struct-initialized-params))) ;; ack
                        (jsonrpc-notify
                         conn 'textDocument/didOpen
                         (xlsp-jsonify
                          (make-xlsp-struct-did-open-text-document-params
                           :text-document (make-xlsp-struct-text-document-item
                                           :uri (xlsp-urify (buffer-file-name buffer*))
                                           :language-id ""
                                           :version 0
                                           :text (with-current-buffer buffer*
                                                   (save-restriction
                                                     (widen)
                                                     (buffer-substring-no-properties
                                                      (point-min) (point-max))))))))
                        (when-let ((settings
                                    (bound-and-true-p xlsp-workspace-configuration)))
                          (jsonrpc-notify
                           conn 'workspace/didChangeConfiguration
                           (xlsp-jsonify
                            (make-xlsp-struct-did-change-configuration-params
                             :settings settings)))))
                      buffer)
         :error-fn (apply-partially
                    (lambda (buffer* error)
                      (xlsp-message "%s %s (%s)" name (plist-get error :message)
                                   (plist-get error :code))
                      (xlsp-connection-remove buffer*))
                    buffer)
         :timeout 10
         :timeout-fn (apply-partially
                      (lambda (buffer*)
                        (xlsp-message "%s timed out" name)
                        (xlsp-connection-remove buffer*))
                      buffer))
      (error (jsonrpc-shutdown conn :cleanup)
             (setq conn nil)
             (signal (car err) (cdr err))))
    conn))

(put 'xlsp-workspace-configuration 'safe-local-variable 'listp) ; see Commentary

(provide 'xlsp)

;;; xlsp.el ends here
