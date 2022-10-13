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
(require 'xlsp-completion)
(require 'xlsp-handle-request)
(require 'xlsp-handle-notification)

(defgroup xlsp nil
  "Language Server Protocol."
  :prefix "xlsp-"
  :group 'applications)

(defclass xlsp-connection (jsonrpc-process-connection)
  ((ready-p :initform nil :type boolean :documentation "Handshake completed.")
   (capabilities :initform nil)
   (server-info :initform nil)))

(defvar xlsp--connections nil
  "Global alist of ((MODE . INODE-NUM) . XLSP-CONNECTION).
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
                   :completion (make-xlsp-struct-completion-client-capabilities)
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
  "Return existing or new xlsp-connection."
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
         (process (make-process
                   :name name
                   :command (split-string command)
                   :connection-type 'pipe
                   :coding 'utf-8-emacs-unix
                   :noquery t
                   :stderr (get-buffer-create (format "*%s stderr*" name))
                   :file-handler t))
         (conn (condition-case err
                   (make-instance
                    'xlsp-connection
                    :name name
                    :events-buffer-scrollback-size xlsp-events-buffer-size
                    :notification-dispatcher #'xlsp-handle-notification
                    :request-dispatcher #'xlsp-handle-request
                    :process process)
                 (error (delete-process process)
                        (signal (car err) (cdr err))))))
    (with-current-buffer (jsonrpc-events-buffer conn)
      ;; I don't make the rules of jsonrpc.el
      (let ((hidden (format " %s" (buffer-name))))
        (when-let ((hidden-buffer (get-buffer hidden)))
          (let (kill-buffer-query-functions)
            (kill-buffer hidden-buffer)))
        (rename-buffer hidden)))
    (xlsp-message "%s executing: %s" name command)
    (condition-case err
        (jsonrpc-async-request
         conn
         'initialize
         (xlsp-jsonify initialize-params)
         :success-fn (apply-partially
                      (lambda (buffer* result-plist)
                        (let ((result (xlsp-unjsonify 'xlsp-struct-initialize-result
                                                      result-plist)))
                          (oset conn ready-p t)
                          (oset conn capabilities
                                (xlsp-struct-initialize-result-capabilities result))
                          (oset conn server-info
                                (xlsp-struct-initialize-result-server-info result))
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
                               :settings settings))))))
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

(cl-defmethod xlsp-handle-notification
  (_conn (method (eql textDocument/publishDiagnostics)) params)
  "Handle it."
  (let ((struct-params
         (xlsp-unjsonify (xlsp-notification-param-type method) params)))
    (ignore struct-params)))

(cl-defstruct xlsp-struct-synchronize
  "Keep shit together."
  (buffer nil :type buffer)
  (version 0 :type integer)
  (events nil :type (list-of xlsp-literal)))

(defun xlsp-synchronize-closure ()
  (or (bound-and-true-p xlsp--synchronize-closure)
      (set (make-local-variable 'xlsp--synchronize-closure)
           (apply-partially
            (cl-function
             (lambda (synchronize* &key cumulate send &allow-other-keys)
               "SYNCHRONIZE* is naturally thread unsafe state."
               (when cumulate
                 (push cumulate (xlsp-struct-synchronize-events synchronize*))
                 (cl-incf (xlsp-struct-synchronize-version synchronize*)))
               (when send
                 (make-xlsp-struct-did-change-text-document-params
                  :text-document
                  (make-xlsp-struct-versioned-text-document-identifier
                   :uri (xlsp-urify
                         (buffer-file-name
                          (xlsp-struct-synchronize-buffer synchronize*)))
                   :version (xlsp-struct-synchronize-version synchronize*))
                  :content-changes
                  (prog1 (nreverse
                          (copy-sequence
                           (xlsp-struct-synchronize-events synchronize*)))
                    ;; Events sent; clear for next batch.
                    (setf (xlsp-struct-synchronize-events synchronize*) nil))))))
            (make-xlsp-struct-synchronize :buffer (current-buffer))))))

(defun xlsp-will-save-text-document ()
  (or (bound-and-true-p xlsp--will-save-text-document)
      (set (make-local-variable 'xlsp--will-save-text-document)
           (apply-partially
            (lambda ()
              )))))

(defun xlsp-did-save-text-document ()
  (or (bound-and-true-p xlsp--did-save-text-document)
      (set (make-local-variable 'xlsp--did-save-text-document)
           (apply-partially
            (lambda ()
              )))))

(defun xlsp-did-change-text-document ()
  (or (bound-and-true-p xlsp--did-change-text-document)
      (set (make-local-variable 'xlsp--did-change-text-document)
           (apply-partially
            (lambda (before-state* beg end &optional deleted)
              "BEFORE-STATE* is naturally thread unsafe."
              (cl-flet ((get-row-col (pos)
                          (let ((send-pos
                                 (with-temp-buffer
                                   (save-excursion
                                     (insert (alist-get 'snippet before-state*)))
                                   (xlsp-completion-position
                                    (current-buffer)
                                    (min pos (point-max))))))
                            (make-xlsp-struct-position
                             :line (+ (alist-get 'line-offset before-state*)
                                      (car send-pos))
                             :character (cdr send-pos)))))
                (if (not deleted)
                    ;; In `before-change-functions'
                    (save-excursion
                      (goto-char beg)
                      (save-restriction
                        (widen)
                        (setf (alist-get 'character-offset before-state*)
                              (- beg (line-beginning-position))
                              (alist-get 'line-offset before-state*)
                              (1- (line-number-at-pos beg))
                              (alist-get 'snippet before-state*)
                              (buffer-substring-no-properties (line-beginning-position)
                                                              end))))
                  ;; In `after-change-functions'
                  (let ((pos (1+ (alist-get 'character-offset before-state*))))
                    (funcall
                     (xlsp-synchronize-closure)
                     :cumulate
                     (xlsp-literal
                      :range (make-xlsp-struct-range
                              ;; get-row-col POS is one-indexed
                              :start (get-row-col pos)
                              :end (get-row-col (+ pos deleted)))
                      :text (buffer-substring-no-properties beg end))))
                  (setf (alist-get 'character-offset before-state*) nil
                        (alist-get 'line-offset before-state*) nil
                        (alist-get 'snippet before-state*) nil))))
            '((character-offset . nil) (line-offset . nil) (snippet . nil))))))

(define-minor-mode xlsp-mode
  "Start and consult language server if applicable."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (prog1 map
              (define-key map (kbd "C-M-i") #'xlsp-completion-in-region)))
  :group 'xlsp
  (cl-macrolet ((install-hooks (&rest pairs)
                  `(dolist (entry ',pairs)
                     (cl-destructuring-bind (hooks . closure)
                         entry
                       (if xlsp-mode
                           (add-hook hooks (funcall closure) nil :local)
                         (remove-hook hooks (funcall closure) :local))))))
    (install-hooks (before-change-functions . xlsp-did-change-text-document)
                   (after-change-functions . xlsp-did-change-text-document)
                   (before-save-hook . xlsp-will-save-text-document)
                   (after-save-hook . xlsp-did-save-text-document))))

(cl-defmethod jsonrpc-connection-ready-p ((conn xlsp-connection) _what)
  (and (cl-call-next-method) (oref conn ready-p)))

(put 'xlsp-workspace-configuration 'safe-local-variable 'listp) ; see Commentary

(provide 'xlsp)

;;; xlsp.el ends here
