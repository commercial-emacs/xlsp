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

(eval-and-compile
  (require 'lsp-hyphenate)
  (defconst lsp-requests (with-temp-buffer
                           (save-excursion (insert-file-contents "_requests.el"))
                           (read (current-buffer))))
  (defconst lsp-notifications (with-temp-buffer
                                (save-excursion (insert-file-contents "_notifications.el"))
                                (read (current-buffer))))
  (defconst lsp-structures (with-temp-buffer
                             (save-excursion (insert-file-contents "_structures.el"))
                             (read (current-buffer))))
  (defconst lsp-enumerations (with-temp-buffer
                               (save-excursion (insert-file-contents "_enumerations.el"))
                               (read (current-buffer))))

  (defun lsp-structure (alist)
    (let-alist alist
      (concat "lsp-struct-" (lsp-hyphenate .name))))

  (defun lsp-request (alist)
    (let-alist alist
      (concat "lsp-request-" (lsp-hyphenate .method))))

  (defun lsp-notification (alist)
    (let-alist alist
      (concat "lsp-notification-" (lsp-hyphenate .method))))

  (defun lsp-enumeration (alist)
    (let-alist alist
      (concat "lsp-" (lsp-hyphenate .name))))

  (defun lsp-structure-type (alist)
    (let-alist alist
      (cond ((equal .kind "reference")
             (make-symbol (concat "lsp-struct-" (lsp-hyphenate .name))))
            (.items
             (lsp-structure-type (aref .items 0)))
            (t (make-symbol (lsp-hyphenate (or .name .kind)))))))

  (defun lsp-property-type (alist)
    "Supposedly SLOT-OPTION :type only used for documentation."
    (let-alist alist
      (if (equal "array" .type.kind)
          (list 'list-of (lsp-structure-type .type.element))
        (lsp-structure-type .type))))

  (defun lsp-namespace (variable namespace)
    (concat (lsp-hyphenate namespace) "/" (lsp-hyphenate variable)))

  (defun lsp-top-sort (structures)
    "Return STRUCTURES such that a structure's dependency precedes it."
    (let (seen alist)
      (dotimes (i (length structures))
        (push `(,(alist-get 'name (aref structures i)) .
                ,(aref structures i))
              alist))
      (cl-flet ((get-unseen-parents
                  (struct)
                  (let ((parents (alist-get 'extends struct)))
                    (delq nil
                          (append
                           (seq-map
                            (lambda (parent)
                              (and (equal "reference" (alist-get 'kind parent))
                                   (when-let ((name (alist-get 'name parent)))
                                     (unless (member name seen)
                                       name))))
                            parents)
                           nil)))))
        (cl-loop with queue = (mapcar #'car alist)
                 with result
                 until (null queue)
                 for name = (car queue)
                 for parents = (get-unseen-parents (assoc-default name alist))
                 do (if (member name seen)
                        (pop queue)
                      (if parents
                          (dolist (parent parents)
                            (if (assoc-default parent alist)
                                (push parent queue)
                              (push parent seen)))
                        (push (assoc-default name alist) result)
                        (push name seen)
                        (pop queue)))
                 finally return (nreverse result)))))

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(cl-defstruct (,(intern (lsp-structure entry))
                        ,@(when-let ((extends .extends))
                            `((:include
                               ;; `lsp-structure-type' returns an intern-less
                               ;; `make-symbol'.
                               ,(intern (symbol-name
                                         (lsp-structure-type (aref extends 0)))))))
                        (:copier nil))
           ,@(when (stringp .documentation) (list .documentation))
           ,@(cl-mapcan #'identity
                        (seq-map
                         (lambda (parent)
                           (when-let ((type (intern-soft (lsp-structure parent))))
                             (cl-remove-if-not #'cdr (cl-struct-slot-info type))))
                         (seq-drop .extends 1)))
           ,@(seq-map
              (lambda (alist)
                (let-alist alist
                  `(,(make-symbol (lsp-hyphenate .name))
                    nil
                    :read-only t
                    :type ,(lsp-property-type alist)
                    ,@(when (or .optional .documentation)
                        (list :documentation
                              (string-trim-right
                               (concat (if .optional "Optional. " "")
                                       (or .documentation ""))))))))
              .properties)))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "RenameOptions"
   ;;                                 (alist-get 'name x)))
   ;;              lsp-structures))
   (lsp-top-sort lsp-structures))

  (seq-map
   (lambda (entry)
     (let-alist entry
       (let ((namespace (lsp-enumeration entry)))
         (eval
          `(progn
             ,@(seq-map
                (lambda (entry)
                  (let-alist entry
                    `(defconst ,(intern (lsp-namespace .name namespace))
                       ,.value ,.documentation)))
                .values))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "MarkupKind" (alist-get 'name x)))
   ;;              lsp-enumerations))
   lsp-enumerations)

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(cl-defstruct (,(intern (lsp-request entry))
                        (:copier nil))
           ,@(when (stringp .documentation) (list .documentation))
           (params
            nil
            :read-only t
            :type ,(lsp-structure-type .params))
           ,@(when .registrationOptions
               `((registration-options
                  nil
                  :read-only t
                  :type ,(lsp-structure-type .registrationOptions))))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "workspace/executeCommand" (alist-get 'method x)))
   ;;              lsp-requests))
   lsp-requests)

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(cl-defstruct (,(intern (lsp-notification entry))
                        (:copier nil))
           ,@(when (stringp .documentation) (list .documentation))
           (params
            nil
            :read-only t
            :type ,(lsp-structure-type .params))
           ,@(when .registrationOptions
               `((registration-options
                  nil
                  :read-only t
                  :type ,(lsp-structure-type .registrationOptions))))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "telemetry/event"
   ;;                                 (alist-get 'method x)))
   ;;              lsp-notifications))
   lsp-notifications))

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

(defconst lsp-struct-empty (make-hash-table :size 1))

(defun lsp-jsonify (obj)
  (cond
   ((listp obj) ; lsp-literal qua plist
    obj)
   ((vectorp obj)
    (apply json-array-type
           (seq-map #'lsp-jsonify obj)))
   ((not (get (type-of obj) 'cl--class))
    obj)
   (t
    (let* ((json-object-type 'plist)
           (result (json-new-object))
           (type (type-of obj))
           (slots (cl-remove-if-not #'cdr (cl-struct-slot-info type))))
      (dolist (slot (mapcar (lambda (x) (symbol-name (car x))) slots))
        (when-let ((getter (intern-soft (concat (symbol-name type) "-" slot)))
                   (getter-p (fboundp getter))
                   (value (funcall getter obj)))
          (setq result (json-add-to-object
                        result (lsp-unhyphenate slot t) (lsp-jsonify value)))))
      (or result lsp-struct-empty)))))

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

(cl-defgeneric lsp-handle-request (conn method &rest params))
(cl-defgeneric lsp-handle-notification (conn method &rest params))
(cl-defmethod lsp-handle-notification (_conn _method &key &allow-other-keys)
  "Handle unknown notification.")

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
         (adapter (lambda (fn-using-keys)
                    (lambda (conn method plist)
                      "Too late to change jsonrpc's dispatcher signature."
                      (apply fn-using-keys conn method plist))))
         (command (or (alist-get mode lsp-invocations) "false"))
         (conn (make-instance
                'jsonrpc-process-connection
                :name name
                :events-buffer-scrollback-size lsp-events-buffer-size
                :notification-dispatcher (funcall adapter #'lsp-handle-notification)
                :request-dispatcher (funcall adapter #'lsp-handle-request)
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
                          (jsonrpc-async-request
                           conn
                           'workspace/didChangeConfiguration
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
