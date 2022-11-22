;;; xlsp.el --- Language Server Protocol client. -*- lexical-binding: t; -*-

;; Authors: dick        <dickie.smalls@commandlinesystems.com>
;; URL: https://github.com/commercial-emacs/xlsp
;; Version: 0.0.1pre
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (company "0.9.13") (markdown-mode "2.6.1"))

;;; Commentary:

;; Overview
;; --------
;; M-x xlsp-mode        toggles for the current buffer
;; M-x global-xlsp-mode toggles globally
;; C-M-i                completion (disambiguate then TAB to complete)
;; M-.                  jump to definition
;; M-,                  jump back from definition
;; C-u M-.              prompt jump to definition (TAB to complete)
;;
;; .emacs or init.el configuration
;; -------------------------------
;; (add-hook 'c-mode-hook #'xlsp-mode) ; activate for c-mode
;; -or-
;; (global-xlsp-mode) ; activate for any mode in xlsp-server-invocations
;;
;; Being hip to the youth
;; ----------------------
;; M-x customize-option RET xlsp-completion-menus-p
;; M-x customize-option RET xlsp-hover-help-p
;; ...
;; and other activations which it's hard to imagine are tolerable.
;;
;; "Nothing is happening."
;; -----------------------
;; M-x customize-option RET xlsp-server-invocations RET
;;
;; What the "x" in "xlsp" references
;; ---------------------------------
;; It is merely a differentiator, just as the "x" in "xemacs" had been
;; (contrary to the popular misconception that it referenced X11).
;;
;; Credits
;; -------
;; Logic, some transcribed verbatim, was patterned after GNU eglot.
;; Functions missing in emacs-27 and emacs-28 transcribed from GNU emacs-29.

;;; Code:

(require 'filenotify)
(require 'project)
(require 'xlsp-handle-request)
(require 'xlsp-handle-notification)
(require 'xlsp-company)
(require 'xlsp-server)
(require 'xlsp-xref)

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar xlsp-synchronize-closure)
    (defvar xlsp-did-change-text-document)
    (defvar xlsp-completion-at-point)
    (defvar xlsp-did-save-text-document)
    (defvar xlsp-did-open-text-document)
    (defvar xlsp-did-close-text-document)
    (defvar minibuffer-default-prompt-format)
    (define-derived-mode lisp-data-mode prog-mode "Lisp-Data"
      "Major mode for buffers holding data written in Lisp syntax."
      :group 'lisp
      (lisp-mode-variables nil t nil)
      (setq-local electric-quote-string t)
      (setq imenu-case-fold-search nil))
    (gv-define-expander plist-get
      (lambda (do plist prop)
        (macroexp-let2 macroexp-copyable-p key prop
          (gv-letplace (getter setter) plist
            (macroexp-let2 nil p `(cdr (plist-member ,getter ,key))
              (funcall do
                       `(car ,p)
                       (lambda (val)
                         `(if ,p
                              (setcar ,p ,val)
                            ,(funcall setter `(cons ,key (cons ,val ,getter)))))))))))
    (defmacro project-root (project)
      (with-suppressed-warnings ((obsolete project-roots))
        `(car (project-roots ,project))))
    (defmacro project-buffers (project)
      `(let ((root (expand-file-name (file-name-as-directory
                                      (project-root ,project)))))
         (nreverse
          (cl-loop for b in (buffer-list)
                   for dd = (expand-file-name (buffer-local-value
                                               'default-directory b))
                   when (string-prefix-p root dd)
                   collect b)))))
  (when (< emacs-major-version 29)
    (defmacro with-undo-amalgamate (&rest body)
      "Like `progn' but perform BODY with amalgamated undo barriers.

This allows multiple operations to be undone in a single step.
When undo is disabled this behaves like `progn'."
      (declare (indent 0) (debug t))
      (let ((handle (make-symbol "--change-group-handle--")))
        `(let ((,handle (prepare-change-group))
               ;; Don't truncate any undo data in the middle of this,
               ;; otherwise Emacs might truncate part of the resulting
               ;; undo step: we want to mimic the behavior we'd get if the
               ;; undo-boundaries were never added in the first place.
               (undo-outer-limit nil)
               (undo-limit most-positive-fixnum)
               (undo-strong-limit most-positive-fixnum))
           (unwind-protect
               (progn
                 (activate-change-group ,handle)
                 ,@body)
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))))))
    (defun seq-keep (function sequence)
      "Apply FUNCTION to SEQUENCE and return all non-nil results."
      (delq nil (seq-map function sequence)))))

;; Commercial Emacs
(declare-function tree-sitter-node-type "tree-sitter")
(declare-function tree-sitter-node-at "tree-sitter")
(declare-function c-indent-line "cc-cmds")

(defcustom xlsp-hover-help-p nil
  "Hover help in echo area.  Overridden by global-eldoc-mode."
  :group 'xlsp
  :type 'boolean)

(defcustom xlsp-completion-menus-p nil
  "Graphical completion menus.  Overridden by global-company-mode."
  :group 'xlsp
  :type 'boolean)

(defmacro xlsp-advise-tag (variable)
  `(symbol-name ',variable))

(cl-defmethod initialize-instance ((conn xlsp-connection) _slots)
  (cl-call-next-method)
  (let ((xlsp-advise-sentinel
         (lambda (f proc change &rest args)
           "jsonrpc--message should not usurp echo area."
           (cl-letf (((symbol-function 'jsonrpc--message) #'ignore))
             ;; inhibit-message fixed in 60df437 in Commercial
             (prog1 (apply f proc change args)
               (when-let ((conn (process-get proc 'jsonrpc-connection))
                          (died-p (not (process-live-p proc))))
                 (display-warning
                  'xlsp
                  (format "%s, %s" (process-name proc) change)
                  :warning)))))))
    (add-function :around (process-sentinel (jsonrpc--process conn))
                  xlsp-advise-sentinel
                  `((name . ,(xlsp-advise-tag xlsp-advise-sentinel))))))

(defvar xlsp--connections nil
  "Global alist of ((MODE . INODE-NUM) . XLSP-CONNECTION).
I use inode in case project directory gets renamed.")

(defvar xlsp--inode-data (make-hash-table)
  "Every time you `xlsp-inode', make a backref to original directory.")

(defsubst xlsp-conn-string (mode project-dir)
  (format "%s/%s" mode (file-name-nondirectory
                        (directory-file-name project-dir))))

(defun xlsp-inode (directory)
  (when-let ((inode (file-attribute-inode-number
                     (file-attributes (expand-file-name directory)))))
    (prog1 inode
      (puthash inode directory xlsp--inode-data))))

(defmacro xlsp-gv-connection (conn-key)
  `(alist-get ,conn-key xlsp--connections nil nil #'equal))

(defmacro with-xlsp-connection (args buffer &rest body)
  (declare (indent 2))
  (cl-destructuring-bind (conn-key project-dir)
      args
    `(when-let ((mode (buffer-local-value 'major-mode ,buffer))
                (file-name (buffer-file-name ,buffer))
                (inode
                 (xlsp-inode (expand-file-name
                              (if-let ((project (project-current nil file-name)))
                                  (project-root project)
                                (if (file-directory-p file-name)
                                    file-name
                                  (file-name-directory file-name))))))
                (,project-dir (gethash inode xlsp--inode-data))
                (,conn-key (cons mode inode)))
       ,@body)))

(defmacro xlsp-capability (conn &rest methods)
  "We want to catch incorrect METHODS at compile-time."
  (declare (indent defun))
  (let ((sequence 'result))
    (mapc (lambda (method)
            (setq sequence (list sequence))
            (push method sequence))
          methods)
    `(when-let ((result (oref ,conn capabilities)))
       (ignore-errors ,sequence))))

(defmacro xlsp-sync-p (conn field)
  "Sync kind exists and is not 0."
  `(when-let ((kind (xlsp-sync-kind ,conn ,field)))
     (not (eq kind xlsp-text-document-sync-kind/none))))

(defmacro xlsp-sync-kind (conn field)
  "Return value of sync kind."
  `(when-let ((sync (xlsp-capability ,conn
                      xlsp-struct-server-capabilities-text-document-sync)))
     (if (recordp sync)
         (funcall ',field sync)
       sync)))

(defun xlsp-connection-remove (buffer)
  (with-xlsp-connection (conn-key project-dir)
      buffer
    (xlsp--connection-destroy conn-key project-dir)))

(defun xlsp-connection-get (buffer)
  (with-xlsp-connection (conn-key project-dir)
      buffer
    (when-let ((ret (or (xlsp-gv-connection conn-key)
                        (setf (xlsp-gv-connection conn-key)
                              (xlsp--connect buffer project-dir)))))
      (prog1 ret
        (cl-pushnew buffer (oref ret buffers))))))

(defun xlsp-connection-reset (buffer)
  (xlsp-connection-remove buffer)
  (xlsp-connection-get buffer))

(defun xlsp--initialization-options (project-dir)
  (ignore project-dir))

(cl-defstruct xlsp-synchronize-state
  "Keep shit together."
  (buffer nil :type buffer)
  (version 0 :type integer)
  (events nil :type (list-of xlsp-literal))
  (timer nil :type vector))

(cl-defstruct xlsp-completion-state
  "Frameworks generally give structure.  Company-mode does not."
  (beg nil :type integer)
  (prefix nil :type string)
  (cache-p nil :type boolean)
  (cached-texts nil :type list)
  (kinds nil :type list)
  (details nil :type list)
  (trigger-char nil :type character)
  (additionses nil :type list)
  (index-of (make-hash-table :test #'equal) :type hash-table))

(defalias 'xlsp-synchronize-closure
  (lambda ()
    (xlsp-get-closure
      xlsp-synchronize-closure
      (apply-partially
       (cl-function
        (lambda (synchronize* &key cumulate send save version &allow-other-keys)
          "SYNCHRONIZE* is naturally thread unsafe state."
          (when cumulate
            (push cumulate
                  (xlsp-synchronize-state-events synchronize*))
            (cl-incf (xlsp-synchronize-state-version synchronize*))
            (when (and (not (xlsp-synchronize-state-timer synchronize*))
                       (not send))
              (setf (xlsp-synchronize-state-timer synchronize*)
                    (run-with-idle-timer
                     0.8 nil
                     (apply-partially
                      (lambda (buffer*)
                        "Assume no context switch to :cumulate during :send."
                        (with-current-buffer buffer*
                          (funcall xlsp-synchronize-closure :send t)
                          (setf (xlsp-synchronize-state-timer synchronize*) nil)))
                      (xlsp-synchronize-state-buffer synchronize*))))))
          (when send
            (when (xlsp-synchronize-state-timer synchronize*)
              (cancel-timer (xlsp-synchronize-state-timer synchronize*)))
            (when-let ((conn (xlsp-connection-get
                              (xlsp-synchronize-state-buffer synchronize*)))
                       (kind (xlsp-sync-kind
                              conn xlsp-struct-text-document-sync-options-change))
                       (changes
                        (cond ((eq kind xlsp-text-document-sync-kind/none) nil)
                              ((eq kind xlsp-text-document-sync-kind/incremental)
                               (apply #'vector
                                      (nreverse
                                       (copy-sequence
                                        (xlsp-synchronize-state-events synchronize*)))))
                              (t (let ((full-text
                                        (with-current-buffer
                                            (xlsp-synchronize-state-buffer synchronize*)
                                          (save-restriction
                                            (widen)
                                            (buffer-substring-no-properties
                                             (point-min) (point-max))))))
                                   (vector (xlsp-literal :text full-text)))))))
              (when (cl-plusp (length changes))
                (jsonrpc-notify     ; notifications are fire-and-forget
                 conn
                 xlsp-notification-text-document/did-change
                 (xlsp-jsonify
                  (make-xlsp-struct-did-change-text-document-params
                   :text-document
                   (make-xlsp-struct-versioned-text-document-identifier
                    :uri (xlsp-urify
                          (buffer-file-name
                           (xlsp-synchronize-state-buffer synchronize*)))
                    :version (xlsp-synchronize-state-version synchronize*))
                   :content-changes
                   (prog1 changes
                     ;; Events sent; clear for next batch.
                     (setf (xlsp-synchronize-state-events synchronize*) nil))))))))
          (when save
            (jsonrpc-notify
             (xlsp-connection-get (xlsp-synchronize-state-buffer synchronize*))
             xlsp-notification-text-document/did-save
             (xlsp-jsonify
              (make-xlsp-struct-did-save-text-document-params
               :text-document
               (make-xlsp-struct-versioned-text-document-identifier
                :uri (xlsp-urify
                      (buffer-file-name
                       (xlsp-synchronize-state-buffer synchronize*)))
                :version (xlsp-synchronize-state-version synchronize*))))))
          (when version
            (xlsp-synchronize-state-version synchronize*))))
       (make-xlsp-synchronize-state :buffer (current-buffer))))))

(defmacro xlsp-sync-then-request (buffer &rest args)
  `(progn
     (when-let ((conn (xlsp-connection-get ,buffer)))
       (dolist (b (oref conn buffers))
         (funcall (with-current-buffer b (xlsp-synchronize-closure)) :send t)))
     (funcall (function ,(if (memq :success-fn args)
                             'jsonrpc-async-request
                           'jsonrpc-request))
              ,@args)))

(defsubst xlsp-new-text (item)
  (or (when-let ((text-edit
                  (xlsp-struct-completion-item-text-edit item)))
        (xlsp-struct-text-edit-new-text text-edit))
      (xlsp-struct-completion-item-filter-text item)
      (xlsp-struct-completion-item-label item)))

(defalias 'xlsp-default-completion-filter
  (lambda (query items)
    (cl-loop for item in items
             when (string-prefix-p query (xlsp-new-text item))
             collect item)))

(defvar xlsp-completion-filter-function (symbol-function 'xlsp-default-completion-filter)
  "Accepts QUERY (prefix) and ITEMS.  Returns filtered ITEMS.")

(defalias 'xlsp-flx-completion-filter-function
  (lambda (query items)
    "By Lewang."
    (cl-flet ((flx-regexp (query)
                (let ((breakdown-str
                       (mapcar (lambda (c)
                                 (apply #'string c (when (= (downcase c) c)
                                                     (list (upcase c)))))
                               query)))
                  (concat (format "[%s]" (nth 0 breakdown-str))
                          (mapconcat (lambda (c)
                                       (format "[^%s]*[%s]" c c))
                                     (cdr breakdown-str) "")))))
      (let ((re (flx-regexp query)))
        (seq-keep
         (lambda (item)
           (when-let ((text (xlsp-new-text item)))
             (when (string-match-p re text)
               item)))
         items)))))

(defun xlsp-format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (require 'markdown-mode)
  (defvar markdown-fontify-code-blocks-natively)
  (pcase-let ((`(,string ,mode)
               (if (stringp markup)
                   (list markup 'gfm-view-mode)
                 (list (xlsp-struct-markup-content-value markup)
                       (pcase (xlsp-struct-markup-content-kind markup)
                         ((pred (equal xlsp-markup-kind/markdown)) 'gfm-view-mode)
                         ((pred (equal xlsp-markup-kind/plain-text)) 'text-mode)
                         (_ major-mode))))))
    (when (stringp string)
      (with-temp-buffer
        (setq-local markdown-fontify-code-blocks-natively t)
        (insert string)
        (let ((inhibit-message t)
	      message-log-max)
          (ignore-errors (delay-mode-hooks (funcall mode))))
        (font-lock-ensure)
        (string-trim (buffer-string))))))

(defun xlsp-do-request-signature-help (cache* buffer eldoc-cb)
  "Implicitly called by eldoc machinery which passes in ELDOC-CB.
ELDOC-CB takes a docstring, and optionally bespoke key-value
pairs for its frontends."
  (let ((heuristic-target (with-current-buffer buffer
                            (thing-at-point 'symbol))))
    (if (eq (car cache*) heuristic-target)
        (funcall eldoc-cb (cdr cache*))
      (when-let ((conn (xlsp-connection-get buffer))
                 (params (make-xlsp-struct-signature-help-params
                          :text-document (make-xlsp-struct-text-document-identifier
                                          :uri (xlsp-urify (concat (buffer-file-name buffer))))
                          :position (let ((their-pos (xlsp-their-pos buffer (point))))
                                      (make-xlsp-struct-position
                                       :line (car their-pos)
                                       :character (cdr their-pos)))
                          :context (make-xlsp-struct-signature-help-context
                                    :trigger-kind
                                    (if (and (eq last-command 'self-insert-command)
                                             (characterp last-command-event)
                                             (member (char-to-string last-command-event)
                                                     (append (xlsp-capability
                                                               conn xlsp-struct-server-capabilities-signature-help-provider
                                                               xlsp-struct-signature-help-options-trigger-characters)
                                                             nil)))
                                        xlsp-signature-help-trigger-kind/trigger-character
                                      xlsp-signature-help-trigger-kind/content-change)))))
        (xlsp-sync-then-request
         buffer conn
         xlsp-request-text-document/signature-help
         (xlsp-jsonify params)
         :success-fn
         (lambda (result-plist)
           (when-let ((help
                       (xlsp-unjsonify 'xlsp-struct-signature-help result-plist))
                      (formatify
                       (apply-partially
                        (cl-function
                         (lambda (i* sig
                                  &aux
                                  (sig-active
                                   (xlsp-struct-signature-help-active-signature help))
                                  (documentation
                                   (xlsp-struct-signature-information-documentation sig))
                                  (label
                                   (xlsp-struct-signature-information-label sig))
                                  (params
                                   (xlsp-struct-signature-information-parameters sig))
                                  (param-active
                                   (or (xlsp-struct-signature-information-active-parameter sig)
                                       (xlsp-struct-signature-help-active-parameter help)))
                                  (i (prog1 i* (cl-incf i*)))
                                  formals-beg formals-end)
                           (when (string-match "\\([^(]+\\)(\\([^)]+\\))" label)
                             ;; Ad-hoc attempt to parse label as <name>(<args>)
                             (setq formals-beg (match-beginning 2)
                                   formals-end (match-end 2))
                             (add-face-text-property (match-beginning 1) (match-end 1)
                                                     'font-lock-function-name-face
                                                     nil label))
                           (when (eql i sig-active)
                             ;; Processing the "active signature".
                             (when-let ((doc-p (stringp documentation))
                                        (match-p (string-match
                                                  "[[:space:]]*\\([^.\r\n]+[.]?\\)"
                                                  documentation))
                                        (doc-match (match-string 1 documentation)))
                               ;; Add one-line-summary to signature line
                               (unless (string-prefix-p (string-trim doc-match) label)
                                 (setq label (concat label ": "
                                                     (xlsp-format-markup doc-match)))))
                             (when-let ((formals-p formals-beg)
                                        (formals (cl-subseq label formals-beg formals-end))
                                        (param-p (fixnump param-active))
                                        (param (aref params (min (1- (length params))
                                                                 param-active)))
                                        (param-label (xlsp-struct-parameter-information-label
                                                      param)))
                               ;; Highlight the active one of the args.
                               (when-let ((beg-end
                                           (if (stringp param-label)
                                               (let (case-fold-search)
                                                 (when (string-match
                                                        (format "\\<%s\\>"
                                                                (regexp-quote param-label))
                                                        formals)
                                                   (cons (+ formals-beg (match-beginning 0))
                                                         (+ formals-beg (match-end 0)))))
                                             (cons (aref param-label 0)
                                                   (aref param-label 1)))))
                                 (add-face-text-property
                                  (car beg-end) (cdr beg-end)
                                  'eldoc-highlight-function-argument
                                  nil label))
                               ;; Add its doc on its own line.
                               (when-let ((param-doc
                                           (xlsp-struct-parameter-information-documentation
                                            param)))
                                 (setq label
                                       (concat
                                        label "\n"
                                        (propertize
                                         (if (stringp param-label)
                                             param-label
                                           (apply #'cl-subseq label
                                                  (append param-label nil)))
                                         'face 'eldoc-highlight-function-argument)
                                        ": " (xlsp-format-markup param-doc))))))
                           label))
                        ;; closure I*
                        0))
                      (formatted (mapconcat formatify
                                            (xlsp-struct-signature-help-signatures help)
                                            "\n")))
             (setcar cache* heuristic-target)
             (setcdr cache* formatted)
             (funcall eldoc-cb formatted)))
         :error-fn
         (lambda (error)
           (xlsp-message "xlsp-do-request-signature-help: %s (%s)"
                         (plist-get error :message)
                         (plist-get error :code))))))))

(defun xlsp-do-request-hover (cache* sync* buffer eldoc-cb)
  "Implicitly called by eldoc machinery which passes in ELDOC-CB.
ELDOC-CB takes a docstring, and optionally bespoke key-value
pairs for its frontends.  The eldoc situation got even messier
in v28, if that were possible, and a SYNC* value of true
retrofits current logic to v27."
  (let ((heuristic-target (with-current-buffer buffer
                            (thing-at-point 'symbol)))
        (eldoc-cb-args '(:buffer t)))
    (if (equal (car cache*) heuristic-target)
        (apply eldoc-cb (cdr cache*) eldoc-cb-args)
      (let* ((conn (xlsp-connection-get buffer))
             (params (make-xlsp-struct-hover-params
                      :text-document (make-xlsp-struct-text-document-identifier
                                      :uri (xlsp-urify (concat (buffer-file-name buffer))))
                      :position (let ((their-pos (xlsp-their-pos buffer (point))))
                                  (make-xlsp-struct-position
                                   :line (car their-pos)
                                   :character (cdr their-pos)))))
             (success-fn
              (lambda (result-plist)
                (when-let ((hover
                            (xlsp-unjsonify 'xlsp-struct-hover result-plist))
                           (hover-contents (xlsp-struct-hover-contents hover))
                           (markups
                            (let ((contents hover-contents))
                              (unless (vectorp contents)
                                (setq contents (vector contents)))
                              (mapcar
                               (lambda (content)
                                 "MarkedString = string | {language:string; value:string}"
                                 (if (and (listp content)
                                          (plist-get content :language)
                                          (plist-get content :value))
                                     (mapconcat #'identity
                                                (cl-remove-if-not #'stringp content)
                                                "\n")
                                   content))
                               contents)))
                           (formatted
                            (mapconcat #'identity
                                       (seq-keep #'xlsp-format-markup markups) "\n")))
                  (setcar cache* heuristic-target)
                  (setcdr cache* formatted)
                  (apply eldoc-cb formatted eldoc-cb-args))))
             (error-fn
              (lambda (error)
                (xlsp-message "xlsp-do-request-hover: %s (%s)"
                              (plist-get error :message)
                              (plist-get error :code)))))
        (if sync*
            (funcall success-fn
                     (xlsp-sync-then-request
                      buffer conn xlsp-request-text-document/hover
                      (xlsp-jsonify params)))
          (xlsp-sync-then-request
           buffer conn xlsp-request-text-document/hover
           (xlsp-jsonify params)
           :success-fn success-fn :error-fn error-fn))))))

(cl-defun xlsp-do-request-completion (buffer pos callback
                                      &key trigger-char sync)
  "Note SYNC argument and qq/sync/ in xlsp-sync-then-request are distinct.
The first says whether to jsonrpc-async-request or jsonrpc-request.
The second refers to LSP document synchronization."
  (when-let ((conn (xlsp-connection-get buffer))
             (params (make-xlsp-struct-completion-params
                      :text-document (make-xlsp-struct-text-document-identifier
                                      :uri (xlsp-urify (concat (buffer-file-name buffer))))
                      :position (let ((their-pos (xlsp-their-pos buffer pos)))
                                  (make-xlsp-struct-position
                                   :line (car their-pos)
                                   :character (cdr their-pos)))
                      :context (make-xlsp-struct-completion-context
                                :trigger-kind
                                (if trigger-char
                                    xlsp-completion-trigger-kind/trigger-character
                                  xlsp-completion-trigger-kind/invoked)
                                :trigger-character
                                (when trigger-char (char-to-string trigger-char))))))
    (if sync
        (let ((result-plist (xlsp-sync-then-request
                             buffer conn
                             xlsp-request-text-document/completion
                             (xlsp-jsonify params))))
          (funcall callback (xlsp-unjsonify 'xlsp-struct-completion-list result-plist)))
      (xlsp-sync-then-request
       buffer conn
       xlsp-request-text-document/completion
       (xlsp-jsonify params)
       :success-fn
       (lambda (result-plist)
         (funcall callback (xlsp-unjsonify 'xlsp-struct-completion-list result-plist)))
       :error-fn
       (lambda (error)
         (xlsp-message "xlsp-do-request-completion: %s (%s)"
                       (plist-get error :message)
                       (plist-get error :code)))))))

(defun xlsp-do-request-definition (buffer pos)
  "This retrieves a location, not a definition."
  (when-let ((conn (xlsp-connection-get buffer))
             (params (make-xlsp-struct-definition-params
                      :text-document (make-xlsp-struct-text-document-identifier
                                      :uri (xlsp-urify (concat (buffer-file-name buffer))))
                      :position (let ((their-pos (xlsp-their-pos buffer pos)))
                                  (make-xlsp-struct-position
                                   :line (car their-pos)
                                   :character (cdr their-pos)))))
             (result-array (xlsp-sync-then-request
                            buffer conn
                            xlsp-request-text-document/definition
                            (xlsp-jsonify params))))
    (unless (vectorp result-array)
      (setq result-array (vector result-array)))
    ;; first attempt parsing as Location[] then as LocationLink[]
    (when-let ((locations
                (seq-map (apply-partially #'xlsp-unjsonify 'xlsp-struct-location)
                         result-array)))
      (unless (xlsp-struct-location-uri (car locations))
        (setq locations (seq-map (apply-partially #'xlsp-unjsonify
                                                  'xlsp-struct-location-link)
                                 result-array)))
      locations)))

(defsubst xlsp-formatting-options (buffer)
  (defvar c-basic-offset)
  (with-current-buffer buffer
    (make-xlsp-struct-formatting-options
     :tab-size (if (and (eq indent-line-function #'c-indent-line)
                        (fixnump c-basic-offset))
                   c-basic-offset
                 tab-width)
     :insert-spaces (if indent-tabs-mode :json-false t)
     :trim-final-newlines
     ;; not serious
     (if delete-trailing-lines
         t
       :json-false)
     :trim-trailing-whitespace
     ;; not serious
     (if (memq 'delete-trailing-whitespace before-save-hook)
         t
       :json-false)
     :insert-final-newline
     (if require-final-newline
         t
       :json-false))))

(defun xlsp-do-request-formatting (buffer)
  (let* ((conn (xlsp-connection-get buffer))
         (params (make-xlsp-struct-document-formatting-params
                  :text-document (make-xlsp-struct-text-document-identifier
                                  :uri (xlsp-urify (concat (buffer-file-name buffer))))
                  :options (xlsp-formatting-options buffer)))
         (result-array (xlsp-sync-then-request
                        buffer conn
                        xlsp-request-text-document/formatting
                        (xlsp-jsonify params))))
    (when-let ((edits
                (seq-map (apply-partially #'xlsp-unjsonify 'xlsp-struct-text-edit)
                         result-array)))
      (xlsp-apply-text-edits edits))))

(defun xlsp-do-request-range-formatting (buffer beg end)
  (let* ((conn (xlsp-connection-get buffer))
         (params (make-xlsp-struct-document-range-formatting-params
                  :text-document (make-xlsp-struct-text-document-identifier
                                  :uri (xlsp-urify (concat (buffer-file-name buffer))))
                  :range (make-xlsp-struct-range
                          :start (let ((their-pos (xlsp-their-pos buffer beg)))
                                   (make-xlsp-struct-position
                                    :line (car their-pos)
                                    :character (cdr their-pos)))
                          :end (let ((their-pos (xlsp-their-pos buffer end)))
                                 (make-xlsp-struct-position
                                  :line (car their-pos)
                                  :character (cdr their-pos))))
                  :options (xlsp-formatting-options buffer)))
         (result-array (xlsp-sync-then-request
                        buffer conn
                        xlsp-request-text-document/range-formatting
                        (xlsp-jsonify params))))
    (when-let ((edits
                (seq-map (apply-partially #'xlsp-unjsonify 'xlsp-struct-text-edit)
                         result-array)))
      (xlsp-apply-text-edits edits))))

(defun xlsp-do-request-on-type-formatting (buffer pos on-character)
  "LSP calls this qq/on type formatting/ where type is a verb.
Usually nouns follow prepositions (qq/on/), and given types as
nouns, e.g., variable types, are so prevalent in software, the
naming is fairly egregious."
  (let* ((conn (xlsp-connection-get buffer))
         (params (make-xlsp-struct-document-on-type-formatting-params
                 :text-document (make-xlsp-struct-text-document-identifier
                                 :uri (xlsp-urify (concat (buffer-file-name buffer))))
                 :position (let ((their-pos (xlsp-their-pos buffer pos)))
                             (make-xlsp-struct-position
                              :line (car their-pos)
                              :character (cdr their-pos)))
                 :ch (char-to-string on-character)
                 :options (xlsp-formatting-options buffer)))
         (result-array (xlsp-sync-then-request
                        buffer conn
                        xlsp-request-text-document/on-type-formatting
                        (xlsp-jsonify params))))
    (when-let ((edits
                (seq-map (apply-partially #'xlsp-unjsonify 'xlsp-struct-text-edit)
                         result-array)))
      (xlsp-apply-text-edits edits))))

(defun xlsp-apply-text-edits (edits)
  (let ((tick (buffer-chars-modified-tick)))
    (condition-case err
        (with-undo-amalgamate
          (dolist (edit (reverse (append edits nil)))
            (let* ((new-text (xlsp-struct-text-edit-new-text edit))
                   (range (xlsp-struct-text-edit-range edit))
                   (beg (xlsp-our-pos (current-buffer)
                                      (xlsp-struct-range-start range)))
                   (end (xlsp-our-pos (current-buffer)
                                      (xlsp-struct-range-end range))))
              (replace-region-contents
               beg end (apply-partially #'identity new-text)))))
      (error (let ((inhibit-message t))
               (unless (= (buffer-chars-modified-tick) tick)
                 (undo-boundary)
                 (undo-only)))
             (message "xlsp-apply-text-edits: %s"
                      (error-message-string err))))))

(defcustom xlsp-events-buffer-size (truncate 2e6)
  "Events buffer max size.  Zero for no buffer, nil for infinite."
  :group 'xlsp
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Number of characters")))

(defun xlsp-message (format &rest args)
  "Discrete logging."
  (let ((lhs (format-time-string "%H%M%S.%3N [lsp] " (current-time)))
        (inhibit-message t)) ; but echo area still gets cleared... bad.
    (apply #'message (concat lhs format) args)))

(defun xlsp-do-request-workspace-symbols (buffer query)
  (when-let ((conn (xlsp-connection-get buffer))
             (params (make-xlsp-struct-workspace-symbol-params
                      :query query))
             (result-array (xlsp-sync-then-request
                            buffer conn
                            xlsp-request-workspace/symbol
                            (xlsp-jsonify params))))
    (seq-map (apply-partially
              #'xlsp-unjsonify
              'xlsp-struct-workspace-symbol)
             result-array)))

(defvar-local xlsp-hooks-alist nil
  "Alist entries of the form (HOOKS-SYM . HOOK-COMPILED-FUNC).
Hooks must be tracked since closures ruin remove-hook's #'equal criterion.")

(defun xlsp-completion-callback (state* buffer* cb* completion-list)
  "The interval [BEG, END) spans the region supplantable by
CANDIDATES."
  (if-let ((items
            (append (xlsp-struct-completion-list-items completion-list) nil))
           (beg-end (if-let ((has-text-edit
                              (seq-find #'xlsp-struct-completion-item-text-edit
                                        items))
                             (range (xlsp-struct-text-edit-range
                                     (xlsp-struct-completion-item-text-edit
                                      has-text-edit))))
                        ;; Good, server has specific range info.
                        (cons (xlsp-our-pos buffer* (xlsp-struct-range-start range))
                              (xlsp-our-pos buffer* (xlsp-struct-range-end range)))
                      ;; Bad, let's hope server agrees with us.
                      (with-current-buffer buffer*
                        (or (bounds-of-thing-at-point 'symbol)
                            `(,(point) . ,(point))))))
           (beg (car beg-end))
           (end (cdr beg-end))
           (extant (with-current-buffer buffer*
                     ;; server often out of sync by design
                     (ignore-errors (buffer-substring-no-properties
                                     beg end))))
           (filtered-items
            (sort
             (funcall
              (or xlsp-completion-filter-function
                  (symbol-function 'xlsp-default-completion-filter))
              extant items)
             (lambda (i1 i2)
               (cond ((not (string-prefix-p extant (xlsp-new-text i1)))
                      i2)
                     ((not (string-prefix-p extant (xlsp-new-text i2)))
                      i1)
                     (t
                      (let ((s1 (xlsp-struct-completion-item-sort-text i1))
                            (s2 (xlsp-struct-completion-item-sort-text i2)))
                        (if (and s1 s2)
                            (string< s1 s2)
                          (not s2))))))))
           (texts (mapcar #'xlsp-new-text filtered-items)))
      (prog1 (funcall cb* texts)
        (setf (xlsp-completion-state-beg state*) beg
              (xlsp-completion-state-prefix state*) extant
              (xlsp-completion-state-cache-p state*) (not (xlsp-struct-completion-list-is-incomplete completion-list))
              (xlsp-completion-state-cached-texts state*) (when (xlsp-completion-state-cache-p state*) texts)
              (xlsp-completion-state-kinds state*) (mapcar #'xlsp-struct-completion-item-kind filtered-items)
              (xlsp-completion-state-details state*) (mapcar #'xlsp-struct-completion-item-detail filtered-items)
              (xlsp-completion-state-additionses state*) (mapcar #'xlsp-struct-completion-item-additional-text-edits filtered-items))
        (with-current-buffer buffer*
          (setq company-prefix (buffer-substring-no-properties
                                beg (max beg (point)))))
        (clrhash (xlsp-completion-state-index-of state*))
        (dotimes (i (length texts))
          (puthash (nth i texts) i
                   (xlsp-completion-state-index-of state*)))
        (when-let ((conn (xlsp-connection-get buffer*))
                   (capable-p (xlsp-capability conn
                                xlsp-struct-server-capabilities-completion-provider
                                xlsp-struct-completion-options-resolve-provider)))
          ;; "By default the request can only delay the
          ;; computation of the detail and documentation
          ;; properties."
          (cl-loop with obeg = (xlsp-completion-state-beg state*)
                   with oprefix = (xlsp-completion-state-prefix state*)
                   for i below (length filtered-items)
                   for pre = (nth i filtered-items)
                   unless (xlsp-struct-completion-item-detail pre)
                   do (jsonrpc-async-request
                       conn xlsp-request-completion-item/resolve
                       (xlsp-jsonify pre)
                       :success-fn
                       (apply-partially
                        (cl-function
                         (lambda (i* result-plist
                                     &aux (post (xlsp-unjsonify 'xlsp-struct-completion-item result-plist)))
                           (when-let
                               ((active-p (with-current-buffer buffer*
                                            company-point))
                                (text (xlsp-new-text post))
                                (stable-beg-p
                                 (eq
                                  obeg
                                  (xlsp-completion-state-beg state*)))
                                (stable-prefix-p
                                 (equal
                                  oprefix
                                  (xlsp-completion-state-prefix state*))))
                             (setf (nth i* (xlsp-completion-state-kinds state*))
                                   (xlsp-struct-completion-item-kind post))
                             (setf (nth i* (xlsp-completion-state-details state*))
                                   (xlsp-struct-completion-item-detail post))
                             (setf (nth i* (xlsp-completion-state-additionses state*))
                                   (xlsp-struct-completion-item-additional-text-edits post))
                             (with-current-buffer buffer*
                               (company-update-candidates company-candidates)
                               (company-call-frontends 'update)
                               (company-call-frontends 'post-command)))))
                        i)))))
    (prog1 (funcall cb* nil)
      (setf state* (make-xlsp-completion-state)))))

(defun xlsp-heuristic-reuse-matches-p (buffer state)
  (with-current-buffer buffer
    (when-let ((beg (xlsp-completion-state-beg state))
               (after-p (>= (point) beg))
               (orig-prefix (xlsp-completion-state-prefix state))
               (matches (xlsp-completion-state-cached-texts state))
               (forward-p (>= (point) (+ beg (length orig-prefix))))
               (prefix (buffer-substring beg (point))))
      (and (string-prefix-p orig-prefix prefix)
           (string-match-p "^[[:alnum:]_]+$" prefix)))))

(define-minor-mode xlsp-mode
  "Start and consult language server if applicable."
  :lighter nil
  :group 'xlsp
  (let* ((completion-state (make-xlsp-completion-state))
         (completion-directive
          (lambda (cb)
            (if (xlsp-heuristic-reuse-matches-p (current-buffer) completion-state)
                (let ((prefix (buffer-substring
                               (xlsp-completion-state-beg completion-state)
                               (point)))
                      (matches (xlsp-completion-state-cached-texts completion-state)))
                  (funcall cb (cl-remove-if-not
                               (apply-partially #'string-prefix-p prefix)
                               matches)))
              (xlsp-do-request-completion
               (current-buffer) (point)
               (apply-partially #'xlsp-completion-callback
                                completion-state (current-buffer) cb)
               :trigger-char (xlsp-completion-state-trigger-char completion-state)))))
         (xlsp-advise-cache
          ;; xlsp has its own cached-texts in completion-state.
          ;; This advice deals with company's cache.
          (lambda (f &rest args)
            "LSP said isIncomplete.  So undo the caching."
            (let ((restore-cache company-candidates-cache))
              (prog1 (apply f args)
                (when xlsp-mode
                  (unless (xlsp-completion-state-cache-p completion-state)
                    (setq company-candidates-cache restore-cache)))))))
         (xlsp-advise-contains
          (lambda (elt cur)
            "Closures contain dotted pairs (symbol . compiled-function)."
            (cl-labels ((recurse (elt cur)
                          (cond ((eq elt cur) t)
                                ((consp cur)
                                 (or (recurse elt (car cur))
                                     (recurse elt (cdr cur))))
                                (t nil))))
              (recurse elt cur))))
         (backend
          (lambda (directive &optional candidate &rest _args)
            (cl-case directive
              (candidates
               (cons :async completion-directive))
              (sorted
               (apply-partially #'identity t))
              (annotation
               (when-let ((index-of (xlsp-completion-state-index-of completion-state))
                          (details (xlsp-completion-state-details completion-state))
                          (detail (nth (gethash candidate index-of) details)))
                 (concat " " (propertize
                              detail 'face 'font-lock-function-name-face))))
              (kind             ; keys in company-vscode-icons-mapping
               (when-let ((index-of (xlsp-completion-state-index-of completion-state))
                          (kinds (xlsp-completion-state-kinds completion-state))
                          (kind (alist-get
                                 (nth (gethash candidate index-of) kinds)
                                 xlsp-company-kind-alist)))
                 (intern-soft (downcase kind))))
              (post-completion
               (when-let ((index-of (xlsp-completion-state-index-of completion-state))
                          (additionses (xlsp-completion-state-additionses completion-state))
                          (additions (nth (gethash candidate index-of) additionses)))
                 (xlsp-apply-text-edits additions)))
              (prefix
               ;; We use the "prefix" directive only to predict whether
               ;; issuing a candidates call will be fruitful.  Only
               ;; the server knows what the prefix really is.
               (unless (cond ((derived-mode-p 'tree-sitter-prog-mode)
                              (cl-case (when-let ((type (tree-sitter-node-type
                                                         (tree-sitter-node-at))))
                                         (intern type))
                                ((comment string) t)))
                             ((derived-mode-p 'prog-mode)
                              (company-in-string-or-comment))
                             (t nil))
                 (let ((word (company-grab-symbol)))
                   (if-let ((conn (xlsp-connection-get (current-buffer)))
                            (triggers
                             (append
                              (xlsp-capability conn
                                xlsp-struct-server-capabilities-completion-provider
                                xlsp-struct-completion-options-trigger-characters)
                              nil))
                            (trigger-char
                             (car (memq (char-after (max (point-min) (1- (point))))
                                        (mapcar #'string-to-char triggers)))))
                       (cons word
                             (setf (xlsp-completion-state-trigger-char completion-state)
                                   trigger-char))
                     (prog1 word
                       (setf (xlsp-completion-state-trigger-char completion-state)
                             nil))))))))))
    (if xlsp-mode
        (progn
          (add-function :around (symbol-function 'company-calculate-candidates)
                        xlsp-advise-cache
                        `((name . ,(xlsp-advise-tag xlsp-advise-cache))))
          (add-function :override (symbol-function 'company--contains)
                        xlsp-advise-contains
                        `((name . ,(xlsp-advise-tag xlsp-advise-contains))))
          (cl-macrolet ((dont-play
                          (var)
                          `(let ((standard-value
                                  (eval (car (get ',var 'standard-value)))))
                             (unless (eq (symbol-value ',var) standard-value)
                               (setq-local ,var standard-value)))))
            ;; Idle hands and knobs account for emacs's rap for fragility.
            (dont-play company-minimum-prefix-length)
            (dont-play company-tooltip-idle-delay)
            (dont-play company-idle-delay)
            (dont-play completion-styles))
          (unless company-mode
            (when xlsp-completion-menus-p
              (company-mode)))
          (setq-local company-backends (cons backend company-backends)
                      company-lighter (concat " " company-lighter-base))
          (xlsp-toggle-hooks nil) ; cleanse palate even if toggling on
          (when-let ((conn (xlsp-connection-get (current-buffer))))
            (if (jsonrpc-connection-ready-p conn :handshook)
                (progn
                  (xlsp-toggle-hooks conn)
                  (when (xlsp-sync-p
                         conn xlsp-struct-text-document-sync-options-open-close)
                    (funcall (xlsp-did-open-text-document))))
              (ignore "Presume async handshake will install hooks."))))
      ;; those add-functions are forever...
      (when-let ((conn (xlsp-connection-get (current-buffer)))
                 (sync-p (xlsp-sync-p
                          (xlsp-connection-get (current-buffer))
                          xlsp-struct-text-document-sync-options-open-close)))
        ;; things like project-kill-buffers kill the process buffer
        ;; before did-close can attempt notification
        (condition-case err
            (funcall (xlsp-did-close-text-document))
          (error (xlsp-message "xlsp-mode: %s, %s"
                               (buffer-name) (cl-second err)))))
      (xlsp-toggle-hooks nil)
      (xlsp-deregister-buffer (current-buffer))
      (unless global-eldoc-mode
        (eldoc-mode -1))
      (unless global-company-mode
        (company-mode -1))
      (mapc #'kill-local-variable '(company-backends
                                    company-minimum-prefix-length
                                    company-idle-delay
                                    company-tooltip-idle-delay
                                    company-lighter
                                    eldoc-documentation-function
                                    xlsp-synchronize-closure
                                    completion-styles)))))

(defun xlsp--connection-destroy (conn-key project-dir)
  (dolist (b (cl-remove-if-not
              #'buffer-file-name
              (when-let ((proj (project-current nil project-dir)))
                (project-buffers proj))))
    (with-current-buffer b
      (when (equal major-mode (car conn-key))
        (when xlsp-mode
          (xlsp-mode -1)))))

  (let ((conn (xlsp-gv-connection conn-key)))
    (when (and conn
               (jsonrpc-running-p conn)
               (buffer-live-p (jsonrpc--events-buffer conn))
               (buffer-live-p (process-buffer (jsonrpc--process conn))))
      (jsonrpc-async-request conn xlsp-request-shutdown nil)
      ;; eglot got this right by just firing off the exit notification
      ;; sight unseen.  Ideally it'd issue from the success-fn callback
      ;; of xlsp-request-shutdown, but jsonrpc--process-filter
      ;; triggers that callback, which could kill the connection,
      ;; and jsonrpc--process-filter lives in connection's buffer.
      ;; Upshot: Text from arbitrary buffers gets delete-region'ed!!!).
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (jsonrpc-notify conn xlsp-notification-exit nil)))
    ;; Unfortunately this cannot wait because user could
    ;; toggle xlsp-mode very quickly.
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'jsonrpc--message) #'ignore))
      (when conn (jsonrpc-shutdown conn :cleanup)))
    (setq xlsp--connections
          (assoc-delete-all conn-key xlsp--connections))))

(defun xlsp--connect (buffer project-dir)
  (when-let
      ((mode (buffer-local-value 'major-mode buffer))
       (name (xlsp-conn-string mode project-dir))
       (initialize-params
        (make-xlsp-struct-initialize-params
         :process-id (emacs-pid)
         :initialization-options (xlsp--initialization-options project-dir)
         :capabilities xlsp-default-struct-client-capabilities
         :workspace-folders (xlsp-array
                             (make-xlsp-struct-workspace-folder
                              :uri (xlsp-urify project-dir)
                              :name project-dir))))
       (command (alist-get mode xlsp-server-invocations))
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
                  ;; Should process die exogenously,
                  ;; a sentinel in jsonrpc calls :on-shutdown
                  :on-shutdown (with-xlsp-connection (conn-key project-dir)
                                   buffer
                                 (lambda (_conn)
                                   (xlsp--connection-destroy conn-key project-dir)))
                  :process process)
               (error (delete-process process)
                      (signal (car err) (cdr err))))))
    (xlsp-message "%s executing: %s" name command)
    (condition-case err
        (jsonrpc-async-request
         conn
         xlsp-request-initialize
         (xlsp-jsonify initialize-params)
         :success-fn
         (cl-function
          (lambda (result-plist
                   &aux (result (xlsp-unjsonify 'xlsp-struct-initialize-result
                                                result-plist)))
            (if (not (buffer-live-p buffer))
                (xlsp-message "xlsp--connect: %s no longer exists!" buffer)
              (with-current-buffer buffer
                ;; Populate CONN state
                (oset conn ready-p t)
                (oset conn capabilities
                      (xlsp-struct-initialize-result-capabilities result))
                (oset conn server-info
                      (xlsp-struct-initialize-result-server-info result))
                (condition-case err
                    (progn
                      ;; Ack
                      (jsonrpc-notify conn xlsp-notification-initialized
                                      (xlsp-jsonify (make-xlsp-struct-initialized-params)))

                      ;; Register project files
                      (with-xlsp-connection (conn-key project-dir)
                          buffer
                        (dolist (b (cl-remove-if-not
                                    #'buffer-file-name
                                    (when-let ((proj (project-current nil project-dir)))
                                      (project-buffers proj))))
                          (with-current-buffer b
                            (when (equal major-mode (car conn-key))
                              ;; Now that we know capabilities, selectively activate hooks
                              (when xlsp-mode
                                (xlsp-toggle-hooks nil) ; cleanse palate
                                (xlsp-toggle-hooks conn)
                                (funcall (xlsp-did-open-text-document)))))))

                      ;; Register workspace-specific config, if any.
                      ;; Amazingly, pyright won't budge without this.
                      (jsonrpc-notify
                       conn xlsp-notification-workspace/did-change-configuration
                       (xlsp-jsonify
                        (make-xlsp-struct-did-change-configuration-params
                         :settings xlsp-struct-empty))))
                  (error
                   (xlsp-message "success-fn: %s" (error-message-string err))
                   (xlsp-connection-remove buffer)))))))
         :error-fn
         (lambda (error)
           (xlsp-message "%s %s (%s)" name (plist-get error :message)
                         (plist-get error :code))
           (xlsp-connection-remove buffer))
         :timeout
         10
         :timeout-fn
         (lambda ()
           (xlsp-message "%s timed out" name)
           (xlsp-connection-remove buffer)))
      (error
       ;; cleaning up process only happens when forming
       ;; the request had errors, not when callbacks do.
       (jsonrpc-shutdown conn :cleanup)
       (setq conn nil)
       (signal (car err) (cdr err))))
    conn))

(defalias 'xlsp-completion-at-point
  (lambda ()
    (xlsp-get-closure
      xlsp-completion-at-point
      (apply-partially
       (lambda (state*)
         "Return (BEG END BLANDY-MONNIER . _PROPS)"
         (let* ((heuristic-prefix
                 (bounds-of-thing-at-point 'symbol))
                (for-buffer (current-buffer))
                (for-point (point))
                (beg (or (car heuristic-prefix) for-point))
                (end (or (cdr heuristic-prefix) for-point))
                (blandy-monnier
                 ;; BLANDY-MONNIER takes three arguments
                 ;; PREFIX, PREDICATE, NIL-FOR-TRY-ELSE-T.
                 ;; PREFIX is buffer text between [BEG, END).
                 ;; Other values for NIL-FOR-TRY-ELSE-T include
                 ;; 'metadata, 'lambda, and '(boundaries ...),
                 ;; which we disregard mostly out of spite for
                 ;; its hamfisted api and for the felony of
                 ;; minibuffer.el generally.
                 (lambda (prefix pred nil-for-try)
                   (with-current-buffer for-buffer
                     (let* ((matches
                             (if (xlsp-heuristic-reuse-matches-p for-buffer state*)
                                 (xlsp-completion-state-cached-texts state*)
                               (xlsp-do-request-completion
                                for-buffer
                                for-point
                                (apply-partially #'xlsp-completion-callback
                                                 state* for-buffer
                                                 #'identity)
                                :sync t)))
                            (pruned (cl-remove-if-not
                                     (apply-partially #'string-prefix-p prefix)
                                     matches)))
                       (cl-case nil-for-try
                         ((nil)
                          (try-completion prefix pruned))
                         ((t)
                          (all-completions "" pruned pred))))))))
           (list beg end blandy-monnier)))
       (make-xlsp-completion-state)))))

(defalias 'xlsp-did-change-text-document
  (lambda ()
    (xlsp-get-closure
      xlsp-did-change-text-document
      (let* ((state-empty '((character-offset . nil)
                            (line-offset . nil)
                            (snippet . nil)))
             (before-state* state-empty))
        (lambda (beg end &optional deleted)
          "BEFORE-STATE* is naturally thread unsafe."
          (cl-flet ((get-row-col (pos)
                      (let ((their-pos
                             (with-temp-buffer
                               (save-excursion
                                 (insert (alist-get 'snippet before-state*)))
                               (xlsp-their-pos (current-buffer) (min pos (point-max))))))
                        (make-xlsp-struct-position
                         :line (+ (alist-get 'line-offset before-state*)
                                  (car their-pos))
                         :character (cdr their-pos)))))
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
                (funcall (xlsp-synchronize-closure)
                         :cumulate
                         (xlsp-literal
                          :range (xlsp-jsonify
                                  (make-xlsp-struct-range
                                   ;; get-row-col POS is one-indexed
                                   :start (get-row-col pos)
                                   :end (get-row-col (+ pos deleted))))
                          :text (buffer-substring-no-properties beg end))))
              (setq before-state* state-empty))))))))

(defalias 'xlsp-did-save-text-document
  (lambda ()
    (xlsp-get-closure
      xlsp-did-save-text-document
      (lambda () (funcall (xlsp-synchronize-closure) :save t)))))

(defalias 'xlsp-did-open-text-document
  (lambda ()
    (xlsp-get-closure
      xlsp-did-open-text-document
      (apply-partially
       (lambda (buffer*)
         (jsonrpc-notify
          (xlsp-connection-get buffer*)
          xlsp-notification-text-document/did-open
          (xlsp-jsonify
           (make-xlsp-struct-did-open-text-document-params
            :text-document (make-xlsp-struct-text-document-item
                            :uri (xlsp-urify (buffer-file-name buffer*))
                            :language-id ""
                            :version (funcall (xlsp-synchronize-closure) :version t)
                            :text (with-current-buffer buffer*
                                    (save-restriction
                                      (widen)
                                      (buffer-substring-no-properties
                                       (point-min) (point-max)))))))))
       (current-buffer)))))

(defalias 'xlsp-did-close-text-document
  (lambda ()
    (xlsp-get-closure
      xlsp-did-close-text-document
      (apply-partially
       (lambda (buffer*)
         (jsonrpc-notify
          (xlsp-connection-get buffer*)
          xlsp-notification-text-document/did-close
          (xlsp-jsonify
           (make-xlsp-struct-did-close-text-document-params
            :text-document (make-xlsp-struct-text-document-item
                            :uri (xlsp-urify (buffer-file-name buffer*))
                            :language-id ""
                            :version (funcall (xlsp-synchronize-closure) :version t)
                            :text (with-current-buffer buffer*
                                    (save-restriction
                                      (widen)
                                      (buffer-substring-no-properties
                                       (point-min) (point-max)))))))))
       (current-buffer)))))

(defun xlsp-deregister-buffer (buffer)
  "Dispose connection if last registered buffer."
  (when-let ((conn (xlsp-connection-get buffer)))
    (oset conn buffers (delq buffer
                             (cl-remove-if-not
                              #'buffer-live-p
                              (oref conn buffers))))
    (unless (oref conn buffers)
      (xlsp-connection-remove buffer))))

(defalias 'xlsp-toggle-hooks
  (lambda (conn)
    "Truthiness of CONN is tantamount to on/off."
    (let ((completion-predicate
           (lambda (conn)
             (xlsp-capability
               conn xlsp-struct-server-capabilities-completion-provider)))
          (did-change-predicate
           (lambda (conn)
             (xlsp-sync-p conn xlsp-struct-text-document-sync-options-change)))
          (did-save-predicate
           (lambda (conn)
             (xlsp-sync-p conn xlsp-struct-text-document-sync-options-save)))
          (did-open-close-predicate
           (lambda (conn)
             (xlsp-sync-p conn xlsp-struct-text-document-sync-options-open-close)))
          (signature-help-predicate
           (lambda (conn)
             (xlsp-capability
               conn xlsp-struct-server-capabilities-signature-help-provider)))
          (hover-predicate
           (lambda (conn)
             (xlsp-capability
               conn xlsp-struct-server-capabilities-hover-provider)))
          (definitions-predicate
           (lambda (conn)
             (xlsp-capability
               conn xlsp-struct-server-capabilities-definition-provider)))
          (format-predicate
           (lambda (conn)
             "Really annoying feature."
             (when (and (fboundp 'ert-running-test)
                        (ert-running-test))
               (xlsp-format-trigger-characters conn))))
          ;; Note a clear_message() is called on every keystroke,
          ;; but the elisp-mode eldoc functions are fast enough
          ;; to mask a flickering echo area.  Not so, xlsp.
          (eldoc-cache '(nil . nil)))
      (dolist (entry
               ;; [HOOKS PREDICATE CLOSURE &optional DEPTH]
               ;; N.B. You don't have lex binding when defining
               ;; lambda expressions in the backquote.
               `([post-self-insert-hook
                  ,format-predicate
                  (lambda ()
                    (lambda ()
                      ;; how kbd_buffer_get_event calls make_lispy_event
                      ;; to yield the symbol 'return from lispy_function_keys
                      ;; is not yet clear.
                      (let ((event last-input-event))
                        (when (eq event 'return)
                          (setq event ?\n))
                        (when (and (characterp event)
                                   (memq event
                                         (xlsp-format-trigger-characters
                                          (xlsp-connection-get (current-buffer)))))
                          (xlsp-do-request-on-type-formatting
                           (current-buffer) (point) event)))))]
                 [completion-at-point-functions
                  ,completion-predicate
                  xlsp-completion-at-point]
                 [before-change-functions
                  ,did-change-predicate
                  xlsp-did-change-text-document]
                 [after-change-functions
                  ,did-change-predicate
                  xlsp-did-change-text-document]
                 [after-save-hook
                  ,did-save-predicate
                  xlsp-did-save-text-document]
                 [kill-buffer-hook
                  identity
                  (lambda ()
                    (apply-partially #'xlsp-mode -1))]
                 [change-major-mode-hook
                  identity
                  (lambda ()
                    (apply-partially #'xlsp-mode -1))]
                 [before-revert-hook
                  ,did-open-close-predicate
                  (lambda ()
                    (apply-partially #'xlsp-mode -1))]
                 ;; xlsp-mode takes care of after-revert.
                 [eldoc-documentation-functions
                  ,signature-help-predicate
                  (lambda ()
                    (apply-partially
                     #'xlsp-do-request-signature-help
                     (gv-ref ,eldoc-cache)
                     (current-buffer)))]
                 [eldoc-documentation-functions
                  ,hover-predicate
                  (lambda ()
                    (apply-partially
                     #'xlsp-do-request-hover
                     (gv-ref ,eldoc-cache)
                     nil
                     (current-buffer)))]
                 [xref-backend-functions
                  ,definitions-predicate
                  (lambda ()
                    (apply-partially #'identity 'xlsp))]
                 ))
        (cl-destructuring-bind (hooks predicate closure &optional depth)
            (append entry nil)
          (if conn
              (when-let ((add-p (funcall predicate conn))
                         (hook (funcall closure)))
                (add-hook hooks hook depth :local)
                (push (cons hooks hook) xlsp-hooks-alist))
            (when (symbolp closure)     ; one of the defaliases
              (kill-local-variable closure)))))
      (if conn
          (unless eldoc-mode
            ;; Any earlier attempt to activate eldoc-mode
            ;; fails for lack of eldoc-documentation-functions.
            (when (memq eldoc-documentation-function '(nil ignore))
              ;; Primarily for emacs-27 when eldoc-documentation-function
              ;; meant a very different thing.
              (setq-local eldoc-documentation-function
                          (apply-partially
                           #'xlsp-do-request-hover
                           (gv-ref eldoc-cache)
                           :sync
                           (current-buffer)
                           (lambda (&rest args)
                             (identity (car args))))))
            (when xlsp-hover-help-p
              (eldoc-mode)))
        (cl-loop for (hooks . hook) in xlsp-hooks-alist
                 do (remove-hook hooks hook :local)
                 finally (kill-local-variable 'xlsp-hooks-alist)))))
  "Ngl, this is ridonk.")

(cl-defmethod jsonrpc-connection-ready-p ((conn xlsp-connection) _what)
  (and (cl-call-next-method) (oref conn ready-p)))

(cl-defmethod jsonrpc--events-buffer ((conn xlsp-connection))
  "I don't make the rules of jsonrpc.el, but I should."
  (let ((extant (and (slot-boundp conn '-events-buffer)
                     (eieio-oref conn '-events-buffer))))
    (if (buffer-live-p extant)
        extant
      (let ((b (get-buffer-create (format " *%s events*" (eieio-oref conn 'name)))))
        (prog1 b
          (with-current-buffer b (special-mode))
          (eieio-oset conn '-events-buffer b))))))

(defun xlsp-glob-to-regexp (their-glob)
  "Considered `eshell-extended-glob' and friends.
The problem is that goes from glob to files, and I need converse."
  (cl-loop with i = 0
           with sep = (substring (file-name-as-directory ".") -1)
           with result = ""
           with regexp-action =
           (list
            ;; Because last man wins in a tie,
            ;; "**" needs to appear after "*", and
            ;; "**/" needs to appear after "**".
            (cons (concat
                   "\\(\\b{\\|[^\\]{\\)" ; unescaped left curly brace
                   "\\(.*?\\)"           ; non-greedy between braces
                   "\\([^\\]}\\)")       ; unescaped right curly brace
                  (lambda (match)
                    "{a,b} to (a|b).  Bug, {a,[0-9]} becomes (a|\\[0-9])."
                    (concat
                     (save-match-data
                       (replace-regexp-in-string
                        "{" "\\(" (regexp-quote (match-string 1 match))
                        nil t))
                     (save-match-data
                       (replace-regexp-in-string
                        "," "\\|" (regexp-quote (match-string 2 match))
                        nil t))
                     (save-match-data
                       (replace-regexp-in-string
                        "}" "\\)"
                        (replace-regexp-in-string
                         "," "\\|" (regexp-quote (match-string 3 match))
                         nil t)
                        nil t)))))
            (cons "\\(\\b\\[\\|\\([^\\]\\)\\[\\)!" ; unescaped [!)
                  (lambda (match)
                    "[!0-9] to [^0-9]"
                    (concat (match-string 1 match) "^")))
            (cons "\\(\\b\\*\\|\\([^\\]\\)\\*\\)" ; starting or unescaped *
                  (lambda (match)
                    "* to [^/]+"
                    (concat (regexp-quote (or (match-string 2 match) ""))
                            "[^" sep "]+")))
            (cons (concat "\\(\\b\\*\\*" ; starting **
                          "\\|"
                          "\\([^\\]\\)\\*\\*" ; unescaped **
                          "\\)")
                  (lambda (match)
                    "** to .*"
                    (concat (regexp-quote (or (match-string 2 match) "")) ".*")))
            (cons (concat "\\(\\b\\*\\*\\(" sep "\\)" ; starting **/
                          "\\|"
                          "\\([^\\]\\)\\*\\*\\(" sep "\\)" ; unescaped **/
                          "\\)")
                  (lambda (match)
                    "**/ to (.*/)?, e.g., a/**/b accepts a/b"
                    (cond ((match-string 2 match)
                           (concat ".*" sep))
                          ((match-string 4 match)
                           (concat (regexp-quote (or (match-string 3 match) ""))
                                   (concat "\\(.*" sep "\\)?")))
                          (t
                           (concat (regexp-quote (or (match-string 3 match) ""))
                                   (concat ".*"))))))
            (cons "\\(\\b\\?\\|\\([^\\]\\)\\?\\)" ; unescaped ?
                  (lambda (match)
                    "? to [^/]"
                    (concat (regexp-quote (or (match-string 2 match) ""))
                            "[^" sep "]"))))
           for remaining = (substring their-glob i)
           for matches = (seq-keep
                          (lambda (pair)
                            (cl-destructuring-bind (regexp . action)
                                pair
                              (when (string-match regexp remaining)
                                (cons (match-data) action))))
                          regexp-action)
           while (save-match-data
                   (let ((leftmost (length remaining))
                         prevailing)
                     (dolist (match matches)
                       (cl-destructuring-bind (data . action)
                           match
                         (set-match-data data)
                         (when (< (match-beginning 0) leftmost)
                           (setq prevailing match))))
                     (when prevailing
                       (cl-destructuring-bind (data . action)
                           prevailing
                         (set-match-data data)
                         (let ((match-beg (match-beginning 0))
                               (match-end (match-end 0))
                               (replace (funcall action remaining)))
                           (setq result
                                 (concat result
                                         (regexp-quote (substring their-glob i
                                                                  (+ i match-beg)))
                                         replace))
                           (setq i (+ i match-end)))))))
           finally return (concat "^" result (regexp-quote remaining) "$")))

(defun xlsp-connection-register-watched-files (conn reg)
  (cl-macrolet ((gv (id) `(alist-get ,id (oref conn watched-files))))
    (let ((id (intern (xlsp-struct-registration-id reg))))
      (xlsp-connection-unregister-watched-files
       conn (make-xlsp-struct-unregistration
             :id (symbol-name id)
             :method (xlsp-struct-registration-method reg)))
      (setf (gv id)
            (seq-keep
             (lambda (watcher) ; deal with WatchKind later
               (let ((pat (xlsp-struct-file-system-watcher-glob-pattern watcher)))
                 (when (stringp pat) ; deal with RelativePattern later
                   (xlsp-glob-to-regexp pat))))
             (xlsp-struct-did-change-watched-files-registration-options-watchers
              (xlsp-unjsonify 'xlsp-struct-did-change-watched-files-registration-options
                              (xlsp-struct-registration-register-options reg))))))))

(defun xlsp-connection-unregister-watched-files (conn unreg)
  (cl-macrolet ((gv (id) `(alist-get ,id (oref conn watched-files))))
    (let ((id (intern (xlsp-struct-unregistration-id unreg))))
      (mapc #'file-notify-rm-watch (gv id))
      (setf (gv id) (assq-delete-all id (gv id))))))

(defun xlsp-format-trigger-characters (conn)
  (when-let ((options
              (xlsp-capability
                conn
                xlsp-struct-server-capabilities-document-on-type-formatting-provider)))
    (cl-remove-if
     #'zerop ; string-to-char of empty string is NUL character
     (seq-keep
      #'string-to-char
      (cons (xlsp-struct-document-on-type-formatting-options-first-trigger-character
             options)
            (append
             (xlsp-struct-document-on-type-formatting-options-more-trigger-character
              options)
             nil))))))

;;;###autoload
(defun xlsp-format-buffer ()
  (interactive)
  (unless xlsp-mode
    (xlsp-mode))
  (when-let ((conn (xlsp-connection-get (current-buffer)))
             (capable-p
              (xlsp-capability
                conn xlsp-struct-server-capabilities-document-formatting-provider)))
    (xlsp-do-request-formatting (current-buffer))))

;;;###autoload
(defun xlsp-format-region (beg end)
  (interactive "r")
  (unless xlsp-mode
    (xlsp-mode))
  (when-let ((conn (xlsp-connection-get (current-buffer)))
             (capable-p
              (xlsp-capability
                conn xlsp-struct-server-capabilities-document-range-formatting-provider)))
    (xlsp-do-request-range-formatting (current-buffer) beg end)))

(put 'xlsp-workspace-configuration 'safe-local-variable 'listp)

;;;###autoload
(define-globalized-minor-mode global-xlsp-mode
  xlsp-mode
  (lambda ()
    (when (and (buffer-file-name)
               (project-current)
               (alist-get major-mode xlsp-server-invocations))
      (xlsp-mode)))
  :group 'xlsp
  :version "29.1")

;; Should not require autoload since only relevant for toggling off.
(let ((xlsp-advise-global
       (lambda (f &rest args)
         "Constrain buffer-list to avoid buffers killed by side-effect."
         (let ((just-source-files
                (lambda (buffers)
                  (cl-remove-if-not #'buffer-file-name buffers))))
           (unwind-protect
               (progn
                 (add-function :filter-return
                               (symbol-function 'buffer-list)
                               just-source-files)
                 (apply f args))
             (remove-function (symbol-function 'buffer-list)
                              just-source-files))))))
  (add-function :around (symbol-function 'global-xlsp-mode)
                xlsp-advise-global
                `((name . ,(xlsp-advise-tag xlsp-advise-global)))))

(provide 'xlsp)

;;; xlsp.el ends here
