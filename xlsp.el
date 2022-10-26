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

;; Overview
;; --------
;; M-x xlsp-mode toggles for the current buffer.
;; M-x global-xlsp-mode toggles globally.
;;
;; .emacs or init.el configuration
;; -------------------------------
;; (add-hook 'c-mode-hook #'xlsp-mode) ; activate for c-mode
;; -or-
;; (global-xlsp-mode) ; activate for any mode in xlsp-server-invocations
;;
;; Nothing happens
;; ---------------
;; Modes must have a corresponding entry in xlsp-server-invocations.
;;
;; Per-workspace configuration
;; ---------------------------
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
;;
;; What the "x" in "xlsp" references
;; ---------------------------------
;; It is merely a differentiator, just as the "x" in "xemacs" had been
;; (contrary to the popular misconception that it referenced X11).

;;; Code:

(require 'filenotify)
(require 'project)
(require 'xlsp-handle-request)
(require 'xlsp-handle-notification)
(require 'xlsp-company)
(require 'xlsp-server)

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar xlsp--synchronize-closure)
    (defvar xlsp--did-change-text-document)
    (defvar xlsp--did-save-text-document)
    (defvar xlsp--did-open-text-document)
    (defvar xlsp--did-close-text-document))
  (when (< emacs-major-version 29)
    (defun seq-keep (function sequence)
      "Apply FUNCTION to SEQUENCE and return all non-nil results."
      (delq nil (seq-map function sequence)))))

;; Commercial Emacs
(declare-function tree-sitter-node-type "tree-sitter")
(declare-function tree-sitter-node-at "tree-sitter")

(defclass xlsp-connection (jsonrpc-process-connection)
  ((ready-p :initform nil :type boolean :documentation "Handshake completed.")
   (capabilities :initform nil)
   (server-info :initform nil)
   (buffers :initform nil)
   (watched-files :initform nil)))

(defmacro xlsp-advise-tag (variable)
  `(symbol-name ',variable))

(cl-defmethod initialize-instance ((conn xlsp-connection) _slots)
  (cl-call-next-method)
  (let ((xlsp-advise-sentinel
         (lambda (f proc change &rest args)
           "jsonrpc--message should not usurp echo area."
           (cl-letf (((symbol-function 'jsonrpc--message) #'ignore))
             ;; inhibit-message not cutting it
             (prog1 (apply f proc change args)
               (when-let ((conn (process-get proc 'jsonrpc-connection))
                          (died-p (not (process-live-p proc))))
                 (display-warning
                  'xlsp
                  (format "%s, %s" (process-name proc) change)
                  :warning))))))
        (xlsp-advise-filter
         (lambda (proc string)
           "jsonrpc--process-filter prone to typical output flushing issues."
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (let ((inhibit-read-only t)
                     (connection (process-get proc 'jsonrpc-connection)))
                 ;; Insert the text, advancing the process marker.
                 (save-excursion
                   (goto-char (process-mark proc))
                   (insert string)
                   (set-marker (process-mark proc) (point)))
                 (catch 'done
                   (while t
                     ;; More than one message might have arrived
                     (unless (setf (jsonrpc--expected-bytes connection)
                                   (or (jsonrpc--expected-bytes connection)
                                       (and (search-forward-regexp
                                             "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                                             (+ (point) 100) t)
                                            (string-to-number (match-string 1)))))
                       (throw 'done t))

                     ;; Attempt to complete a message body
                     (let ((available-bytes (- (position-bytes (process-mark proc))
                                               (position-bytes (point))))
                           (message-end (byte-to-position
                                         (+ (position-bytes (point))
                                            (jsonrpc--expected-bytes connection)))))
                       (cond ((< available-bytes (jsonrpc--expected-bytes connection))
                              ;; message still incomplete
                              (throw 'done t))
                             ((< (length (buffer-substring-no-properties (point) message-end))
                                 (- message-end (point)))
                              ;; output flushing vagaries
                              (throw 'done t))
                             (t
                              (save-restriction
                                (narrow-to-region (point) message-end)
                                (unwind-protect
                                    (when-let ((json-message
                                                (condition-case err
                                                    (jsonrpc--json-read)
                                                  (error
                                                   (prog1 nil
                                                     (jsonrpc--warn "Invalid JSON: %s\n%s"
                                                                    (cdr err) (buffer-string)))))))
                                      (with-temp-buffer
                                        ;; Calls success-fn and error-fn of
                                        ;; jsonrpc-async-request, which can arbitrarily
                                        ;; pollute or even kill (process-buffer PROC).
                                        ;; Ergo, ensuing buffer-live-p check.
                                        (jsonrpc-connection-receive connection json-message)))
                                  (when (buffer-live-p (process-buffer proc))
                                    (with-current-buffer (process-buffer proc)
                                      (goto-char message-end)
                                      (delete-region (point-min) (point))
                                      (setf (jsonrpc--expected-bytes connection) nil))))))))))))))))
    (add-function :around (process-sentinel (jsonrpc--process conn))
                  xlsp-advise-sentinel
                  `((name . ,(xlsp-advise-tag xlsp-advise-sentinel))))
    (add-function :override (process-filter (jsonrpc--process conn))
                  xlsp-advise-filter
                  `((name . ,(xlsp-advise-tag xlsp-advise-filter))))))

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

(defmacro xlsp-project-root (project)
  "`project-root' only exists in emacs-28"
  (if (fboundp 'project-root)
      `(project-root ,project)
    (with-suppressed-warnings ((obsolete project-roots))
      `(car (project-roots ,project)))))

(defmacro xlsp-project-buffers (project)
  "`project-buffers' only exists in emacs-28"
  (if (fboundp 'project-buffers)
      `(project-buffers ,project)
    `(let ((root (expand-file-name (file-name-as-directory
                                    (xlsp-project-root ,project)))))
       (nreverse
        (cl-loop for b in (buffer-list)
                 for dd = (expand-file-name (buffer-local-value
                                             'default-directory b))
                 when (string-prefix-p root dd)
                 collect b)))))

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
    (xlsp--connection-destroy conn-key project-dir)))

(defun xlsp-find-file-hook (conn-key*)
  (with-xlsp-connection (conn-key project-dir)
      (current-buffer)
    (when (equal conn-key conn-key*)
      (funcall (xlsp-did-open-text-document)))))

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

(defun xlsp-their-pos (buffer our-pos)
  "Return cons pair of LSP-space zero-indexed line and character offset.
PositionEncodingKind currently disregarded."
  (with-current-buffer buffer
    (save-excursion
      (goto-char our-pos)
      (save-restriction
        (widen)
        (let ((utf-16 (encode-coding-region (line-beginning-position)
                                            (point) 'utf-16 t)))
          (cons (1- (line-number-at-pos))
                (/ (- (length utf-16) 2) 2))))))) ; subtract 2 for BOM

(defun xlsp-our-pos (buffer their-pos)
  "Return one-indexed charpos for LSP-space zero-indexed line/offset."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((source-line (line-number-at-pos))
              (target-line (1+ (xlsp-struct-position-line their-pos))))
          (when-let ((line-reached (zerop (forward-line (- target-line source-line))))
                     (utf-16 (encode-coding-region
                              (line-beginning-position)
                              (line-end-position) 'utf-16 t))
                     (pos (xlsp-struct-position-character their-pos))
                     (upto-char (condition-case nil
                                    (cl-subseq      ; add 2 for BOM
                                     utf-16 0
                                     (+ 2 (* 2 pos)))
                                  (args-out-of-range
                                   ;; server often out of sync by design
                                   nil))))
            (+ (line-beginning-position)
               (length (decode-coding-string upto-char 'utf-16)))))))))

(defmacro xlsp-sync-then-request (buffer &rest args)
  `(progn
     (funcall (with-current-buffer ,buffer (xlsp-synchronize-closure)) :send t)
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
             when (string-prefix-p query (xlsp-new-text item) :ignore-case)
             collect item)))

(defvar xlsp-completion-filter-function (symbol-function 'xlsp-default-completion-filter)
  "Up to you.")

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

(defmacro xlsp-capability (conn &rest methods)
  (declare (indent defun))
  `(when-let ((result (oref ,conn capabilities)))
     (catch 'done
       (dolist (method ',methods)
         (setq result (ignore-errors (funcall method result)))
         (unless result (throw 'done result))))
     result))

(defun xlsp-do-request-completion (buffer pos callback trigger-char)
  (when-let ((conn (xlsp-connection-get buffer))
             (params (make-xlsp-struct-completion-params
                      :text-document (make-xlsp-struct-text-document-identifier
                                      :uri (xlsp-urify (concat (buffer-file-name buffer))))
                      :position (let ((send-pos (xlsp-their-pos buffer pos)))
                                  (make-xlsp-struct-position
                                   :line (car send-pos)
                                   :character (cdr send-pos)))
                      :context (make-xlsp-struct-completion-context
                                :trigger-kind
                                (if trigger-char
                                    xlsp-completion-trigger-kind/trigger-character
                                  xlsp-completion-trigger-kind/invoked)
                                :trigger-character
                                (when trigger-char (char-to-string trigger-char))))))
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
                     (plist-get error :code))))))

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

(defmacro xlsp-tack (lst tack)
  `(if (consp ,lst)
       (setcdr (last ,lst) (list ,tack))
     (setf ,lst (list ,tack))))

(define-minor-mode xlsp-mode
  "Start and consult language server if applicable."
  :lighter nil
  :group 'xlsp
  (require 'company)
  (require 'company-capf)               ; for company--contains
  (defvar company-prefix)
  (defvar company-candidates-cache)
  (defvar company-candidates)
  (defvar company-backends)
  (defvar company-minimum-prefix-length)
  (defvar company-tooltip-idle-delay)
  (defvar company-idle-delay)
  (defvar company-mode)
  (defvar global-company-mode)
  (defvar company-candidates-length)
  (defvar company-point)
  (declare-function company-grab-symbol "company")
  (declare-function company-in-string-or-comment "company")
  (declare-function company-mode "company")
  (declare-function company--contains "company-capf")
  (declare-function company-call-frontends "company")
  (declare-function company-update-candidates "company")
  (let* ((completion-state `((beg . nil) (end . nil) (cache-p . nil)
                             (kinds . nil) (details . nil) (trigger-char . nil)
                             (index-of . ,(make-hash-table :test #'equal))))
         (completion-callback
          (lambda (buffer* cb* completion-list)
            "The interval [BEG, END) spans the region supplantable by
CANDIDATES.  CACHE-P advises `company-calculate-completions'
whether to cache CANDIDATES."
            (if-let ((items
                      (append
                       (xlsp-struct-completion-list-items completion-list) nil))
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
                                  (bounds-of-thing-at-point 'symbol))))
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
                  (setf (alist-get 'beg completion-state) beg
                        (alist-get 'end completion-state) end
                        (alist-get 'cache-p completion-state) (not (xlsp-struct-completion-list-is-incomplete completion-list))
                        (alist-get 'kinds completion-state) (mapcar #'xlsp-struct-completion-item-kind filtered-items)
                        (alist-get 'details completion-state) (mapcar #'xlsp-struct-completion-item-detail filtered-items))
                  (clrhash (alist-get 'index-of completion-state))
                  (dotimes (i (length texts))
                    (puthash (nth i texts) i
                             (alist-get 'index-of completion-state)))
                  (when (xlsp-capability (xlsp-connection-get buffer*)
                          xlsp-struct-server-capabilities-completion-provider
                          xlsp-struct-completion-options-resolve-provider)
                    ;; "By default the request can only delay the
                    ;; computation of the detail and documentation
                    ;; properties."
                    (cl-loop with conn = (xlsp-connection-get buffer*)
                             with obeg = (alist-get 'beg completion-state)
                             with oend = (alist-get 'end completion-state)
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
                                            (alist-get 'beg completion-state)))
                                          (stable-end-p
                                           (eq
                                            oend
                                            (alist-get 'end completion-state))))
                                       (setf (nth i* (alist-get 'kinds completion-state))
                                             (xlsp-struct-completion-item-kind post))
                                       (setf (nth i* (alist-get 'details completion-state))
                                             (xlsp-struct-completion-item-detail post))
                                       (with-current-buffer buffer*
                                         (company-update-candidates company-candidates)
                                         (company-call-frontends 'update)
                                         (company-call-frontends 'post-command)))))
                                  i)))))
              (prog1 (funcall cb* nil)
                (clrhash (alist-get 'index-of completion-state))
                (setf (alist-get 'beg completion-state) nil
                      (alist-get 'end completion-state) nil
                      (alist-get 'cache-p completion-state) nil
                      (alist-get 'kinds completion-state) nil
                      (alist-get 'details completion-state) nil)))))
         (completion-directive
          (lambda (cb)
            (xlsp-do-request-completion
             (current-buffer) (point)
             (apply-partially completion-callback (current-buffer) cb)
             (alist-get 'trigger-char completion-state))))
         (xlsp-advise-cache
          (lambda (f &rest args)
            "LSP said isIncomplete.  So undo the caching."
            (let ((restore-cache company-candidates-cache))
              (prog1 (apply f args)
                (when xlsp-mode
                  (unless (alist-get 'cache-p completion-state)
                    (setq company-candidates-cache restore-cache)))))))
         (xlsp-advise-prefix
          (lambda (&rest _args)
            "Don't tell LSP what the prefix is."
            (when xlsp-mode
              (cl-destructuring-bind (beg . end)
                  (cons (alist-get 'beg completion-state)
                        (alist-get 'end completion-state))
                (when beg
                  ;; `company--insert-candidate' requires point, not END.
                  (setq company-prefix (buffer-substring-no-properties beg (point))))))))
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
               (when-let ((detail (nth (gethash candidate
                                                (alist-get 'index-of completion-state))
                                       (alist-get 'details completion-state))))
                 (concat " " (propertize
                              detail 'face 'font-lock-function-name-face))))
              (kind             ; keys in company-vscode-icons-mapping
               (when-let ((kind (alist-get
                                 (nth (gethash candidate
                                               (alist-get 'index-of completion-state))
                                      (alist-get 'kinds completion-state))
                                 xlsp-company-kind-alist)))
                 (intern-soft (downcase kind))))
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
                             (setf (alist-get 'trigger-char completion-state)
                                   trigger-char))
                     (prog1 word
                       (setf (alist-get 'trigger-char completion-state)
                             nil))))))))))
    (if xlsp-mode
        (progn
          (add-function :around (symbol-function 'company-calculate-candidates)
                        xlsp-advise-cache
                        `((name . ,(xlsp-advise-tag xlsp-advise-cache))))
          (add-function :before (symbol-function 'company-update-candidates)
                        xlsp-advise-prefix
                        `((name . ,(xlsp-advise-tag xlsp-advise-prefix))))
          (add-function :override (symbol-function 'company--contains)
                        xlsp-advise-contains
                        `((name . ,(xlsp-advise-tag xlsp-advise-contains))))
          (setq-local company-backends company-backends)
          (cl-pushnew backend company-backends)
          (cl-macrolet ((unchanged-p
                          (var)
                          `(eq (symbol-value ',var)
                               (car (get ',var 'standard-value)))))
            (unless (unchanged-p company-minimum-prefix-length)
              (setq-local company-minimum-prefix-length 3))
            (unless (unchanged-p company-tooltip-idle-delay)
              (setq-local company-tooltip-idle-delay 0.5))
            (unless (unchanged-p company-idle-delay)
              (setq-local company-idle-delay 0.2)))
          (unless company-mode
            (company-mode))
          (unless eldoc-mode
            (eldoc-mode))
          (xlsp-toggle-hooks nil) ; cleanse palate even if toggling on
          (when-let ((conn (xlsp-connection-get (current-buffer))))
            (if (jsonrpc-connection-ready-p conn :handshook)
                (xlsp-toggle-hooks conn)
              (ignore "Presume async handshake will install hooks."))))
      ;; those add-functions are forever...
      (xlsp-toggle-hooks nil)
      (xlsp-deregister-buffer (current-buffer))
      (unless global-company-mode
        (company-mode -1))
      (unless global-eldoc-mode
        (eldoc-mode -1))
      (mapc #'kill-local-variable '(company-backends
                                    company-minimum-prefix-length
                                    company-idle-delay
                                    company-tooltip-idle-delay)))))

(defun xlsp--connection-destroy (conn-key project-dir)
  ;; Remove hook added in xlsp--connect.
  (remove-hook 'find-file-hook (apply-partially #'xlsp-find-file-hook conn-key))
  (dolist (b (cl-remove-if-not
              #'buffer-file-name
              (when-let ((proj (project-current nil project-dir)))
                (xlsp-project-buffers proj))))
    (with-current-buffer b
      (when (equal major-mode (car conn-key))
        (when xlsp-mode
          (xlsp-mode -1)))))

  (let ((conn (xlsp-gv-connection conn-key)))
    (when (and conn (jsonrpc-running-p conn))
      (jsonrpc-async-request
       conn
       xlsp-request-shutdown
       nil
       :error-fn
       (lambda (error)
         (xlsp-message "%s %s %s (%s)"
                       xlsp-request-shutdown
                       (xlsp-conn-string (car conn-key) project-dir)
                       (plist-get error :message) (plist-get error :code)))
       :timeout
       2
       :timeout-fn
       (lambda ()
         (xlsp-message "%s %s did not respond"
                       xlsp-request-shutdown
                       (xlsp-conn-string (car conn-key) project-dir))))

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

(defmacro xlsp-sync-p (conn field)
  `(when-let ((kind (xlsp-sync-kind ,conn ,field)))
     (not (eq kind xlsp-text-document-sync-kind/none))))

(defmacro xlsp-sync-kind (conn field)
  "Return value of sync kind."
  `(when-let ((sync (xlsp-capability ,conn
                      xlsp-struct-server-capabilities-text-document-sync)))
     (if (recordp sync)
         (funcall ',field sync)
       sync)))

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

                ;; Register future file-opens for mode and project (conn-key).
                (with-xlsp-connection (conn-key project-dir)
                    buffer
                  (when (xlsp-sync-p
                         conn xlsp-struct-text-document-sync-options-open-close)
                    (add-hook 'find-file-hook
                              (apply-partially #'xlsp-find-file-hook conn-key))))

                ;; Ack
                (jsonrpc-notify conn xlsp-notification-initialized
                                (xlsp-jsonify (make-xlsp-struct-initialized-params)))

                ;; Register project files
                (with-xlsp-connection (conn-key project-dir)
                    buffer
                  (dolist (b (cl-remove-if-not
                              #'buffer-file-name
                              (when-let ((proj (project-current nil project-dir)))
                                (xlsp-project-buffers proj))))
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
                   :settings (or (bound-and-true-p xlsp-workspace-configuration)
                                 xlsp-struct-empty))))))))
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

(cl-defstruct xlsp-struct-synchronize
  "Keep shit together."
  (buffer nil :type buffer)
  (version 0 :type integer)
  (events nil :type (list-of xlsp-literal))
  (timer nil :type vector))

(defmacro xlsp-get-closure (probe-p private expr)
  "I'll do a lot to avoid defvar proliferation."
  (declare (indent defun))
  `(or (bound-and-true-p ,private)
       (unless ,probe-p
         (set (make-local-variable ',private) ,expr))))

(defun xlsp-synchronize-closure (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--synchronize-closure
    (apply-partially
     (cl-function
      (lambda (synchronize* &key cumulate send save version &allow-other-keys)
        "SYNCHRONIZE* is naturally thread unsafe state."
        (when cumulate
          (push cumulate
                (xlsp-struct-synchronize-events synchronize*))
          (cl-incf (xlsp-struct-synchronize-version synchronize*))
          (when (and (not (xlsp-struct-synchronize-timer synchronize*))
                     (not send))
            (setf (xlsp-struct-synchronize-timer synchronize*)
                  (run-with-idle-timer
                   0.8 nil
                   (apply-partially
                    (lambda (buffer*)
                      "Assume no context switch to :cumulate during :send."
                      (with-current-buffer buffer*
                        (funcall xlsp--synchronize-closure :send t)
                        (setf (xlsp-struct-synchronize-timer synchronize*) nil)))
                    (xlsp-struct-synchronize-buffer synchronize*))))))
        (when send
          (when (xlsp-struct-synchronize-timer synchronize*)
            (cancel-timer (xlsp-struct-synchronize-timer synchronize*)))
          (let* ((conn (xlsp-connection-get
                        (xlsp-struct-synchronize-buffer synchronize*)))
                 (kind (xlsp-sync-kind conn xlsp-struct-text-document-sync-options-change))
                 (changes
                  (cond ((eq kind xlsp-text-document-sync-kind/none) nil)
                        ((eq kind xlsp-text-document-sync-kind/incremental)
                         (apply #'vector
                                (nreverse
                                 (copy-sequence
                                  (xlsp-struct-synchronize-events synchronize*)))))
                        (t (let ((full-text
                                  (with-current-buffer
                                      (xlsp-struct-synchronize-buffer synchronize*)
                                    (save-restriction
                                      (widen)
                                      (buffer-substring-no-properties
                                       (point-min) (point-max))))))
                             (vector (xlsp-literal :text full-text)))))))
            (unless (zerop (length changes))
              (jsonrpc-notify ; notifications are fire-and-forget
               conn
               xlsp-notification-text-document/did-change
               (xlsp-jsonify
                (make-xlsp-struct-did-change-text-document-params
                 :text-document
                 (make-xlsp-struct-versioned-text-document-identifier
                  :uri (xlsp-urify
                        (buffer-file-name
                         (xlsp-struct-synchronize-buffer synchronize*)))
                  :version (xlsp-struct-synchronize-version synchronize*))
                 :content-changes
                 (prog1 changes
                   ;; Events sent; clear for next batch.
                   (setf (xlsp-struct-synchronize-events synchronize*) nil))))))))
        (when save
          (jsonrpc-notify
           (xlsp-connection-get (xlsp-struct-synchronize-buffer synchronize*))
           xlsp-notification-text-document/did-save
           (xlsp-jsonify
            (make-xlsp-struct-did-save-text-document-params
             :text-document
             (make-xlsp-struct-versioned-text-document-identifier
              :uri (xlsp-urify
                    (buffer-file-name
                     (xlsp-struct-synchronize-buffer synchronize*)))
              :version (xlsp-struct-synchronize-version synchronize*))))))
        (when version
          (xlsp-struct-synchronize-version synchronize*))))
     (make-xlsp-struct-synchronize :buffer (current-buffer)))))

(defun xlsp-did-change-text-document (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--did-change-text-document
    (let* ((state-empty '((character-offset . nil) (line-offset . nil) (snippet . nil)))
           (before-state* state-empty))
      (lambda (beg end &optional deleted)
        "BEFORE-STATE* is naturally thread unsafe."
        (cl-flet ((get-row-col (pos)
                    (let ((send-pos
                           (with-temp-buffer
                             (save-excursion
                               (insert (alist-get 'snippet before-state*)))
                             (xlsp-their-pos (current-buffer) (min pos (point-max))))))
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
                :range (xlsp-jsonify
                        (make-xlsp-struct-range
                         ;; get-row-col POS is one-indexed
                         :start (get-row-col pos)
                         :end (get-row-col (+ pos deleted))))
                :text (buffer-substring-no-properties beg end))))
            (setq before-state* state-empty)))))))

(defun xlsp-did-save-text-document (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--did-save-text-document
    (lambda () (funcall (xlsp-synchronize-closure) :save t))))

(defun xlsp-did-open-text-document (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--did-open-text-document
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
     (current-buffer))))

(defun xlsp-did-close-text-document (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--did-close-text-document
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
     (current-buffer))))

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
    (let ((did-change-predicate
           (lambda (conn)
             (xlsp-sync-p conn xlsp-struct-text-document-sync-options-change)))
          (did-save-predicate
           (lambda (conn)
             (xlsp-sync-p conn xlsp-struct-text-document-sync-options-save)))
          (did-open-close-predicate
           (lambda (conn)
             (xlsp-sync-p conn xlsp-struct-text-document-sync-options-open-close))))
      (dolist (entry
               ;; [HOOKS PREDICATE CLOSURE &optional DEPTH]
               `([before-change-functions
                  ,did-change-predicate
                  xlsp-did-change-text-document]
                 [after-change-functions
                  ,did-change-predicate
                  xlsp-did-change-text-document]
                 [after-save-hook
                  ,did-save-predicate
                  xlsp-did-save-text-document]
                 [kill-buffer-hook
                  ,did-open-close-predicate
                  xlsp-did-close-text-document
                  -2]
                 [kill-buffer-hook
                  identity
                  (lambda (&optional probe-p) ; for the closure
                    (apply-partially #'xlsp-deregister-buffer (current-buffer)))
                  2]
                 ;; xlsp-find-file-hook takes care of after-revert.
                 [before-revert-hook
                  ,did-open-close-predicate
                  xlsp-did-close-text-document]))
        (cl-destructuring-bind (hooks predicate closure &optional depth)
            (append entry nil)
          (if conn
              (when (funcall predicate conn)
                (add-hook hooks (funcall closure) depth :local))
            (when-let ((extant (funcall closure :probe)))
              (remove-hook hooks extant :local))))))))

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

(put 'xlsp-workspace-configuration 'safe-local-variable 'listp) ; see Commentary

;;;###autoload
(define-globalized-minor-mode global-xlsp-mode
  xlsp-mode
  (lambda ()
    (when (and (buffer-file-name)
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
