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

;; The "x" in "xlsp" is merely a differentiator, just as the "x" in
;; "xemacs" had been (contrary to the popular misconception that it
;; referenced X11).
;;
;; Emacs's completion model "capf" came with Blandy's initial revision in
;; 1991.  It received a facelift in 2008 but the bones remained
;; largely intact.
;;
;; Preservationists laboriously retrofit any new feature to the capf
;; model, which lacks async and couldn't have foreseen Microsoft's
;; Language Server Protocol (LSP).  In particular, capf keys off a
;; client-determined prefix, whilst LSP's notion of the prefix
;; originates from the server.

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

(require 'project)
(require 'xlsp-handle-request)
(require 'xlsp-handle-notification)

(defgroup xlsp nil
  "Language Server Protocol."
  :prefix "xlsp-"
  :group 'applications)

(defclass xlsp-connection (jsonrpc-process-connection)
  ((ready-p :initform nil :type boolean :documentation "Handshake completed.")
   (capabilities :initform nil)
   (server-info :initform nil)
   (buffers :initform nil)))

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

(defun xlsp-connection--destroy (conn-key)
  "Should not be called outside xlsp.el."
  ;; Was added in xlsp--connect.
  (remove-hook 'find-file-hook (apply-partially #'xlsp-find-file-hook conn-key))
  (let ((conn (xlsp-gv-connection conn-key)))
    (if (and conn (jsonrpc-running-p conn))
        (jsonrpc-async-request
         conn
         xlsp-request-shutdown
         nil
         :success-fn
         (cl-function
          (lambda (_no-result)
            (jsonrpc-notify conn xlsp-notification-exit nil)
            (setq xlsp--connections (assoc-delete-all conn-key xlsp--connections))
            (run-with-timer 3 nil #'jsonrpc-shutdown conn :cleanup)))
         :error-fn
         (lambda (error)
           (xlsp-message "%s %s (%s)" xlsp-request-shutdown (plist-get error :message)
                         (plist-get error :code))))
      (setq xlsp--connections (assoc-delete-all conn-key xlsp--connections))
      (when conn
        (jsonrpc-shutdown conn :cleanup)))))

(defun xlsp-connection-remove (buffer)
  (with-xlsp-connection (conn-key project-dir)
      buffer
    (xlsp-connection--destroy conn-key)))

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

(defun xlsp-completion-in-region (buffer beg)
  "No-frills minibuffer completion, like `lisp-complete-symbol'."
  (interactive (list (current-buffer) (point)))
  (when-let ((beg-end (with-current-buffer buffer
                        (save-excursion
                          (goto-char beg)
                          (bounds-of-thing-at-point 'symbol))))
             (our-prefix (with-current-buffer buffer
                           (buffer-substring-no-properties
                            (car beg-end) (cdr beg-end))))
             (candidates (xlsp-completion-salvo buffer our-prefix beg)))
    (completion-in-region (car beg-end) (cdr beg-end) candidates)))

(defun xlsp-position (buffer pos)
  "Return cons pair of LSP-space zero-indexed line and character offset.
PositionEncodingKind currently disregarded."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (save-restriction
        (widen)
        (let ((utf-16 (encode-coding-region (line-beginning-position)
                                            (point) 'utf-16 t)))
          (cons (1- (line-number-at-pos))
                (1- (/ (length utf-16) 2))))))))

(defmacro xlsp-sync-then-request (buffer &rest args)
  `(progn
     (funcall (with-current-buffer ,buffer (xlsp-synchronize-closure)) :send t)
     (funcall #'jsonrpc-request ,@args)))

(defun xlsp-completion-salvo (buffer filter-on pos)
  "One-shot learning."
  (when-let ((conn (xlsp-connection-get buffer))
             (params (make-xlsp-struct-completion-params
                      :text-document (make-xlsp-struct-text-document-identifier
                                      :uri (xlsp-urify (buffer-file-name buffer)))
                      :position (let ((send-pos (xlsp-position buffer pos)))
                                  (make-xlsp-struct-position
                                   :line (car send-pos)
                                   :character (cdr send-pos)))
                      :context (make-xlsp-struct-completion-context
                                :trigger-kind xlsp-completion-trigger-kind/invoked)))
             (result (xlsp-unjsonify
                      'xlsp-struct-completion-list
                      (xlsp-sync-then-request
                       buffer conn xlsp-request-text-document/completion
                       (xlsp-jsonify params) :deferred t))))
    (cl-loop for item in (append (xlsp-struct-completion-list-items result) nil)
             ;; :end2 says PREFIX matches beginning of RESULT
             when (cl-search filter-on (xlsp-struct-completion-item-filter-text item)
                             :end2 (length filter-on))
             collect (xlsp-struct-completion-item-filter-text item))))

(defun xlsp--capabilities (project-dir)
  (ignore project-dir)
  xlsp-default-struct-client-capabilities)

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

(defun xlsp-message (format &rest args)
  "Message out with FORMAT with ARGS."
  (let ((lhs (format-time-string "%Y%m%dT%H%M%S [lsp] " (current-time)))
        (inhibit-message t))
    (apply #'message (concat lhs format) args)))

(defmacro xlsp-capability (conn &rest methods)
  (declare (indent defun))
  `(when-let ((result (oref ,conn capabilities)))
     (catch 'done
       (dolist (method ',methods)
         (setq result (funcall method result))
         (unless result (throw 'done result))))
     result))

(define-minor-mode xlsp-mode
  "Start and consult language server if applicable."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (prog1 map
              ;; Because ph___ capf
              (define-key map (kbd "C-M-<return>") #'xlsp-completion-in-region)))
  :group 'xlsp
  (xlsp-toggle-hooks nil) ; cleanse palate even if toggling on
  (when xlsp-mode
    (when-let ((conn (xlsp-connection-get (current-buffer))))
      (if (jsonrpc-connection-ready-p conn :handshook)
          (xlsp-toggle-hooks conn)
        (ignore "Presume async handshake will toggle hooks.")))))

(defun xlsp--connect (buffer project-dir)
  (when-let
      ((mode (buffer-local-value 'major-mode buffer))
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
       (command (alist-get mode xlsp-invocations))
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
                                   (xlsp-connection--destroy conn-key)))
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
                (when (xlsp-capability conn
                        xlsp-struct-server-capabilities-text-document-sync
                        xlsp-struct-text-document-sync-options-open-close)
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
                            (xlsp-project-buffers (project-current nil project-dir))))
                  (with-current-buffer b
                    (when (equal major-mode (car conn-key))
                      ;; Now that we know capabilities, selectively activate hooks
                      (when xlsp-mode
                        (xlsp-toggle-hooks nil) ; cleanse palate
                        (xlsp-toggle-hooks conn)
                        (funcall (xlsp-did-open-text-document)))))))

              ;; Register workspace-specific config, if any.
              (when-let ((settings
                          (bound-and-true-p xlsp-workspace-configuration)))
                (jsonrpc-notify
                 conn xlsp-notification-workspace/did-change-configuration
                 (xlsp-jsonify
                  (make-xlsp-struct-did-change-configuration-params
                   :settings settings)))))))
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
  (events nil :type (list-of xlsp-literal))
  (timer nil :type vector))

(defmacro xlsp-get-closure (probe-p private expr)
  "I'll do a lot to avoid defvar proliferation."
  (declare (indent defun))
  `(or (bound-and-true-p ,private)
       (unless ,probe-p
         (set (make-local-variable ',private) ,expr))))

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar xlsp--synchronize-closure)
    (defvar xlsp--did-change-text-document)
    (defvar xlsp--did-save-text-document)
    (defvar xlsp--did-open-text-document)
    (defvar xlsp--did-close-text-document)))

(defun xlsp-synchronize-closure (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--synchronize-closure
    (apply-partially
     (cl-function
      (lambda (synchronize* &key cumulate send save &allow-other-keys)
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
                 (kind (xlsp-capability conn
                         xlsp-struct-server-capabilities-text-document-sync
                         xlsp-struct-text-document-sync-options-change))
                 (changes
                  (if (equal kind xlsp-text-document-sync-kind/incremental)
                      (apply #'vector
                             (nreverse
                              (copy-sequence
                               (xlsp-struct-synchronize-events synchronize*))))
                    (let ((full-text
                           (with-current-buffer
                               (xlsp-struct-synchronize-buffer synchronize*)
                             (save-restriction
                               (widen)
                               (buffer-substring-no-properties
                                (point-min) (point-max))))))
                      (vector (xlsp-literal :text full-text))))))
            (unless (zerop (length changes))
              (jsonrpc-notify
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
              :version (xlsp-struct-synchronize-version synchronize*))))))))
     (make-xlsp-struct-synchronize :buffer (current-buffer)))))

(defun xlsp-did-change-text-document (&optional probe-p)
  (xlsp-get-closure
    probe-p
    xlsp--did-change-text-document
    (let ((state-empty
           '((character-offset . nil) (line-offset . nil) (snippet . nil))))
      (apply-partially
       (lambda (before-state* beg end &optional deleted)
         "BEFORE-STATE* is naturally thread unsafe."
         (cl-flet ((get-row-col (pos)
                     (let ((send-pos
                            (with-temp-buffer
                              (save-excursion
                                (insert (alist-get 'snippet before-state*)))
                              (xlsp-position (current-buffer) (min pos (point-max))))))
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
             (setq before-state* state-empty))))
       state-empty))))

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
                          :version 0
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
                          :version 0
                          :text (with-current-buffer buffer*
                                  (save-restriction
                                    (widen)
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))))))))
     (current-buffer))))

(defalias 'xlsp-toggle-hooks
  (lambda (conn)
    (let ((did-change-predicate
           (lambda (conn)
             (let ((kind (xlsp-capability conn
                           xlsp-struct-server-capabilities-text-document-sync
                           xlsp-struct-text-document-sync-options-change)))
               (and kind (not (equal kind xlsp-text-document-sync-kind/none))))))
          (did-save-predicate
           (lambda (conn)
             (xlsp-capability conn
               xlsp-struct-server-capabilities-text-document-sync
               xlsp-struct-text-document-sync-options-save)))
          (did-open-close-predicate
           (lambda (conn)
             (xlsp-capability conn
               xlsp-struct-server-capabilities-text-document-sync
               xlsp-struct-text-document-sync-options-open-close))))
      (dolist (entry
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
                  (lambda (&rest _args) ; for the closure
                    (lambda () ; for the actual hook
                      "Dispose connection if last registered buffer."
                      (when-let ((conn (xlsp-connection-get (current-buffer))))
                        (oset conn buffers (delq (current-buffer)
                                                 (cl-remove-if-not
                                                  #'buffer-live-p
                                                  (oref conn buffers))))
                        (unless (oref conn buffers)
                          (xlsp-connection-remove (current-buffer))))))
                  2]
                 [before-revert-hook
                  ,did-open-close-predicate
                  xlsp-did-close-text-document]
                 [after-revert-hook
                  ,did-open-close-predicate
                  xlsp-did-open-text-document]))
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
      (with-current-buffer
          (get-buffer-create (format " *%s events*" (eieio-oref conn 'name)))
        (buffer-disable-undo)
        (setq buffer-read-only t)
        (eieio-oset conn '-events-buffer (current-buffer))))))

(put 'xlsp-workspace-configuration 'safe-local-variable 'listp) ; see Commentary

;;;###autoload
(define-globalized-minor-mode global-xlsp-mode
  xlsp-mode
  (lambda ()
    (when (and (buffer-file-name)
               (alist-get major-mode xlsp-invocations))
      (xlsp-mode)))
  :group 'xlsp
  :version "29.1")

(provide 'xlsp)

;;; xlsp.el ends here
