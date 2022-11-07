;;; xlsp-compilation.el -*- lexical-binding: t; -*-

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
(defvar company-lighter)
(defvar company-lighter-base)
(declare-function company-grab-symbol "company")
(declare-function company-in-string-or-comment "company")
(declare-function company-mode "company")
(declare-function company--contains "company-capf")
(declare-function company-call-frontends "company")
(declare-function company-update-candidates "company")

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar xlsp-synchronize-closure)
    (defvar xlsp-did-change-text-document)
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

(provide 'xlsp-compilation)

;;; xlsp-compilation.el ends here
