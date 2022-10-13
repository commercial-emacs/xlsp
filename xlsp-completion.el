;;; xlsp-completion.el -*- lexical-binding: t; -*-

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

;; Emacs's completion story is one of recalcitrant tradition.
;;
;; Its completion model "capf" came with Blandy's initial revision in
;; 1991.  It received a facelift in 2008 but the bones remained
;; largely intact.
;;
;; Preservationists laboriously retrofit any new feature to the capf
;; model, which lacks async and couldn't have foreseen Microsoft's
;; Language Server Protocol (LSP).  In particular, capf keys off a
;; client-determined prefix, whilst LSP's notion of the prefix
;; originates from the server.

;;; Code:

(require 'cl-lib)

(defun xlsp-completion-in-region (buffer beg end)
  "No-frills minibuffer completion, like `lisp-complete-symbol'."
  (interactive (cons (current-buffer)
	             (or (when-let ((c (bounds-of-thing-at-point 'symbol)))
                           (list (car c) (cdr c)))
                         (list (point) (min (point-max) (1+ (point)))))))
  (when-let ((candidates (xlsp-completion-salvo buffer beg)))
    (completion-in-region beg end candidates)))

(defun xlsp-completion-position (buffer pos)
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

(defun xlsp-completion-salvo (buffer pos)
  "One-shot learning."
  (let ((conn (xlsp-connection-get buffer))
        (params (make-xlsp-struct-completion-params
                 :text-document (make-xlsp-struct-text-document-identifier
                                 :uri (xlsp-urify (buffer-file-name buffer)))
                 :position (let ((send-pos (xlsp-completion-position buffer pos)))
                             (make-xlsp-struct-position
                              :line (car send-pos)
                              :character (cdr send-pos)))
                 :context (make-xlsp-struct-completion-context
                           :trigger-kind xlsp-completion-trigger-kind/invoked))))
    (jsonrpc-request conn 'textDocument/completion (xlsp-jsonify params) :deferred t)))

;;;###autoload
(defun xlsp-completion-at-point ()
  "To be add-hook'ed to `completion-at-point-functions'."
  (unless (nth 4 (syntax-ppss)) ; not within a comment
    (let* ((trigger-chars (-> (lsp--capability-for-method "textDocument/completion")
                              (lsp:completion-options-trigger-characters?)))
           (bounds-start (or (-some--> (cl-first (bounds-of-thing-at-point 'symbol))
                               (save-excursion
                                 (ignore-errors
                                   (goto-char (+ it 1))
                                   (while (lsp-completion--looking-back-trigger-characterp
                                           trigger-chars)
                                     (cl-incf it)
                                     (forward-char))
                                   it)))
                             (point)))
           result done?
           (candidates
            (lambda ()
              (lsp--catch 'input
                  (let ((lsp--throw-on-input lsp-completion-use-last-result)
                        (same-session? (and lsp-completion--cache
                                            ;; Special case for empty prefix and empty result
                                            (or (cl-second lsp-completion--cache)
                                                (not (string-empty-p
                                                      (plist-get (cddr lsp-completion--cache) :prefix))))
                                            (equal (cl-first lsp-completion--cache) bounds-start)
                                            (s-prefix?
                                             (plist-get (cddr lsp-completion--cache) :prefix)
                                             (buffer-substring-no-properties bounds-start (point))))))
                    (cond
                     ((or done? result) result)
                     ((and (not lsp-completion-no-cache)
                           same-session?
                           (listp (cl-second lsp-completion--cache)))
                      (setf result (apply #'lsp-completion--filter-candidates
                                          (cdr lsp-completion--cache))))
                     (t
                      (-let* ((resp (lsp-request-while-no-input
                                     "textDocument/completion"
                                     (plist-put (lsp--text-document-position-params)
                                                :context (lsp-completion--get-context trigger-chars))))
                              (completed (and resp
                                              (not (and (lsp-completion-list? resp)
                                                        (lsp:completion-list-is-incomplete resp)))))
                              (items (lsp--while-no-input
                                       (--> (cond
                                             ((lsp-completion-list? resp)
                                              (lsp:completion-list-items resp))
                                             (t resp))
                                         (if (or completed
                                                 (seq-some #'lsp:completion-item-sort-text? it))
                                             (lsp-completion--sort-completions it)
                                           it)
                                         (-map (lambda (item)
                                                 (lsp-put item
                                                          :_emacsStartPoint
                                                          (or (lsp-completion--guess-prefix item)
                                                              bounds-start)))
                                               it))))
                              (markers (list bounds-start (copy-marker (point) t)))
                              (prefix (buffer-substring-no-properties bounds-start (point)))
                              (lsp-completion--no-reordering (not lsp-completion-sort-initial-results)))
                        (lsp-completion--clear-cache same-session?)
                        (setf done? completed
                              lsp-completion--cache (list bounds-start
                                                          (cond
                                                           ((and done? (not (seq-empty-p items)))
                                                            (lsp-completion--to-internal items))
                                                           ((not done?) :incomplete))
                                                          :lsp-items nil
                                                          :markers markers
                                                          :prefix prefix)
                              result (lsp-completion--filter-candidates
                                      (cond (done?
                                             (cl-second lsp-completion--cache))
                                            (lsp-completion-filter-on-incomplete
                                             (lsp-completion--to-internal items)))
                                      :lsp-items items
                                      :markers markers
                                      :prefix prefix))))))
                (:interrupted lsp-completion--last-result)
                (`,res (setq lsp-completion--last-result res))))))
      (list
       bounds-start
       (point)
       (lambda (probe pred action)
         (if (eq action 'metadata)
             '(metadata (category . lsp-capf)
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
           (complete-with-action action (funcall candidates) probe pred)))
       :annotation-function #'lsp-completion--annotate
       :company-kind #'lsp-completion--candidate-kind
       :company-deprecated #'lsp-completion--candidate-deprecated
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (goto-char bounds-start)
         (and (lsp-completion--looking-back-trigger-characterp trigger-chars) t))
       :company-match #'lsp-completion--company-match
       :company-doc-buffer (-compose #'lsp-doc-buffer
                                     #'lsp-completion--get-documentation)
       :exit-function
       (-rpartial #'lsp-completion--exit-fn candidates)))))


(provide 'xlsp-completion)

;;; xlsp-completion.el ends here
