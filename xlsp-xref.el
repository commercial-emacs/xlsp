;;; xlsp-xref.el -*- lexical-binding: t; -*-

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

;;; Code:

(require 'xlsp-utils)
(require 'xlsp-struct)
(require 'xref)

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar xlsp--xref-backend-identifier-completion-table)))

(declare-function xlsp-do-request-workspace-symbols "xlsp")

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'xlsp)))
  "Succumb to Monnier's Rube Goldberg contraption.
Laboriously deconstructed in Ftry_completion(), Fall_completions() in minibuf.c."
  (xlsp-get-closure
    nil
    xlsp--xref-backend-identifier-completion-table
    (apply-partially
     (cl-function
      (lambda (buffer* state* current _pred action
               &aux (prevailing* (car state*)) (matches* (cdr state*)))
        (unless (string-prefix-p prevailing* current)
          ;; MATCHES* should remain a good superset of matches
          ;; if CURRENT merely extends PREVAILING*.  Otherwise, re-query.
          (when-let ((prospective ; don't store null results for empty query
                      (mapcar #'xlsp-struct-workspace-symbol-name
                              (xlsp-do-request-workspace-symbols buffer* current))))
            (setcar state* current)
            (setcdr state* prospective)
            (setq prevailing* (car state*)
                  matches* (cdr state*))))
        (let ((matches (cl-remove-if-not
                        (apply-partially #'string-prefix-p current)
                        matches*)))
          (cl-case action
            ;; from Ftry_completion()
            ((nil)
             (cond ((null matches)
                    nil)
                   ((= 1 (length matches))
                    t)
                   (t
                    (seq-reduce (lambda (so-far candidate)
                                  (if (< (length candidate)
                                         (length so-far))
                                      candidate
                                    so-far))
                                (cdr matches)
                                (car matches)))))
            ;; from Fall_completion(), yes, a list of t for `cl-case'.
            ((t)
             matches)))))
     (current-buffer) (cons "" nil))))

(provide 'xlsp-xref)
