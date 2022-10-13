;;; xlsp-utils.el -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'url-util)
(require 'jsonrpc)

(defun xlsp-urify (path)
  "RFC3986, like all RFCs, are write-only.
https://microsoft.github.io/language-server-protocol/specifications/\\
lsp/3.17/specification/#uri"
  (let ((url-path-allowed-chars (copy-sequence url-path-allowed-chars)))
    (aset url-path-allowed-chars ?: nil)
    (url-encode-url (concat "file://" (expand-file-name path)))))

(defvar xlsp--hyphenate-data (make-hash-table :test #'equal)
  "Every time you `xlsp-hyphenate', make a backref to original camel.")

(defun xlsp-hyphenate (camel)
  "Break out for `make schema`."
  (let ((word "")
        case-fold-search lifo pc)
    (dolist (c (append camel nil))
      (let ((capital-now (and (>= c ?A) (<= c ?Z)))
            (capital-before (and pc (>= pc ?A) (<= pc ?Z)))
            (letter-before (and pc (string-match-p "[a-zA-Z]" (make-string 1 pc)))))
        (cond ((and capital-before (not capital-now))
               (if (string-match-p "[A-Z].*[A-Z]" word)
                   ;; more than one capital: pinch the loaf
                   (progn
                     (push (downcase (cl-subseq word 0 (1- (length word)))) lifo)
                     (setq word (concat (cl-subseq word (1- (length word)))
                                        (char-to-string c))))
                 (setq word (concat word (char-to-string c)))))
              ((and capital-before capital-now)
               (setq word (concat word (char-to-string c))))
              (t ;; (not capital-before)
               (if (and capital-now letter-before)
                   (progn
                     (unless (zerop (length word))
                       (push (downcase word) lifo))
                     (setq word (char-to-string c)))
                 (setq word (concat word (char-to-string c)))))))
      (setq pc c))
    (when word
      (push (downcase word) lifo))
    (let* ((fifo (reverse lifo))
           (result (mapconcat #'identity fifo "-")))
      (prog1 result
        (puthash result camel xlsp--hyphenate-data)))))

(defun xlsp-unhyphenate (hyphenate &optional slot-p)
  (when-let ((result (gethash hyphenate xlsp--hyphenate-data)))
    (if slot-p
        (concat (downcase (cl-subseq result 0 1))
                (cl-subseq result 1))
      result)))

(provide 'xlsp-utils)

;;; xlsp-utils.el ends here
