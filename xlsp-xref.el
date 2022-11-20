;;; xlsp-xref.el -*- lexical-binding: t; -*-

(require 'xlsp-utils)
(require 'xlsp-struct)
(require 'xref)

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar xlsp-xref-backend-identifier-completion-table)))

(declare-function xlsp-do-request-workspace-symbols "xlsp")
(declare-function xlsp-do-request-definition "xlsp")
(declare-function xlsp-message "xlsp")
(declare-function seq-keep "xlsp")

(defsubst xlsp-xref-re-ceiling (identifier)
  "We want the occurrence of IDENTIFIER spanning point.
Failing that we want the one before, or failing that, after."
  (let ((bounded-regex (concat "\\_<" (regexp-quote identifier) "\\_>"))
        case-fold-search)
    (save-excursion
      (let ((after (save-excursion
                     (let (after)
                       (while (and (setq after (re-search-forward bounded-regex nil t))
                                   (nth 4 (syntax-ppss after))))
                       after))))
        ;; Ugly: open-closed intervals of re-search-forward confuse me.
        (if (and after (<= (- after (point)) (length identifier)))
            (point) ; special case for point at IDENTIFIER's start
          (goto-char (if after (- after (length identifier)) (point-max)))
          (or (save-excursion
                (let (before)
                  (while (and (setq before (re-search-backward bounded-regex nil t))
                              (nth 4 (syntax-ppss before))))
                  before))
              (unless (eq (point) (point-max))
                (point))))))))

(cl-defmethod xref-backend-definitions ((_backend (eql xlsp)) identifier)
  "Note LSP keys off IDENTIFIER at a precise file location."
  (save-excursion
    (when-let ((where (xlsp-xref-re-ceiling identifier))
               (locations (xlsp-do-request-definition
                           (current-buffer) (goto-char where)))
               (location-type (type-of (car locations)))
               (uri-getter (if (eq location-type 'xlsp-struct-location-link)
                               #'xlsp-struct-location-link-target-uri
                             #'xlsp-struct-location-uri))
               (range-getter (if (eq location-type 'xlsp-struct-location-link)
                                 #'xlsp-struct-location-link-target-range
                               #'xlsp-struct-location-range)))
      (seq-keep
       (lambda (location)
         (when-let ((file (xlsp-unurify (funcall uri-getter location)))
                    (range (funcall range-getter location))
                    (make-match
                     (lambda (buffer)
                       (with-current-buffer buffer
                         ;; clangd has produced locations
                         ;; that did not reflect dutifully reported
                         ;; didChange notifications.  clangd
                         ;; only got it right after a didSave.  Ergo,
                         ;; the when-let.
                         (if-let ((beg (xlsp-our-pos
                                        (current-buffer)
                                        (xlsp-struct-range-start range)))
                                  (end (xlsp-our-pos
                                        (current-buffer)
                                        (xlsp-struct-range-end range))))
                             (progn
                               (goto-char beg)
                               (xref-make-match
                                identifier
                                (xref-make-file-location file (line-number-at-pos) (current-column))
                                (- end beg)))
                           (prog1 nil
                             (xlsp-message
                              "xref-backend-definitions: No such range %S in %s"
                              range file)))))))
           (if-let ((buffer (find-buffer-visiting file)))
               ;; Else case won't have unsaved changes to FILE
               (funcall make-match buffer)
             (when (file-readable-p file)
               (with-temp-buffer
                 ;; a tad fatuous since `find-file' likely to ensue.
                 (insert-file-contents-literally file)
                 (funcall make-match (current-buffer)))))))
       locations))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'xlsp)))
  "Succumb to Monnier's Rube Goldberg contraption.
See Ftry_completion(), Fall_completions() in minibuf.c.
Avoid seeing the metadata/category/overrides/styles fiasco in minibuffer.el."
  (xlsp-get-closure
    xlsp-xref-backend-identifier-completion-table
    (apply-partially
     (cl-function
      (lambda (buffer* state* prefix _pred action
               &aux (prefix* (car state*)) (matches* (cdr state*)))
        ;; MATCHES* should remain a good superset of matches if PREFIX
        ;; merely extends PREFIX*, that is, my prevailing matches for
        ;; PREFIX* "cat" should have included matches for PREFIX
        ;; "cattle".  Otherwise re-query.
        (unless (string-prefix-p prefix* prefix)
          ;; Most servers would balk at empty string query,
          ;; so don't store null result for that degenerate case.
          (when-let ((matches
                      (mapcar #'xlsp-struct-workspace-symbol-name
                              (xlsp-do-request-workspace-symbols buffer* prefix))))
            (setcar state* prefix)
            (setcdr state* matches)
            (setf prefix* (car state*)
                  matches* (cdr state*))))

        ;; Note LSP keys off symbols at their precise file locations.
        ;; So for now, exclude symbols missing from the current buffer.
        (let ((matches (cl-remove-if-not
                        (lambda (match)
                          (and (string-prefix-p prefix match)
                               (with-current-buffer buffer*
                                 (xlsp-xref-re-ceiling match))))
                        matches*)))
          (cl-case action
            ;; from Ftry_completion()
            ((nil)
             (when matches
               (let* ((suffix-f (lambda (s) (substring s (length prefix))))
                      (against (funcall suffix-f (car matches)))
                      (suffixes (mapcar suffix-f (cdr matches))))
                 (concat prefix
                         (cl-loop for i from 0 below (length against)
                                  for c = (aref against i)
                                  while (cl-every
                                         (lambda (s)
                                           (and (<= i (1- (length s)))
                                                (eql (aref s i) c)))
                                         suffixes)
                                  concat (char-to-string c))))))
            ;; from Fall_completion(), yes, a list of t for `cl-case'.
            ((t)
             matches)))))
     (current-buffer) (cons "" nil))))

(provide 'xlsp-xref)
