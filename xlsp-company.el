;;; xlsp-company.el -*- lexical-binding: t; -*-

(require 'xlsp-utils)
(require 'xlsp-struct)
(require 'company)
(require 'company-capf)               ; for company--contains

(defconst xlsp-company-kind-alist
  (seq-map
   (lambda (entry)
     (let-alist entry
       (cons .value .name)))
   (alist-get 'values
              (seq-find (lambda (x) (equal "CompletionItemKind" (alist-get 'name x)))
                        xlsp-enumerations))))

(provide 'xlsp-company)
