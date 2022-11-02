;;; xlsp-struct.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Command Line Systems

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

(require 'json)
(eval-and-compile
  (require 'xlsp-utils)

  (defconst xlsp-requests (with-temp-buffer
                            (save-excursion
			      (insert-file-contents
			       (expand-file-name
				"_requests.el"
				(file-name-directory (or load-file-name ".")))))
			    (read (current-buffer))))
  (defconst xlsp-notifications (with-temp-buffer
                                 (save-excursion
				   (insert-file-contents
				    (expand-file-name
				     "_notifications.el"
                                     (file-name-directory (or load-file-name ".")))))
                                 (read (current-buffer))))
  (defconst xlsp-structures (with-temp-buffer
                              (save-excursion
				(insert-file-contents
				 (expand-file-name
				  "_structures.el"
                                  (file-name-directory (or load-file-name ".")))))
			      (read (current-buffer))))
  (defconst xlsp-enumerations (with-temp-buffer
				(save-excursion
				  (insert-file-contents
				   (expand-file-name
				    "_enumerations.el"
                                    (file-name-directory (or load-file-name ".")))))
				(read (current-buffer))))

  (defun xlsp-structure (alist)
    (let-alist alist
      (concat "xlsp-struct-" (xlsp-hyphenate .name))))

  (defun xlsp-request (alist)
    (let-alist alist
      (concat "xlsp-request-" (xlsp-hyphenate .method))))

  (defun xlsp-notification (alist)
    (let-alist alist
      (concat "xlsp-notification-" (xlsp-hyphenate .method))))

  (defun xlsp-enumeration (alist)
    (let-alist alist
      (concat "xlsp-" (xlsp-hyphenate .name))))

  (defun xlsp-structure-type (alist)
    (let-alist alist
      (cond ((equal .kind "reference")
             ;; could be an enumeration but intern-soft won't work
             (intern (concat "xlsp-struct-" (xlsp-hyphenate .name))))
            (.items
             (seq-map #'xlsp-structure-type .items))
            (t (intern (xlsp-hyphenate (or .name .kind)))))))

  (defun xlsp-property-type (alist)
    "Supposedly SLOT-OPTION :type only used for documentation."
    (let-alist alist
      (if (equal "array" .type.kind)
          (list 'list-of (xlsp-structure-type .type.element))
        (xlsp-structure-type .type))))

  (defun xlsp-namespace (variable namespace)
    (concat (xlsp-hyphenate namespace) "/" (xlsp-hyphenate variable)))

  (defun xlsp-top-sort (structures)
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
       (let ((namespace (xlsp-enumeration entry)))
         (eval
          `(progn
             ,@(seq-map
                (lambda (entry)
                  (let-alist entry
                    `(defconst ,(intern (xlsp-namespace .name namespace))
                       ,.value ,.documentation)))
                .values))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "MarkupKind" (alist-get 'name x)))
   ;;              xlsp-enumerations))
   xlsp-enumerations)

  (defmacro xlsp--bail (type)
    "Bail on multiple inheritance for now."
    `(if (listp ,type) (car ,type) ,type))

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(cl-defstruct (,(intern (xlsp-structure entry))
                        ,@(when-let ((extends .extends))
                            `((:include
                               ,(xlsp--bail (xlsp-structure-type (aref extends 0)))))))
           ,@(when (stringp .documentation) (list .documentation))
           ,@(cl-mapcan #'identity
                        (seq-map
                         (lambda (parent)
                           (when-let ((type (intern-soft (xlsp-structure parent))))
                             (cl-remove-if-not #'cdr (cl-struct-slot-info type))))
                         (seq-drop .extends 1)))
           ,@(seq-map
              (lambda (alist)
                (let-alist alist
                  `(,(make-symbol (xlsp-hyphenate .name))
                    nil
                    :type ,(xlsp-property-type alist)
                    ,@(when (or .optional .documentation)
                        (list :documentation
                              (string-trim-right
                               (concat (if .optional "Optional. " "")
                                       (or .documentation ""))))))))
              .properties)))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "RenameOptions"
   ;;                                 (alist-get 'name x)))
   ;;              xlsp-structures))
   (xlsp-top-sort xlsp-structures))

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(defconst ,(intern (xlsp-request entry)) ',(intern .method)))
       (eval
        `(cl-defstruct (,(intern (xlsp-request entry))
                        (:copier nil))
           ,@(let ((doc (string-trim-left
                         (concat (or .documentation "")
                                 (if .messageDirection
                                     (concat " (" .messageDirection ")")
                                   "")))))
               (unless (zerop (length doc)) (list doc)))
           (params
            nil
            :read-only t
            :type ,(xlsp--bail (xlsp-structure-type .params)))
           ,@(when .registrationOptions
               `((registration-options
                  nil
                  :read-only t
                  :type ,(xlsp--bail
                          (xlsp-structure-type .registrationOptions)))))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "workspace/executeCommand" (alist-get 'method x)))
   ;;              xlsp-requests))
   xlsp-requests)

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(defconst ,(intern (xlsp-notification entry)) ',(intern .method)))
       (eval
        `(cl-defstruct (,(intern (xlsp-notification entry))
                        (:copier nil))
           ,@(let ((doc (string-trim-left
                         (concat (or .documentation "")
                                 (if .messageDirection
                                     (concat " (" .messageDirection ")")
                                   "")))))
               (unless (zerop (length doc)) (list doc)))
           (params
            nil
            :read-only t
            :type ,(xlsp--bail (xlsp-structure-type .params)))
           ,@(when .registrationOptions
               `((registration-options
                  nil
                  :read-only t
                  :type ,(xlsp--bail (xlsp-structure-type .registrationOptions)))))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "telemetry/event"
   ;;                                 (alist-get 'method x)))
   ;;              xlsp-notifications))
   xlsp-notifications))

(defconst xlsp-struct-empty (make-hash-table :size 1))

(defun xlsp-literal (&rest args)
  (apply #'list args))

(defun xlsp-array (&rest args)
  (apply #'vector args))

(defun xlsp-jsonify (obj)
  "Go from xlsp-struct to json-object-type (plist)."
  (cond
   ((listp obj) ; xlsp-literal qua plist
    obj)
   ((vectorp obj)
    (apply json-array-type (seq-map #'xlsp-jsonify obj)))
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
                        result (xlsp-unhyphenate slot t) (xlsp-jsonify value)))))
      (or result xlsp-struct-empty)))))

(defun xlsp-unjsonify (struct-type json)
  "Go from json-object-type (plist) to xlsp-struct.
Brutal without byte compilation."
  (if (not (get struct-type 'cl--class))
      (unless (eq json-false json)
        json)
    (let ((json-object-type 'plist)
          (slots (cl-remove-if-not #'cdr (cl-struct-slot-info struct-type)))
          arguments)
      (dolist (slot slots)
        (cl-destructuring-bind (sym _ &key type &allow-other-keys)
            slot
          (when-let ((our-kw (intern (concat ":" (symbol-name sym))))
                     (their-kw (intern (concat ":" (xlsp-unhyphenate (symbol-name sym)
                                                                     :as-slot))))
                     (value (ignore-errors (plist-get json their-kw))))
            (setq arguments
                  (nconc arguments
                         (list our-kw
                               (pcase type
                                 (`(list-of ,property-type)
                                  (apply #'xlsp-array
                                         (seq-map
                                          (lambda (elem)
                                            (xlsp-unjsonify property-type elem))
                                          value)))
                                 ((pred listp)
                                  (cond ((not (listp value))
                                         ;; assume non-struct or-type
                                         (xlsp-unjsonify
                                          (seq-find
                                           (lambda (or-type)
                                             (not (get or-type 'cl--class)))
                                           type)
                                          value))
                                        (t
                                         ;; pick the or-type that
                                         ;; results in a non-trivial
                                         ;; struct with a non-nil
                                         ;; field.
                                         (or
                                          (cl-some
                                           (lambda (or-type)
                                             (let ((cand (xlsp-unjsonify or-type value)))
                                               (cl-some
                                                (lambda (slot)
                                                  (and
                                                   (funcall
                                                    (intern (format "%s-%s" or-type slot))
                                                    cand)
                                                   cand))
                                                (mapcar #'car
                                                        (cl-remove-if-not #'cdr (cl-struct-slot-info or-type))))))
                                           (cl-remove-if-not
                                            (lambda (or-type)
                                              (get or-type 'cl--class))
                                            type))
                                          (xlsp-unjsonify (car type) value)))))
                                 (_ (xlsp-unjsonify type value)))))))))
      (apply (intern (format "make-%s" struct-type)) arguments))))

(defconst xlsp-default-struct-client-capabilities
  (make-xlsp-struct-client-capabilities
   :workspace (make-xlsp-struct-workspace-client-capabilities
               :workspace-edit (make-xlsp-struct-workspace-edit-client-capabilities
                                :document-changes t)
               :did-change-watched-files (make-xlsp-struct-did-change-watched-files-client-capabilities
                                          :dynamic-registration t
                                          :relative-pattern-support json-false)
               :configuration t
               :workspace-folders t)
   :text-document (make-xlsp-struct-text-document-client-capabilities
                   :synchronization (make-xlsp-struct-text-document-sync-client-capabilities
                                     :did-save t)
                   :completion (make-xlsp-struct-completion-client-capabilities)
                   :hover (make-xlsp-struct-hover-client-capabilities
                           :content-format (xlsp-array xlsp-markup-kind/plain-text
                                                       xlsp-markup-kind/markdown))
                   :signature-help (make-xlsp-struct-signature-help-client-capabilities
                                    :signature-information
                                    (xlsp-literal
                                     :parameterInformation
                                     (xlsp-literal :labelOffSupport t)
                                     :activeParameterSupport t))
                   :declaration (make-xlsp-struct-declaration-client-capabilities
                                 :link-support t)
                   :definition (make-xlsp-struct-definition-client-capabilities
                                :link-support t)
                   :type-definition (make-xlsp-struct-type-definition-client-capabilities
                                     :link-support t)
                   :implementation (make-xlsp-struct-implementation-client-capabilities
                                    :link-support t)
                   :document-symbol (make-xlsp-struct-document-symbol-client-capabilities
                                     :hierarchical-document-symbol-support t
                                     :symbol-kind
                                     (xlsp-literal
                                      :valueSet
                                      (apply
                                       #'xlsp-array
                                       (mapcar
                                        (lambda (x)
                                          (symbol-value
                                           (intern-soft
                                            (xlsp-namespace x "xlsp-symbol-kind"))))
                                        '("file" "module" "namespace" "package" "class"
                                          "method" "property" "field" "constructor"
                                          "enum" "interface" "function" "variable"
                                          "constant" "string" "number" "boolean" "array"
                                          "object" "key" "null" "enum-member" "struct"
                                          "event" "operator" "type-parameter")))))
                   :code-action (make-xlsp-struct-code-action-client-capabilities
                                 :code-action-literal-support
                                 (xlsp-literal
                                  :codeActionKind
                                  (xlsp-literal
                                   :valueSet
                                   (apply
                                    #'xlsp-array
                                    (mapcar
                                     (lambda (x)
                                       (symbol-value
                                        (intern-soft
                                         (xlsp-namespace x "xlsp-code-action-kind"))))
                                     '("quick-fix" "refactor" "refactor-extract"
                                       "refactor-inline" "refactor-rewrite"
                                       "source" "source-organize-imports"))))))
                   :publish-diagnostics (make-xlsp-struct-publish-diagnostics-client-capabilities
                                         :related-information json-false
                                         :code-description-support json-false
                                         :tag-support
                                         (xlsp-literal
                                          :valueSet
                                          (apply
                                           #'xlsp-array
                                           (mapcar
                                            (lambda (x)
                                              (symbol-value
                                               (intern-soft
                                                (xlsp-namespace x "xlsp-diagnostic-tag"))))
                                            '("unnecessary" "deprecated"))))))))

(provide 'xlsp-struct)
