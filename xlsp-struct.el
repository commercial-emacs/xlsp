;;; xlsp-struct.el -*- lexical-binding: t; -*-

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

(require 'json)
(eval-and-compile
  (require 'xlsp-hyphenate)
  (defconst xlsp-requests (with-temp-buffer
                           (save-excursion (insert-file-contents "_requests.el"))
                           (read (current-buffer))))
  (defconst xlsp-notifications (with-temp-buffer
                                (save-excursion (insert-file-contents "_notifications.el"))
                                (read (current-buffer))))
  (defconst xlsp-structures (with-temp-buffer
                             (save-excursion (insert-file-contents "_structures.el"))
                             (read (current-buffer))))
  (defconst xlsp-enumerations (with-temp-buffer
                               (save-excursion (insert-file-contents "_enumerations.el"))
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
             (intern (concat "xlsp-struct-" (xlsp-hyphenate .name))))
            (.items
             (xlsp-structure-type (aref .items 0)))
            (t (make-symbol (xlsp-hyphenate (or .name .kind)))))))

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
       (eval
        `(cl-defstruct (,(intern (xlsp-structure entry))
                        ,@(when-let ((extends .extends))
                            `((:include
                               ;; `xlsp-structure-type' returns an intern-less
                               ;; `make-symbol'.
                               ,(intern (symbol-name
                                         (xlsp-structure-type (aref extends 0)))))))
                        (:copier nil))
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
                    :read-only t
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

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(cl-defstruct (,(intern (xlsp-request entry))
                        (:copier nil))
           ,@(when (stringp .documentation) (list .documentation))
           (params
            nil
            :read-only t
            :type ,(xlsp-structure-type .params))
           ,@(when .registrationOptions
               `((registration-options
                  nil
                  :read-only t
                  :type ,(xlsp-structure-type .registrationOptions))))))))
   ;; (make-vector
   ;;  1 (seq-find (lambda (x) (equal "workspace/executeCommand" (alist-get 'method x)))
   ;;              xlsp-requests))
   xlsp-requests)

  (seq-map
   (lambda (entry)
     (let-alist entry
       (eval
        `(cl-defstruct (,(intern (xlsp-notification entry))
                        (:copier nil))
           ,@(when (stringp .documentation) (list .documentation))
           (params
            nil
            :read-only t
            :type ,(xlsp-structure-type .params))
           ,@(when .registrationOptions
               `((registration-options
                  nil
                  :read-only t
                  :type ,(xlsp-structure-type .registrationOptions))))))))
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
  "Go from json-object-type (plist) to xlsp-struct."
  (let ((json-object-type 'plist)
        (slots (cl-remove-if-not #'cdr (cl-struct-slot-info struct-type)))
        arguments)
    (dolist (slot slots)
      (cl-destructuring-bind (sym _ &key type &allow-other-keys)
          slot
        (when-let ((keyword (intern (concat ":" (xlsp-hyphenate (symbol-name sym)))))
                   (value (plist-get json keyword)))
          (setq arguments
                (nconc arguments
                       (list keyword
                             (pcase type
                               (`(list-of ,property-type)
                                (apply #'xlsp-array
                                       (seq-map
                                        (lambda (elem)
                                          (xlsp-unjsonify property-type elem))
                                        value)))
                               ((pred (lambda (type) (get type 'cl--class)))
                                (xlsp-unjsonify type value))
                               (_ value))))))))
    (apply (intern (format "make-%s" struct-type)) arguments)))

(provide 'xlsp-struct)