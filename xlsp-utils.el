;;; xlsp-utils.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'url-util)
(require 'jsonrpc)

(defgroup xlsp nil
  "Language Server Protocol."
  :prefix "xlsp-"
  :group 'applications)

(defclass xlsp-connection (jsonrpc-process-connection)
  ((ready-p :initform nil :type boolean :documentation "Handshake completed.")
   (capabilities :initform nil)
   (server-info :initform nil)
   ;; if eieio can make this private, then a public accessor can do
   ;; the dirty of `xlsp-conn-files'
   (files :initform nil)
   (watched-files :initform nil)))

(defun xlsp-urify (path)
  "Go from path to RFC3986.
All RFCs are unintelligible, and this one is no exception.
https://microsoft.github.io/language-server-protocol/specifications/\\
lsp/3.17/specification/#uri"
  (let ((url-path-allowed-chars (copy-sequence url-path-allowed-chars)))
    (aset url-path-allowed-chars ?: nil)
    (url-encode-url (concat "file://" (expand-file-name path)))))

(defun xlsp-unurify (uri)
  "Go from RFC3986 to path."
  (url-unhex-string (url-filename (url-generic-parse-url uri))))

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

(defun xlsp-params-type (f method)
  (let* ((struct-type
          (intern
           (funcall f
            (list (cons 'method (xlsp-hyphenate (symbol-name method)))))))
         (slots (cl-remove-if-not #'cdr (cl-struct-slot-info struct-type))))
    (cl-destructuring-bind (_sym _default &key type &allow-other-keys)
        (assoc 'params slots)
      type)))

(defmacro xlsp-register-handler (type method-sym formal-args &rest body)
  "Register handler.
How does `defmacro' get its eldoc to register the following?

\(fn TYPE METHOD-SYM FORMAL-ARGS &optional [DOCSTRING] BODY..."
  (declare (doc-string 4) (indent 3))
  (cl-destructuring-bind (conn params)
      formal-args
    `(cl-defmethod ,(intern (concat "xlsp-handle-" (symbol-name type)))
       (,conn (method (eql ,(symbol-value method-sym))) params-plist)
       "Handle it."
       (let* ((params-type (xlsp-params-type
                            (function
                             ,(intern (concat "xlsp-" (symbol-name type))))
                            method))
              (,params (xlsp-unjsonify params-type params-plist)))
         ,@body))))

(defmacro xlsp-get-closure (name callback)
  "I'll do a lot to avoid defvar proliferation."
  (declare (indent defun))
  `(or (bound-and-true-p ,name)
       (set (make-local-variable ',name) ,callback)))

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

(defmacro xlsp-tack (lst tack)
  `(if (consp ,lst)
       (setcdr (last ,lst) (list ,tack))
     (setf ,lst (list ,tack))))

(provide 'xlsp-utils)

;;; xlsp-utils.el ends here
