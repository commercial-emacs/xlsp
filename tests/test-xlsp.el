;;; test-xlsp.el -*- lexical-binding: t; -*-

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

(require 'ert-x)
(require 'xlsp)
(require 'log-edit)
(require 'vc-git)

(cl-defmacro test-xlsp-mock-repo ((&rest files) &body body)
  (declare (indent 1))
  `(let* ((dir (make-temp-file "test-xlsp" t))
          (default-directory dir))
     (unwind-protect
         (progn
           (vc-git-create-repo)
           (vc-git-command nil 0 nil "config" "--add" "user.name" "frou")
           (cl-loop for (file . content) in ',files
                    do (with-temp-file (expand-file-name file)
                         (insert (string-trim-left content))))
           (vc-git-register (mapcar #'car ',files))
           (vc-git-checkin (mapcar #'car ',files) "No-Verify: yes
his fooness")
           ,@body)
       (delete-directory dir t))))

(defun test-xlsp-script (beg steps)
  "A single entry of STEPS is of the form (LABEL . PLIST)"
  (apply-partially
   (lambda (state*)
     (save-excursion
       (goto-char (point-max))
       ;; "[%s]%s%s %s:\n%s"
       (save-match-data
         (when-let ((step (car (gv-deref (plist-get state* :queue))))
                    (step-label (car step))
                    (step-plist (cdr step))
                    (match-p (re-search-backward
                              "^\\[?\\([^]]+\\)\\]? (id:\\s-*\\([0-9]+\\))"
                              beg t))
                    (what (match-string 1))
                    (id (string-to-number (match-string 2)))
                    (message (progn
                               (forward-line 1)
                               (read (current-buffer)))))
           (when (and (equal what (plist-get step-plist :what))
                      (cond ((plist-get step-plist :method)
                             (equal (symbol-name
                                     (symbol-value (plist-get step-plist :method)))
                                    (plist-get message :method)))
                            ((plist-get step-plist :ref)
                             (eql id
                                  (plist-get
                                   (alist-get (plist-get step-plist :ref)
                                              (plist-get state* :label-message))
                                   :id)))))
             (pop (gv-deref (plist-get state* :queue)))
             (setf (alist-get step-label
                              (plist-get state* :label-message))
                   message))))))
   (list :queue steps :label-message nil)))

(cl-defmacro test-xlsp-should ((&rest steps)
                               &rest body
                               &key (timeout 5) &allow-other-keys)
  (declare (indent 1))
  (cl-remf body :timeout)
  `(let* ((conn (xlsp-connection-get (current-buffer)))
          (beg (when conn
                 (with-current-buffer (jsonrpc-events-buffer conn)
                   (point-max))))
          (steps ',steps)
          (closure (test-xlsp-script beg (gv-ref steps)))
          (xlsp-advise-logging
           (lambda (conn &rest _args)
             (with-current-buffer (jsonrpc-events-buffer conn)
               (funcall closure)))))
     (unwind-protect
         (progn
           (add-function :after (symbol-function 'jsonrpc--log-event)
                         xlsp-advise-logging
                         `((name . ,(xlsp-advise-tag xlsp-advise-logging))))
           (with-timeout
            (,timeout)
            ,@body
            (while steps
              (accept-process-output nil 0.1)))
           (should-not steps))
       (remove-function (symbol-function 'jsonrpc--log-event)
                        xlsp-advise-logging))))

(ert-deftest test-xlsp-test-basic ()
  (skip-unless (or (executable-find
                    (car (split-string
                          (alist-get 'c-mode xlsp-server-invocations))))
                   (executable-find
                    (setf (alist-get 'c-mode xlsp-server-invocations)
                          "clangd-12"))))
  (skip-unless (executable-find vc-git-program))
  (test-xlsp-mock-repo
      (("foo.c" . "
#include <stdio.h>
#include \"foo.h\"

void main (void) {
  return 0;
}
")
       ("foo.h" . "
#define CONSTANT 5
"))
    ;; Says ert-deftest:
    ;; Macros in BODY are expanded when the test is defined, not when it
    ;; is run.  If a macro (possibly with side effects) is to be tested,
    ;; it has to be wrapped in `(eval (quote ...))'.
    (global-xlsp-mode)
    (eval
     (quote
      (test-xlsp-should
          ((init0 :what "client-request" :method xlsp-request-initialize)
           (init1 :what "server-reply" :ref init0))
        (with-current-buffer (find-file "foo.c")
          (should xlsp-mode)))))
    (with-current-buffer "foo.c"
      (eval
       (quote
        (test-xlsp-should
            ((hover0 :what "client-request" :method xlsp-request-text-document/hover)
             (hover1 :what "server-reply" :ref hover0))
          (should eldoc-mode)
          (ert-simulate-command '(search-forward "main"))
          (ert-run-idle-timers))))
      (should-error ; eldoc-cache forestalls another hover request
       (eval
        (quote
         (test-xlsp-should
             ((hover0 :what "client-request" :method xlsp-request-text-document/hover))
           :timeout 1
           (backward-char))))))))
