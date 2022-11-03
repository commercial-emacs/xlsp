;;; xlsp-server.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'xlsp-utils)
(require 'xlsp-struct)

(defcustom xlsp-server-invocations
  (quote ((c-mode . "clangd")
          (c++-mode . "clangd")
          (objc-mode . "clangd")
          (rust-mode . "rust-analyzer")
          (python-mode . "pyright-langserver --stdio")))
  "Alist values must begin with an executable, e.g., clangd.
If you need to set environment variables,
try \"env FOO=foo bash -c \\='echo $FOO\\='\"."
  :group 'xlsp
  :type '(alist :key-type symbol :value-type string))

(provide 'xlsp-server)
