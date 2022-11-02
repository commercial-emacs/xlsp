;;; xlsp-server.el -*- lexical-binding: t; -*-

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
