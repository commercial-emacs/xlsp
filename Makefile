include epkg.mk
epkg.mk:
	emacs --batch -l package -f package-initialize -l epkg -f epkg-copy-mk

export EMACS ?= $(shell which emacs)
EPKG_EL := $(filter-out _%,$(shell git ls-files *.el))
EPKG_TEST_EL := $(shell git ls-files tests/*.el)
EPKG_FILES := $(shell git ls-files *.el)
EPKG_MAIN := xlsp.el

.DEFAULT_GOAL := compile

.PHONY: schema
schema: language-server-protocol/_specifications/lsp/3.17/metaModel/metaModel.json
	$(EMACS) -Q --batch -L . -l xlsp-utils -l pp --eval "                  \
(with-temp-buffer                                                              \
  (save-excursion (insert-file-contents \"$^\"))                               \
  (let ((alist (json-parse-buffer :object-type (quote alist)                   \
                                  :null-object nil                             \
                                  :false-object :json-false)))                 \
    (dolist (entry alist)                                                      \
      (cl-destructuring-bind (what . schema)                                   \
          entry                                                                \
        (unless (memq what (quote (metaData)))                                 \
          (with-temp-file (format \"_%s.el\" (xlsp-hyphenate (symbol-name what))) \
            (princ \";; -*- lexical-binding: t -*-\n\n\" (current-buffer)) \
            (pp schema (current-buffer))))))))"

README.rst: README.in.rst xlsp.el
	grep ';;' xlsp.el \
	  | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	  | sed -e 's/^\s*;;\s\?/   /g' \
	  | bash readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: clean
clean:
	git clean -dfX

.PHONY: microsoft
microsoft:
	git submodule add https://github.com/microsoft/language-server-protocol.git

.PHONY: compile
compile: epkg-compile

.PHONY: test
test: compile epkg-test

.PHONY: install
install: epkg-install
