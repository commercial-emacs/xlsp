export EMACS ?= $(shell which emacs)

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
            (princ \";; -*- lisp-data -*-\n\n\" (current-buffer))                \
            (pp schema (current-buffer))))))))"

README.rst: README.in.rst xlsp.el
	grep ';;' xlsp.el \
	  | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	  | sed -e 's/^\s*;;*\s*/   /g' \
	  | bash readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: clean
clean:
	git clean -dfX

.PHONY: microsoft
microsoft:
	git submodule add https://github.com/microsoft/language-server-protocol.git

.PHONY: compile
compile:
	$(EMACS) -batch -L . \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile xlsp*.el ; \
	  (ret=$$? ; rm *.elc && exit $$ret)

.PHONY: test
test: compile
	$(EMACS) -batch -l mytest.el -f ert-run-tests-batch

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	mkdir dist
	cp -p $$(git ls-files *.el) dist

.PHONY: install
install: dist
	$(EMACS) -Q --batch -f package-initialize \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  -f package-refresh-contents \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote xlsp) package-alist)))" \
	  --eval "(with-current-buffer (dired \"dist\") \
	            (package-install-from-buffer))"
