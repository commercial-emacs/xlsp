|build-status|

::

   Overview
   --------
   M-x xlsp-mode toggles for the current buffer.
   M-x global-xlsp-mode toggles globally.

   .emacs or init.el configuration
   -------------------------------
   (add-hook 'c-mode-hook #'xlsp-mode) ; activate for c-mode
   -or-
   (global-xlsp-mode) ; activate for any mode in xlsp-server-invocations

   "Nothing is happening."
   -----------------------
   M-x customize-option RET xlsp-server-invocations RET

   Per-workspace configuration
   ---------------------------
   Loitering .dir-locals.el files are a pain in the tuchus.
   If project-specific config is a must, we recommend this pain in
   your .emacs instead:

   (dir-locals-set-class-variables 'my-project-lsp
   '((python-mode
   (xlsp-workspace-configuration
   (:pylsp (:plugins (:jedi_completion (:include_params t :fuzzy t)
   :pylint (:enabled :json-false)))))))

   (dir-locals-set-directory-class "my-project-dir" 'my-project-lsp)

   What the "x" in "xlsp" references
   ---------------------------------
   It is merely a differentiator, just as the "x" in "xemacs" had been
   (contrary to the popular misconception that it referenced X11).

.. |build-status|
   image:: https://github.com/commercial-emacs/xlsp/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/commercial-emacs/xlsp/actions
   :alt: Build Status

Install
=======
::

   git clone https://github.com/commercial-emacs/xlsp.git
   make install
