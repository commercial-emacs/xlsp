|build-status|

Install
=======
::

   git clone https://github.com/commercial-emacs/xlsp.git
   make -C xlsp install

Usage
=====
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
   
   What the "x" in "xlsp" references
   ---------------------------------
   It is merely a differentiator, just as the "x" in "xemacs" had been
   (contrary to the popular misconception that it referenced X11).
   
   Credits
   -------
   Logic, some transcribed verbatim, was patterned after GNU eglot.
   Functions missing in emacs-27 and emacs-28 transcribed from GNU emacs-29.

.. |build-status|
   image:: https://github.com/commercial-emacs/xlsp/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/commercial-emacs/xlsp/actions
   :alt: Build Status
