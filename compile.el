(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(load "init-requires")

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0 1)
