;;
;; compile.el in ramnes/.emacs.d
;; <contact@ramnes.eu>
;;

;; Load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(setq exec-path (append exec-path '("~/.emacs.d/bin")))

;; Load lisp
(load "init-requires")

;; Compile
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0 1)
