;;
;; init.el in ramnes/.emacs.d
;; <contact@ramnes.eu>
;;

;; Load path
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Load lisp
(load "init-requires")
(load "init-funcs")
(load "init-keys")
(load "init-theme")

;; Flymake (from http://www.plope.com/Members/chrism/flymake-mode)
;; updated because of symlinks messing up, see:
;; http://stackoverflow.com/questions/5793839
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)))
      (list "pyflakes" (list temp-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; see: http://stackoverflow.com/questions/7299893
(defadvice flymake-start-syntax-check-process (after flymake-pyflakes-init () activate compile)
  (set-process-query-on-exit-flag ad-return-value nil))

;; Autocompletion
(add-to-list 'ac-dictionary-directories
             (expand-file-name "~/.emacs.d/auto-complete/dict"))
(setq ac-comphist-file  "~/.emacs.d/auto-complete/ac-comphist.dat")
(ac-config-default)
(ac-linum-workaround)

(setq ac-auto-start 1)
(setq ac-set-trigger-key "TAB")
(setq ac-use-fuzzy 1)
(setq ac-use-quick-help 1)

;; Auto close braces (with triple quote support for python)
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))
(autopair-global-mode 1)

;; 80 char rule (for text and prog only)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; Treat Java 1.5 @-style annotations as comments.
(add-hook 'java-mode-hook
	  #'(lambda ()
	      (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
	      (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; Setup android-mode
(setq android-mode-avd "AVD")
(setq android-mode-sdk-dir "/opt/android-sdk/")

;; Markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; CSS-mode for LESS
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))

;; sh-mode for ebuilds
(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))

;; YALM-mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; Jedi (Python completion)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;; Auto revert files (useful when changing branch)
(global-auto-revert-mode 1)

;; Overwrite regions
(delete-selection-mode 1)

;; Hungry deletion
(setq backward-delete-char-untabify-method 'hungry)

;; Scroll one line at time
(setq scroll-conservatively 10000)
