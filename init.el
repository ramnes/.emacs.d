;;
;; init.el in ramnes/.emacs.d
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
(load "init-funcs")
(load "init-keys")
(load "init-theme")

;; Flymake
(when (load "flymake" t)
  (defun flymake-pycheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace)))
      (list "pycheck" (list temp-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pycheck-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

;; see: http://stackoverflow.com/questions/7299893
(defadvice flymake-start-syntax-check-process
    (after flymake-pyflakes-init () activate compile)
  (set-process-query-on-exit-flag ad-return-value nil))

;; Autocompletion
(add-to-list 'ac-dictionary-directories
             (expand-file-name "~/.emacs.d/auto-complete/dict"))
(setq ac-comphist-file  "~/.emacs.d/auto-complete/ac-comphist.dat")
(ac-config-default)
(ac-linum-workaround)

(setq ac-auto-start 1)
(setq ac-ignore-case nil)
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

;; Fix broken Shift-up
(define-key input-decode-map "\e[1;2A" [S-up])

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Disable electric-indent-mode (by default in 24.4)
(electric-indent-mode 0)

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

;; Avoid annoying "Active processes exist" prompt when quitting Emacs
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))

;; Disable autopair when term-mode
(add-hook 'term-mode-hook
	  #'(lambda ()
	      (setq autopair-dont-activate t)	;; for emacs < 24
	      (autopair-mode -1)))		;; for emacs >= 24

;; Kernel coding style
(setq c-default-style "linux"
      c-basic-offset 8)

;; MMM-Mode for python-mode and rst-mode on docstrings
(setq mmm-global-mode 'maybe)
(setq mmm-parse-when-idle t)
(mmm-add-classes
 '((python-rst
    :submode rst-mode
    :front "\"\"\""
    :back "\"\"\""
    :end-not-begin t)))
(mmm-add-mode-ext-class 'python-mode nil 'python-rst)

;; Split vertically by default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Replace yes or no by y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Accept to use <return> in multiple-cursors
;; Use C-g to quit
(eval-after-load "multiple-cursors-core"
  '(progn
     (define-key mc/keymap (kbd "<return>") nil)))

;; Avoid the useless prompt when killing a live process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
