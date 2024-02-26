;;
;; init.el in ramnes/.emacs.d
;; <contact@ramnes.eu>
;;

;; Do not collect garbage before 50MB to make startup faster
(setq gc-cons-threshold 50000000)

;; Load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(setq exec-path (append exec-path '("~/.emacs.d/bin")))

;; Create go-mode-autoloads if it doesn't exist yet
(if (not (file-exists-p "~/.emacs.d/site-lisp/go-mode/go-mode-autoloads.el"))
    (update-file-autoloads "~/.emacs.d/site-lisp/go-mode/go-mode.el" t
                           "~/.emacs.d/site-lisp/go-mode/go-mode-autoloads.el"))

;; https://github.com/joaotavora/eglot/issues/549
(load-file "~/.emacs.d/site-lisp/project/project.el")

;; https://github.com/jorgenschaefer/elpy/issues/1901
(setq flymake-allowed-file-name-masks '())

;; Load lisp
(load "init-requires")
(load "init-funcs")
(load "init-keys")
(load "init-theme")

;; Don't save backup files in the working directory
(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make isearch use current region if active
(add-hook 'isearch-mode-hook #'isearch-with-region)

;; Flymake
(setq python-flymake-command '("pycheck" "-"))
(add-hook 'prog-mode-hook 'flymake-mode)

;; Auto close braces
(electric-pair-mode)
(electric-indent-mode)
(setq electric-pair-open-newline-between-pairs t)

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
(setq markdown-fontify-code-blocks-natively t)

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))

;; Dockerfile-mode
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; CSS-mode for LESS and Rofi configuration files
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . css-mode))

;; sh-mode for ebuilds
(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))

;; Nix
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; flymake for sh-mode
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

;; YAML-mode
(add-to-list 'auto-mode-alist '("\\.y[a]ml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook (lambda () (flyspell-mode -1)))

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

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

;; Don't over-indent multiline Python function arguments
(setq python-indent-def-block-scale 1)

;; Except for Makefiles
(add-hook 'makefile-mode-hook
   (lambda ()
      (setq indent-tabs-mode t)))

;; Avoid annoying "Active processes exist" prompt when quitting Emacs
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-letf (((symbol-function #'process-list) (lambda ()))) ad-do-it))

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

;; Save without asking when inserting a new word in ispell dictionary
(setq ispell-silently-savep t)

;; flyspell
(add-hook 'text-mode-hook 'flyspell-mode)

;; Automatically save cursor position when leaving a file
(setq-default save-place t)

;; Use flyspell for git-commit-mode
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
(add-hook 'git-commit-mode-hook 'magit-diff-while-committing)

;; git-messenger
(setq git-messenger:show-detail t)

;; popwin
(popwin-mode 1)

;; Enable upcase-region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Save commands history
(savehist-mode 1)

;; Improve desktop-read
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

;; Treat underscores as part of words
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; Activate editorconfig
(editorconfig-mode 1)

;; Show modified lines from git
(global-git-gutter-mode 1)
(custom-set-variables
 '(git-gutter:added-sign "┃")
 '(git-gutter:always-show-separator 1)
 '(git-gutter:deleted-sign "┃")
 '(git-gutter:modified-sign "┃")
 '(git-gutter:unchanged-sign "┃")
 '(git-gutter:update-interval 1))

;; typit
(defun typit ()
  (interactive)
  (require 'typit)
  (setq typit-dict "french.txt")
  (typit-basic-test))

;; helm
(helm-mode 1)
(setq history-delete-duplicates t)

;; If there is no symbol at the cursor, use the last used words instead.
(setq helm-swoop-pattern "")
(setq helm-swoop-pre-input-function
      (lambda ()
        (let (($pre-input (thing-at-point 'symbol)))
          (if (eq (length $pre-input) 0)
              helm-swoop-pattern
            $pre-input))))

;; Go: mimic Python bindings, gofmt before save
(eval-after-load "go-mode"
  '(progn
     (define-key go-mode-map (kbd "C-c .") 'godef-jump)
     (setq gofmt-command "goimports")
     (add-hook 'before-save-hook #'gofmt-before-save)))


;; Markdown: mimic Python bindings
(add-hook 'markdown-mode-hook
          #'(lambda ()
              (define-key markdown-mode-map (kbd "C-c .")
                'markdown-follow-thing-at-point)))

;; Obscure iTerm2 fix
(add-hook 'term-setup-hook
  '(lambda ()
     (define-key function-key-map "\e[1;9A" [M-up])
     (define-key function-key-map "\e[1;9B" [M-down])
     (define-key function-key-map "\e[1;9C" [M-right])
     (define-key function-key-map "\e[1;9D" [M-left])))

;; company-mode
(global-company-mode)
(define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common)

(setq
 company-minimum-prefix-length 2
 company-backends '(company-capf))

;; eglot (LSP)
(setq eglot-sync-connect 0)
(setq eglot-workspace-configuration '((pylsp (plugins (black (enabled . t))))))
(add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)

(define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)

(defun format-or-indent-region (orig &rest args)
  (interactive)
  (if (and (eglot-current-server) (region-active-p))
      (eglot-format (region-beginning) (- (region-end) 1))
    (apply orig args)))

(advice-add 'indent-region :around #'format-or-indent-region)

;; fix diff
(custom-set-faces
 '(diff-added ((t (:inherit diff-changed :extend t :foreground "DarkGreen" :weight bold))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "DarkGreen" :foreground "BrightGreen"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "DarkRed" :foreground "BrightRed"))))
 '(diff-removed ((t (:inherit diff-changed :extend t :foreground "DarkRed" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "slategray"))))
 '(magit-section-highlight ((t (:extend t :background "grey16"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "darkgreen"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "darkred")))))
