;;
;; init.el in ramnes dotfiles
;; <contact@ramnes.eu>
;;

;; Load packages paths
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Load packages
(load "android-mode")
(load "auto-complete")
(load "auto-complete-config")
(load "column-enforce-mode")
(load "hlinum")
(load "jedi")
(load "jinja2-mode")
(load "lua-mode")
(load "php-mode")
(load "rust-mode")

;; Key bindings
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-c\C-r" 'replace-regexp)
(global-set-key "\C-c\C-s" 'replace-string)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-x\C-k" 'kill-buffer-and-window)
(global-set-key "\C-xb" 'buffer-menu)
(global-set-key "\C-xk" 'kill-buffer)

;; Android
(setq android-mode-avd "AVD")
(setq android-mode-sdk-dir "/opt/android-sdk/")

;; Flymake (from http://www.plope.com/Members/chrism/flymake-mode)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Autocompletion
(add-to-list 'ac-dictionary-directories
             (expand-file-name "~/.emacs.d/auto-complete/dict"))
(setq ac-comphist-file  "~/.emacs.d/auto-complete/ac-comphist.dat")
(ac-config-default)

(setq ac-auto-start 1)
(setq ac-set-trigger-key "TAB")
(setq ac-use-fuzzy 1)
(setq ac-use-quick-help 1)

;; Jedi (Python completion)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;; Show Line/Char
(line-number-mode 1)
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)
(hlinum-activate)

;; Line number (from http://bit.ly/1bUYyIp)
(setq linum-format 'dynamic)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))
(global-linum-mode 1)

;; Remove emacs shyty top menu
(menu-bar-mode 0)

;; Auto revert files (useful when changing branch)
(global-auto-revert-mode 1)

;; Overwrite regions
(delete-selection-mode 1)

;; Auto close braces and so (emacs 24+ only)
(when (>= emacs-major-version 24)
  (electric-pair-mode 1))

;; 80 char rule (for text and prog only)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; Treat Java 1.5 @-style annotations as comments.
(add-hook 'java-mode-hook
	  '(lambda ()
	     (setq c-comment-start-regexp
		   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
	     (modify-syntax-entry
	      ?@ "< b" java-mode-syntax-table)))

;; Global colors
(defvar bg "Grey21")
(defvar fg "Orange")
(defvar err "Grey10")

;; Colors (emacs)
(set-face-background 'mode-line bg)
(set-face-background 'mode-line-inactive bg)
(set-face-background 'popup-face bg)
(set-face-background 'popup-menu-face bg)
(set-face-background 'popup-menu-face bg)
(set-face-background 'query-replace fg)
(set-face-background 'region bg)
(set-face-background 'vertical-border nil)

(set-face-background 'isearch "Orange3")
(set-face-background 'isearch-fail "IndianRed3")
(set-face-background 'lazy-highlight "Tan4")

(set-face-bold-p 'font-lock-function-name-face 1)
(set-face-bold-p 'font-lock-keyword-face 1)
(set-face-bold-p 'font-lock-type-face 1)
(set-face-bold-p 'minibuffer-prompt 1)

(set-face-foreground 'font-lock-variable-name-face nil)
(set-face-foreground 'isearch nil)
(set-face-foreground 'isearch-fail nil)
(set-face-foreground 'minibuffer-prompt fg)
(set-face-foreground 'mode-line fg)
(set-face-foreground 'mode-line-inactive fg)
(set-face-foreground 'popup-face fg)
(set-face-foreground 'popup-menu-face fg)
(set-face-foreground 'region fg)
(set-face-foreground 'vertical-border bg)

(set-face-foreground 'font-lock-builtin-face "SkyBlue1")
(set-face-foreground 'font-lock-comment-face "IndianRed3")
(set-face-foreground 'font-lock-constant-face "Magenta")
(set-face-foreground 'font-lock-function-name-face "SteelBlue3")
(set-face-foreground 'font-lock-keyword-face "SkyBlue1")
(set-face-foreground 'font-lock-preprocessor-face "Magenta")
(set-face-foreground 'font-lock-string-face "SeaGreen3")
(set-face-foreground 'font-lock-type-face "SeaGreen3")

;; Colors (flymake)
(set-face-background 'flymake-errline err)
(set-face-foreground 'flymake-errline nil)

;; Colors (auto-complete)
(set-face-background 'ac-candidate-face "Black")
(set-face-background 'ac-selection-face fg)
(set-face-foreground 'ac-candidate-face "White")
(set-face-foreground 'ac-completion-face fg)

;; Colors (highlight)
(set-face-background 'hl-line bg)
(set-face-foreground 'highlight nil)

;; Colors (linum)
(set-face-background 'linum nil)
(set-face-background 'linum-highlight-face bg)
(set-face-bold-p 'linum-highlight-face 1)
(set-face-foreground 'linum bg)
(set-face-foreground 'linum-highlight-face fg)

;; Colors (80+ chars)
(set-face-attribute 'column-enforce-face nil :inherit nil :underline nil)
(set-face-background 'column-enforce-face err)
