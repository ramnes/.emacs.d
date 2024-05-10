;;
;; init-theme.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

(column-number-mode t)
(menu-bar-mode 0)
(diff-mode)
(set-display-table-slot standard-display-table 'wrap ?\…)

;; Colors
(defvar background "Grey16")
(defvar foreground "SkyBlue1")
(defvar foreground-light "LightSkyBlue1")
(defvar error "Grey16")
(defvar warning "Grey16")

;; Emacs base faces
(set-face-background 'diff-added nil)
(set-face-background 'diff-file-header nil)
(set-face-background 'diff-header nil)
(set-face-background 'diff-indicator-added nil)
(set-face-background 'diff-indicator-removed nil)
(set-face-background 'diff-removed nil)
(set-face-background 'ediff-current-diff-A "DarkRed")
(set-face-background 'ediff-current-diff-B "DarkGreen")
(set-face-background 'ediff-current-diff-C "DarkGoldenrod")
(set-face-background 'ediff-even-diff-A error)
(set-face-background 'ediff-even-diff-B error)
(set-face-background 'ediff-even-diff-C error)
(set-face-background 'ediff-fine-diff-A "Red")
(set-face-background 'ediff-fine-diff-B "Green")
(set-face-background 'ediff-fine-diff-C "Goldenrod")
(set-face-background 'ediff-odd-diff-A error)
(set-face-background 'ediff-odd-diff-B error)
(set-face-background 'ediff-odd-diff-C error)
(set-face-background 'highlight background)
(set-face-background 'isearch foreground)
(set-face-background 'isearch-fail "SteelBlue3")
(set-face-background 'lazy-highlight "SkyBlue3")
(set-face-background 'magit-diff-added nil)
(set-face-background 'magit-diff-added-highlight warning)
(set-face-background 'magit-diff-context-highlight background)
(set-face-background 'magit-diff-file-heading-highlight background)
(set-face-background 'magit-diff-hunk-heading nil)
(set-face-background 'magit-diff-hunk-heading-highlight background)
(set-face-background 'magit-diff-removed nil)
(set-face-background 'magit-diff-removed-highlight warning)
(set-face-background 'mode-line background)
(set-face-background 'mode-line-inactive background)
(set-face-background 'popup-face background)
(set-face-background 'popup-menu-face background)
(set-face-background 'popup-menu-face background)
(set-face-background 'query-replace foreground)
(set-face-background 'region background)
(set-face-background 'smerge-markers nil)
(set-face-background 'smerge-mine background)
(set-face-background 'smerge-other background)
(set-face-background 'smerge-refined-change background)
(set-face-background 'vertical-border nil)

(set-face-bold-p 'diff-added 1)
(set-face-bold-p 'diff-indicator-added 1)
(set-face-bold-p 'diff-indicator-removed 1)
(set-face-bold-p 'diff-removed 1)
(set-face-bold-p 'font-lock-constant-face 1)
(set-face-bold-p 'font-lock-function-name-face 1)
(set-face-bold-p 'font-lock-keyword-face 1)
(set-face-bold-p 'font-lock-type-face 1)
(set-face-bold-p 'font-lock-variable-name-face 1)
(set-face-bold-p 'magit-diff-added 1)
(set-face-bold-p 'magit-diff-added-highlight 1)
(set-face-bold-p 'magit-diff-removed 1)
(set-face-bold-p 'magit-diff-removed-highlight 1)
(set-face-bold-p 'minibuffer-prompt 1)
(set-face-bold-p 'smerge-markers 1)

(set-face-foreground 'diff-added "BrightGreen")
(set-face-foreground 'diff-context nil)
(set-face-foreground 'diff-header "Yellow")
(set-face-foreground 'diff-indicator-added "BrightGreen")
(set-face-foreground 'diff-indicator-removed "BrightRed")
(set-face-foreground 'diff-removed "BrightRed")
(set-face-foreground 'font-lock-builtin-face foreground-light)
(set-face-foreground 'font-lock-comment-face "IndianRed3")
(set-face-foreground 'font-lock-constant-face "Magenta")
(set-face-foreground 'font-lock-function-name-face "SteelBlue3")
(set-face-foreground 'font-lock-keyword-face foreground-light)
(set-face-foreground 'font-lock-preprocessor-face "Magenta")
(set-face-foreground 'font-lock-string-face "AquaMarine3")
(set-face-foreground 'font-lock-type-face "AquaMarine3")
(set-face-foreground 'font-lock-variable-name-face nil)
(set-face-foreground 'isearch nil)
(set-face-foreground 'isearch-fail nil)
(set-face-foreground 'link foreground)
(set-face-foreground 'magit-diff-added "BrightGreen")
(set-face-foreground 'magit-diff-added-highlight "BrightGreen")
(set-face-foreground 'magit-diff-context nil)
(set-face-foreground 'magit-diff-context-highlight nil)
(set-face-foreground 'magit-diff-file-heading "Yellow")
(set-face-foreground 'magit-diff-hunk-heading "Yellow")
(set-face-foreground 'magit-diff-hunk-heading-highlight "Yellow")
(set-face-foreground 'magit-diff-removed "BrightRed")
(set-face-foreground 'magit-diff-removed-highlight "BrightRed")
(set-face-foreground 'minibuffer-prompt foreground)
(set-face-foreground 'mode-line foreground)
(set-face-foreground 'mode-line-inactive foreground)
(set-face-foreground 'popup-face foreground)
(set-face-foreground 'popup-menu-face foreground)
(set-face-foreground 'region foreground)
(set-face-foreground 'smerge-markers "Yellow")
(set-face-foreground 'vertical-border background)

;; Flymake faces
(set-face-attribute 'flymake-error nil :inherit nil)
(set-face-attribute 'flymake-warning nil :inherit nil)
(set-face-background 'flymake-error error)
(set-face-background 'flymake-warning warning)
(set-face-foreground 'flymake-error nil)
(set-face-foreground 'flymake-warning nil)

;; Company faces
(set-face-background 'company-tooltip "Black")
(set-face-background 'company-tooltip-annotation background)
(set-face-background 'company-tooltip-selection background)
(set-face-background 'company-tooltip-scrollbar-thumb "Black")
(set-face-background 'company-tooltip-scrollbar-track background)
(set-face-foreground 'company-tooltip "White")
(set-face-foreground 'company-tooltip-annotation foreground)
(set-face-foreground 'company-tooltip-common foreground-light)
(set-face-foreground 'company-tooltip-common-selection foreground)
(set-face-foreground 'company-tooltip-selection foreground)
(set-face-foreground 'company-preview-common foreground)

;; Highlight faces
(set-face-foreground 'highlight nil)

;; 80col faces
(set-face-attribute 'column-enforce-face nil :inherit nil :underline nil)
(set-face-background 'column-enforce-face error)

;; Web-mode faces
(set-face-attribute 'web-mode-block-delimiter-face nil :inherit nil)
(set-face-attribute 'web-mode-builtin-face nil :inherit nil)
(set-face-attribute 'web-mode-html-tag-face nil :inherit font-lock-function-name-face)
(set-face-attribute 'web-mode-symbol-face nil :inherit font-lock-constant-face)
(set-face-foreground 'web-mode-html-attr-name-face nil)
(set-face-foreground 'web-mode-html-tag-bracket-face nil)
(set-face-foreground 'web-mode-html-tag-face nil)
(set-face-foreground 'web-mode-symbol-face nil)

;; Rainbow delimiters faces
(set-face-foreground 'rainbow-delimiters-depth-1-face "White")
(set-face-foreground 'rainbow-delimiters-depth-2-face "SkyBlue1")
(set-face-foreground 'rainbow-delimiters-depth-3-face "CadetBlue1")
(set-face-foreground 'rainbow-delimiters-depth-4-face "Turquoise1")
(set-face-foreground 'rainbow-delimiters-depth-5-face "DeepSkyBlue1")
(set-face-foreground 'rainbow-delimiters-depth-6-face "DeepSkyBlue2")
(set-face-foreground 'rainbow-delimiters-depth-7-face "DodgerBlue1")
(set-face-foreground 'rainbow-delimiters-depth-8-face "DodgerBlue3")
(set-face-foreground 'rainbow-delimiters-depth-9-face "DodgerBlue4")

;; MMM-Mode faces (only for Python docstrings for now)
(set-face-background 'mmm-default-submode-face nil)
(set-face-foreground 'mmm-default-submode-face "AquaMarine3")

;; helm
(set-face-background 'helm-candidate-number nil)
(set-face-background 'helm-match nil)
(set-face-background 'helm-selection background)
(set-face-background 'helm-source-header background)
(set-face-bold-p 'helm-candidate-number 1)
(set-face-foreground 'helm-candidate-number foreground)
(set-face-foreground 'helm-match foreground-light)
(set-face-foreground 'helm-selection foreground)
(set-face-foreground 'helm-source-header foreground)

;; helm-swoop
(set-face-background 'helm-swoop-target-line-face background)
(set-face-foreground 'helm-swoop-target-line-face nil)

;; reStructuredText
(set-face-bold 'rst-level-1 t)
(set-face-bold 'rst-level-2 t)
(set-face-bold 'rst-level-3 t)
(set-face-bold 'rst-level-4 t)
(set-face-bold 'rst-level-5 t)
(set-face-bold 'rst-level-6 t)
(set-face-background 'rst-level-1 nil)
(set-face-background 'rst-level-2 nil)
(set-face-background 'rst-level-3 nil)
(set-face-background 'rst-level-4 nil)
(set-face-background 'rst-level-5 nil)
(set-face-background 'rst-level-6 nil)

;; git-gutter
(custom-set-variables
 '(git-gutter:added-sign "┃")
 '(git-gutter:deleted-sign "┃")
 '(git-gutter:modified-sign "┃")
 '(git-gutter:unchanged-sign "┃"))

(set-face-background 'git-gutter:unchanged nil)
(set-face-foreground 'git-gutter:unchanged background)
(set-face-foreground 'git-gutter:modified "orange")
(set-face-foreground 'git-gutter:added "AquaMarine3")
(set-face-foreground 'git-gutter:deleted "IndianRed3")
(set-face-foreground 'git-gutter:separator background)

;; diff
(custom-set-faces
 '(diff-added ((t (:inherit diff-changed :extend t :foreground "DarkGreen" :weight bold))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "DarkGreen" :foreground "BrightGreen"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "DarkRed" :foreground "BrightRed"))))
 '(diff-removed ((t (:inherit diff-changed :extend t :foreground "DarkRed" :weight bold))))
 '(magit-section-highlight ((t (:extend t :background "grey16")))))
