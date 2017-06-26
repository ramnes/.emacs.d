;;
;; init-theme.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

(column-number-mode t)
(menu-bar-mode 0)
(diff-mode)
(set-display-table-slot standard-display-table 'wrap ?\…)

;; Colors
(defvar bg "Grey24")
(defvar fg "SkyBlue1")
(defvar err "Grey26")

;; Emacs base faces
(set-face-background 'diff-added-face nil)
(set-face-background 'diff-file-header-face nil)
(set-face-background 'diff-header-face nil)
(set-face-background 'diff-removed-face nil)
(set-face-background 'ediff-current-diff-A "DarkRed")
(set-face-background 'ediff-current-diff-B "DarkGreen")
(set-face-background 'ediff-current-diff-C "DarkGoldenrod")
(set-face-background 'ediff-even-diff-A err)
(set-face-background 'ediff-even-diff-B err)
(set-face-background 'ediff-even-diff-C err)
(set-face-background 'ediff-fine-diff-A "Red")
(set-face-background 'ediff-fine-diff-B "Green")
(set-face-background 'ediff-fine-diff-C "Goldenrod")
(set-face-background 'ediff-odd-diff-A err)
(set-face-background 'ediff-odd-diff-B err)
(set-face-background 'ediff-odd-diff-C err)
(set-face-background 'isearch fg)
(set-face-background 'isearch-fail "SteelBlue3")
(set-face-background 'lazy-highlight "SkyBlue3")
(set-face-background 'mode-line bg)
(set-face-background 'mode-line-inactive bg)
(set-face-background 'popup-face bg)
(set-face-background 'popup-menu-face bg)
(set-face-background 'popup-menu-face bg)
(set-face-background 'query-replace fg)
(set-face-background 'region bg)
(set-face-background 'smerge-markers nil)
(set-face-background 'smerge-mine bg)
(set-face-background 'smerge-other bg)
(set-face-background 'smerge-refined-change bg)
(set-face-background 'vertical-border nil)

(set-face-bold-p 'font-lock-function-name-face 1)
(set-face-bold-p 'font-lock-keyword-face 1)
(set-face-bold-p 'font-lock-type-face 1)
(set-face-bold-p 'minibuffer-prompt 1)
(set-face-bold-p 'smerge-markers 1)

(set-face-foreground 'diff-added-face "AquaMarine3")
(set-face-foreground 'diff-context "white")
(set-face-foreground 'diff-removed-face "IndianRed3")
(set-face-foreground 'font-lock-builtin-face "LightSkyBlue1")
(set-face-foreground 'font-lock-comment-face "IndianRed3")
(set-face-foreground 'font-lock-constant-face "Magenta")
(set-face-foreground 'font-lock-function-name-face "SteelBlue3")
(set-face-foreground 'font-lock-keyword-face "LightSkyBlue1")
(set-face-foreground 'font-lock-preprocessor-face "Magenta")
(set-face-foreground 'font-lock-string-face "AquaMarine3")
(set-face-foreground 'font-lock-type-face "AquaMarine3")
(set-face-foreground 'font-lock-variable-name-face nil)
(set-face-foreground 'isearch nil)
(set-face-foreground 'isearch-fail nil)
(set-face-foreground 'minibuffer-prompt fg)
(set-face-foreground 'mode-line fg)
(set-face-foreground 'mode-line-inactive fg)
(set-face-foreground 'popup-face fg)
(set-face-foreground 'popup-menu-face fg)
(set-face-foreground 'region fg)
(set-face-foreground 'smerge-markers "Yellow")
(set-face-foreground 'vertical-border bg)

;; Flymake faces
(set-face-attribute 'flymake-errline nil :inherit nil)
(set-face-background 'flymake-errline err)
(set-face-foreground 'flymake-errline nil)

;; Auto-complete faces
(set-face-background 'ac-candidate-face "Black")
(set-face-background 'ac-selection-face fg)
(set-face-foreground 'ac-candidate-face "White")
(set-face-foreground 'ac-completion-face fg)

;; Highlight faces
(set-face-foreground 'highlight nil)

;; 80col faces
(set-face-attribute 'column-enforce-face nil :inherit nil :underline nil)
(set-face-background 'column-enforce-face err)

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
(set-face-background 'helm-selection fg)
(set-face-background 'helm-source-header bg)
(set-face-foreground 'helm-selection nil)
(set-face-foreground 'helm-source-header fg)

;; helm-swoop
(set-face-background 'helm-swoop-target-line-face bg)
(set-face-foreground 'helm-swoop-target-line-face nil)

;; git-commit-mode
(set-face-bold-p 'git-commit-branch-face t)
(set-face-bold-p 'git-commit-summary-face t)
(set-face-foreground 'git-commit-branch-face "IndianRed2")
(set-face-foreground 'git-commit-comment-face "IndianRed3")
(set-face-foreground 'git-commit-comment-file-face "IndianRed2")
(set-face-foreground 'git-commit-comment-heading-face "IndianRed3")
(set-face-foreground 'git-commit-overlong-summary-face nil)
(set-face-foreground 'git-commit-summary-face nil)

;; reStructuredText
(set-face-bold-p 'rst-level-1 t)
(set-face-bold-p 'rst-level-2 t)
(set-face-bold-p 'rst-level-3 t)
(set-face-bold-p 'rst-level-4 t)
(set-face-bold-p 'rst-level-5 t)
(set-face-bold-p 'rst-level-6 t)
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
(set-face-foreground 'git-gutter:unchanged bg)
(set-face-foreground 'git-gutter:modified "orange")
(set-face-foreground 'git-gutter:added "AquaMarine3")
(set-face-foreground 'git-gutter:deleted "IndianRed3")
(set-face-foreground 'git-gutter:separator bg)
