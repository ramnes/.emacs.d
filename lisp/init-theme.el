;;
;; init-theme.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

;; Line number (from http://bit.ly/1bUYyIp)
(setq linum-format 'dynamic)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; Appearance
(column-number-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(hlinum-activate)
(line-number-mode 1)
(menu-bar-mode 0)
(diff-mode)

;; Colors
(defvar bg "Grey16")
(defvar fg "SkyBlue1")
(defvar err "Black")

;; Emacs base faces
(set-face-background 'diff-added-face nil)
(set-face-background 'diff-file-header-face nil)
(set-face-background 'diff-header-face nil)
(set-face-background 'diff-removed-face nil)
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
(set-face-background 'vertical-border nil)

(set-face-bold-p 'font-lock-function-name-face 1)
(set-face-bold-p 'font-lock-keyword-face 1)
(set-face-bold-p 'font-lock-type-face 1)
(set-face-bold-p 'minibuffer-prompt 1)

(set-face-foreground 'diff-added-face "AquaMarine3")
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
(set-face-background 'hl-line bg)
(set-face-foreground 'highlight nil)

;; Linum faces
(set-face-background 'linum nil)
(set-face-background 'linum-highlight-face bg)
(set-face-bold-p 'linum-highlight-face 1)
(set-face-foreground 'linum bg)
(set-face-foreground 'linum-highlight-face fg)

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
