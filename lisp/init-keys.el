;;
;; init-keys.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;
;; List of functions defined in lisp/init-funcs.el:
;; - last-shell-command
;; - windnew-*
;; - xsel-*

(defvar keys-mode-map (make-keymap) "keys-mode keymap")

(defun add-key (keys func)
  (define-key keys-mode-map (kbd keys) func))

(add-key "C-c C-a" 'mc/mark-all-like-this)
(add-key "C-c C-v" 'mc/edit-lines)
(add-key "C-c C-i" 'indent-region)
(add-key "C-c C-j" 'last-shell-command)
(add-key "C-c RET" 'last-shell-command)
(add-key "C-c C-r" 'replace-regexp)
(add-key "C-c C-s" 'replace-string)
(add-key "C-c /" 'flymake-goto-next-error)

(add-key "C-f" 'fill-region)
(add-key "C-l" 'goto-line)
(add-key "C-v" 'sort-lines)
(add-key "C-b" 'sort-words)
(add-key "C-w" 'kill-region-or-backward-word)

(add-key "C-d <up>" 'smerge-keep-upper)
(add-key "C-d <down>" 'smerge-keep-lower)
(add-key "C-d <left>" 'smerge-prev)
(add-key "C-d <right>" 'smerge-next)
(add-key "C-d p" 'git-gutter:popup-hunk)
(add-key "C-d r" 'git-gutter:revert-hunk)

(add-key "C-x C-b" 'ibuffer)
(add-key "C-x C-k" 'kill-buffer-and-window)
(add-key "C-x k" 'kill-buffer)

(add-key "M-<DEL>" 'windnew-delete)

(add-key "M-<down>" 'windmove-down)
(add-key "M-<left>" 'windmove-left)
(add-key "M-<right>" 'windmove-right)
(add-key "M-<up>" 'windmove-up)

(add-key "M-C-<down>" 'windnew-down)
(add-key "M-C-<left>" 'windnew-left)
(add-key "M-C-<right>" 'windnew-right)
(add-key "M-C-<up>" 'windnew-up)

(add-key "M-C-i" 'windnew-ipython)
(add-key "M-C-j" 'windnew-auto)
(add-key "M-RET" 'windnew-auto)

(add-key "M-C-w" 'xsel-copy)
(add-key "M-C-y" 'xsel-paste)

;; https://github.com/ramnes/move-border
(add-key "M-S-<down>" 'move-border-down)
(add-key "M-S-<left>" 'move-border-left)
(add-key "M-S-<right>" 'move-border-right)
(add-key "M-S-<up>" 'move-border-up)

(add-key "M-s" 'helm-multi-swoop-all)
(add-key "M-m" 'git-messenger:popup-message)
(add-key "M-b" 'vc-annotate)

(define-minor-mode keys-mode
  "Overriding key bindings" t " Keys" 'keys-mode-map)

(keys-mode 1)
