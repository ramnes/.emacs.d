;;
;; init-keys.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

(defvar keys-mode-map (make-keymap) "keys-mode keymap")

(defun add-key (keys func)
  (define-key keys-mode-map (kbd keys) func))

(add-key "C-c C-c" 'comment-or-uncomment-region)
(add-key "C-c C-i" 'indent-region)
;; last-shell-command is defined in init.el
(add-key "C-c C-j" 'last-shell-command)
(add-key "C-c C-r" 'replace-regexp)
(add-key "C-c C-s" 'replace-string)

(add-key "C-f" 'fill-region)
(add-key "C-l" 'goto-line)
(add-key "C-v" 'sort-lines)

(add-key "C-x C-b" 'buffer-menu)
(add-key "C-x C-k" 'kill-buffer-and-window)
(add-key "C-x b" 'buffer-menu)
(add-key "C-x k" 'kill-buffer)

(add-key "M-<down>" 'windmove-down)
(add-key "M-<left>" 'windmove-left)
(add-key "M-<right>" 'windmove-right)
(add-key "M-<up>" 'windmove-up)

;; windnew* are defined in lisp/init-funcs.el
(add-key "M-C-<right>" 'windnew-right)
(add-key "M-C-<down>" 'windnew-down)
(add-key "M-C-<left>" 'windnew-left)
(add-key "M-C-<up>" 'windnew-up)
(add-key "M-C-j" 'windnew-auto)
(add-key "M-<DEL>" 'windnew-delete)
(add-key "M-C-i" 'windnew-auto-ipython-with-imports)

;; dependent of move-border
;; https://github.com/ramnes/move-border
(add-key "M-S-<down>" 'move-border-down)
(add-key "M-S-<left>" 'move-border-left)
(add-key "M-S-<right>" 'move-border-right)
(add-key "M-S-<up>" 'move-border-up)

(define-minor-mode keys-mode
  "Overriding key bindings" t " Keys" 'keys-mode-map)

(keys-mode 1)
