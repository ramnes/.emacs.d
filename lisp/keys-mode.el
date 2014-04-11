;;
;; keys-mode.el in ramnes/.emacs.d 
;; <contact@ramnes.eu>
;;

(defvar keys-mode-map (make-keymap) "keys-mode keymap")

(define-key keys-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)
;; last-shell-command is defined in init.el
(define-key keys-mode-map (kbd "C-c C-j") 'last-shell-command)
(define-key keys-mode-map (kbd "C-c C-r") 'replace-regexp)
(define-key keys-mode-map (kbd "C-c C-s") 'replace-string)

(define-key keys-mode-map (kbd "C-f") 'fill-region)
(define-key keys-mode-map (kbd "C-l") 'goto-line)
(define-key keys-mode-map (kbd "C-v") 'sort-lines)

(define-key keys-mode-map (kbd "C-x C-b") 'buffer-menu)
(define-key keys-mode-map (kbd "C-x C-k") 'kill-buffer-and-window)
(define-key keys-mode-map (kbd "C-x b") 'buffer-menu)
(define-key keys-mode-map (kbd "C-x k") 'kill-buffer)

(define-key keys-mode-map (kbd "M-<down>") 'windmove-down)
(define-key keys-mode-map (kbd "M-<left>") 'windmove-left)
(define-key keys-mode-map (kbd "M-<right>") 'windmove-right)
(define-key keys-mode-map (kbd "M-<up>") 'windmove-up)

;; dependent of move-border
;; https://github.com/ramnes/move-border
(define-key keys-mode-map (kbd "M-S-<down>") 'move-border-down)
(define-key keys-mode-map (kbd "M-S-<left>") 'move-border-left)
(define-key keys-mode-map (kbd "M-S-<right>") 'move-border-right)
(define-key keys-mode-map (kbd "M-S-<up>") 'move-border-up)

(define-minor-mode keys-mode
  "Overriding key bindings" t " Keys" 'keys-mode-map)

(provide 'keys-mode)
