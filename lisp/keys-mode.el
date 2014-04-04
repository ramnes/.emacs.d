;;
;; keys-mode.el in ramnes/.emacs.d 
;; <contact@ramnes.eu>
;;

(defvar keys-mode-map (make-keymap) "keys-mode keymap")

(define-key keys-mode-map "\C-c\C-c" 'comment-or-uncomment-region)
(define-key keys-mode-map "\C-c\C-r" 'replace-regexp)
(define-key keys-mode-map "\C-c\C-s" 'replace-string)
(define-key keys-mode-map "\C-l" 'goto-line)
(define-key keys-mode-map "\C-x\C-b" 'buffer-menu)
(define-key keys-mode-map "\C-x\C-k" 'kill-buffer-and-window)
(define-key keys-mode-map "\C-xb" 'buffer-menu)
(define-key keys-mode-map "\C-xk" 'kill-buffer)

(define-minor-mode keys-mode
  "Overriding key bindings" t " Keys" 'keys-mode-map)

(provide 'keys-mode)
