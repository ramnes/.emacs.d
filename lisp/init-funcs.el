;;
;; init-funcs.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

;; Replay last shell-command or call shell-command
(defun last-shell-command ()
  (interactive)
  (if shell-command-history
      (shell-command (car shell-command-history))
    (call-interactively 'shell-command)))

;; Improve Emacs split*
(defun windnew-left ()
  "Create a new window at the left the current window"
  (interactive)
  (split-window-horizontally))

(defun windnew-up ()
  "Create a new window above the current window"
  (interactive)
  (split-window-vertically))

(defun windnew-right ()
  "Create a new window at the right of the current window"
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun windnew-down ()
  "Create a new window below the current window"
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun windnew-auto ()
  "Create a new window and decide automatically where it is placed"
  (interactive)
  (if (< (* 2 (window-height)) (window-width))
      (windnew-right)
    (windnew-down)))

(defun windnew-delete ()
  "Delete the latest windnew (atm, just delete the current window)"
  (interactive)
  (delete-window))
