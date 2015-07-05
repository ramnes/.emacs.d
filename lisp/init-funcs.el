;;
;; init-funcs.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

(defun last-shell-command ()
  "Replay last `shell-command` or call `shell-command`"
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

;; IPython related stuff
(defun load-ipython (&optional filename)
  "Create a new Ipython buffer, optionnaly running a given file"
  (interactive)
  (set-buffer
   (make-term "ipython" "/usr/bin/env" nil "ipython" (or filename "")
              "--TerminalIPythonApp.force_interact=True"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*ipython*")
  (setq-local origin filename))

(defun reload-ipython (&optional filename)
  (interactive)
  (setq window (get-buffer-window "*ipython*"))
  (if window
      (select-window window)
    (windnew-auto)
    (set-buffer "*ipython*"))
  (if (not filename)
      (setq filename origin))
  (kill-buffer)
  (load-ipython filename))

(defun windnew-ipython ()
  "Create a new IPython buffer from current buffer or reload it"
  (interactive)
  (if (string= (buffer-name) "*ipython*")
      (reload-ipython)
    (if (get-buffer "*ipython*")
        (reload-ipython (buffer-file-name))
      (windnew-auto)
      (load-ipython (buffer-file-name)))))

;; Use xsel for copy/paste, and avoid Emacs super-slow parsing
(defun xsel-copy ()
  (interactive)
  (if (region-active-p)
      (progn
	(shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
	(deactivate-mark))))

(defun xsel-paste ()
  (interactive)
  (insert (shell-command-to-string "xsel -o -b")))
