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

(defun windnew-ipython ()
  "Create a new IPython window and import previous buffer in it"
  ;; TODO: check if already in an *ipython* buffer; if so, reload it
  (interactive)
  (let ((filename (buffer-file-name)))
    (windnew-auto)
    (if (and filename (file-exists-p filename))
	(progn
	  (set-buffer
           (make-term "ipython" "/usr/bin/env" nil "ipython" filename
                      "--TerminalIPythonApp.force_interact=True"))
	  (term-mode)
	  (term-char-mode)
	  (switch-to-buffer "*ipython*"))
      (term "/usr/bin/ipython"))))

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
