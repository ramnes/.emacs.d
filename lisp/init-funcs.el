;;
;; init-funcs.el in ramnes/.emacs.d/lisp
;; <contact@ramnes.eu>
;;

(defun isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in reverse if negative."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

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

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

;; copilot + company-mode
(defun complete-or-indent ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(defun copilot-cycle-to-next-or-first-completion ()
  "Cycle to next completion, or to the first one if there is no more available."
  (interactive)
  (when (copilot--overlay-visible)
    (copilot--get-completions-cycling
     (lambda (result)
       (unless copilot--completion-cache
         (setq copilot--completion-cache result))
       (let ((completions (plist-get result :completions)))
         (if (seq-empty-p completions)
             (message "No completion is available.")
           (let ((idx (mod (+ copilot--completion-idx 1)
                           (length completions))))
             (when (= idx 0)
               (message "Back to first completion."))
             (setq copilot--completion-idx idx)
             (let ((completion (elt completions idx)))
               (copilot--show-completion completion)))))))))

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
  (if (or (< 160 (window-width))
          (< (* 2 (window-height)) (window-width)))
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
  (let ((ipython-window (get-buffer-window "*ipython*")))
    (if ipython-window
        (select-window ipython-window)
      (windnew-auto)
      (set-buffer "*ipython*")))
  (kill-buffer)
  (load-ipython (or filename origin)))

(defun windnew-ipython ()
  "Create a new IPython buffer from current buffer or reload it"
  (interactive)
  (if (string= (buffer-name) "*ipython*")
      (reload-ipython)
    (if (get-buffer "*ipython*")
        (reload-ipython (buffer-file-name))
      (windnew-auto)
      (load-ipython (buffer-file-name)))))

(defun xsel-copy ()
  (interactive)
  (if (region-active-p)
      (progn
	(shell-command-on-region (region-beginning) (region-end) "copy")
	(deactivate-mark))))

(defun xsel-paste ()
  (interactive)
  (insert (shell-command-to-string "paste")))

;; helm-xref
(defun helm-xref-format-candidate-git-or-relative (file line summary)
  "Same as `helm-xref-format-candidate-short', but display git path if possible, relative path otherwise."
  (let ((relative-file-name
         (if (projectile-project-p)
             (file-relative-name file (projectile-project-root))
           (file-relative-name file (file-name-directory (buffer-file-name))))))
    (concat
     (propertize relative-file-name 'font-lock-face 'helm-xref-file-name)
     (when (string= "integer" (type-of line))
       (concat
        ":"
        (propertize (int-to-string line)
                    'font-lock-face 'helm-xref-line-number)))
     ":"
     summary)))
