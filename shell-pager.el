;;; shell-pager.el --- A pager for shells  -*- lexical-binding: t -*-


;;; Commentary:
;;

(require 'em-prompt)

;;; Code:

(defvar-local shell-pager--config nil)

(defvar shell-pager-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Add editing commands.
    map))

(define-derived-mode shell-pager-mode fundamental-mode "shell pager"
  "Major mode for shell paging."
  :keymap shell-pager-mode-map)

(defvar shell-pager-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'shell-pager-next)
    (define-key map (kbd "p") #'shell-pager-previous)
    map))

(define-minor-mode shell-pager-view-mode
  "Similar to `view-mode` but for shell pager buffers."
  :lighter " shell pager view"
  :keymap shell-pager-view-mode-map
  (unless (derived-mode-p 'shell-pager-mode)
    (user-error "Not in view pager buffer"))
  (setq buffer-read-only shell-pager-view-mode))

(defun shell-pager ()
  "Open the pager for current shell."
  (interactive)
  (unless (derived-mode-p 'eshell-mode)
    (user-error "Not in eshell"))
  (let ((shell-buffer (current-buffer))
        (entry (shell-pager--eshell-current-item)))
    (with-current-buffer (shell-pager--buffer)
      (shell-pager-mode)
      (shell-pager-view-mode +1)
      (setq shell-pager--config
            (shell-pager--resolve-shell-buffer shell-buffer))
      (let ((inhibit-read-only t))
        (shell-pager--initialize entry))
      (switch-to-buffer (current-buffer)))))

(defun shell-pager-next ()
  "Show next interaction (command / output)."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let* ((get-next (or (map-elt shell-pager--config :next)
                       (error "No way to get next item")))
         (shell-buffer (shell-pager--shell-buffer))
         (window (get-buffer-window shell-buffer))
         (next (with-current-buffer shell-buffer
                 (if window
                     (with-selected-window window
                       (funcall get-next)))
                 (funcall get-next))))
    (shell-pager--initialize next)
    next))

(defun shell-pager-previous ()
  "Show previous interaction (command / output)."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let* ((get-previous (or (map-elt shell-pager--config :previous)
                           (error "No way to get previous item")))
         (shell-buffer (shell-pager--shell-buffer))
         (window (get-buffer-window shell-buffer))
         (previous (with-current-buffer shell-buffer
                     (if window
                         (with-selected-window window
                           (funcall get-previous)))
                     (funcall get-previous))))
    (shell-pager--initialize previous)
    previous))

(defun shell-pager--initialize (item)
  "Initialize pager with ITEM.

Item is of the form:

\(:command \"ls\"
 :output \"shell-pager.el\")"
  (let ((inhibit-read-only t))
    (save-excursion
      (erase-buffer)
      (insert
       (or (map-elt item :command) "")
       "\n\n"
       (or (map-elt item :output) "")))))

(cl-defun shell-pager--make-config (&key shell-buffer
                                         next previous)
  "Make pager config.

Requires SHELL-BUFFER as well as NEXT and PREVIOUS functions."
  (let ((config))
    (when shell-buffer
      (setq config
            (map-insert config
                        :shell-buffer shell-buffer)))
    (when next
      (setq config
            (map-insert config
                        :next next)))
    (when previous
      (setq config
            (map-insert config
                        :previous previous)))
    config))

(defun shell-pager--resolve-shell-buffer (buffer)
  "Resolve BUFFER to a pager config."
  (with-current-buffer buffer
    (cond ((derived-mode-p 'eshell-mode)
           (shell-pager--make-config
            :shell-buffer buffer
            :next #'shell-pager--eshell-next
            :previous #'shell-pager--eshell-previous))
          (t
           (error "Don't know how to page %s" major-mode)))))

(defun shell-pager--shell-buffer ()
  "Get the pager's shell buffer."
  (or (map-elt shell-pager--config :shell-buffer)
      (error "No shell available")))

(defun shell-pager--buffer ()
  "Get the available shell pager buffer."
  (get-buffer-create "*shell pager*"))

(defun shell-pager--eshell-next ()
  "Move `eshell' to next prompt and return item.

Item is of the form:

\(:command \"ls\"
 :output \"shell-pager.el\")"
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (eshell-next-prompt)
  (shell-pager--eshell-current-item))

(defun shell-pager--eshell-previous ()
  "Move `eshell' to previous prompt and return item.

Item is of the form:

\(:command \"ls\"
 :output \"shell-pager.el\")"
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (eshell-previous-prompt)
  (shell-pager--eshell-current-item))

(defun shell-pager--eshell-current-item ()
  "Return current eshell item.

Item is of the form:

\(:command \"ls\"
 :output \"shell-pager.el\")"
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (save-excursion
    (let ((command-start)
          (command-end)
          (command)
          (output-start)
          (output-end)
          (output))
      (when (re-search-backward eshell-prompt-regexp nil t)
        (setq command-start (match-end 0))
        (goto-char command-start)
        (end-of-line)
        (setq command-end (point))
        (setq command (buffer-substring-no-properties
                       command-start
                       command-end)))
      (forward-line)
      (setq output-start (point))
      (when (re-search-forward eshell-prompt-regexp nil t)
        (setq output-end (match-beginning 0))
        (setq output (buffer-substring-no-properties
                      output-start
                      output-end)))
      (when command
        (list :command (string-trim command)
              :output (when output
                        (string-trim output)))))))

;;; shell-pager.el ends here
