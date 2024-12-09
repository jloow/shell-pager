;;; shell-pager.el --- A pager for shells  -*- lexical-binding: t -*-


;;; Commentary:
;;

(require 'em-prompt)
(require 'comint)

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
  (let* ((move-next (or (map-elt shell-pager--config :next)
                        (error "No way to move to next item")))
         (get-current (or (map-elt shell-pager--config :current)
                          (error "No way to get current item")))
         (shell-buffer (shell-pager--shell-buffer))
         (window (get-buffer-window shell-buffer))
         (next (with-current-buffer shell-buffer
                 (if window
                     (with-selected-window window
                       (funcall move-next))
                   (funcall move-next))
                 (funcall get-current))))
    (shell-pager--initialize next)
    next))

(defun shell-pager-previous ()
  "Show previous interaction (command / output)."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let* ((get-previous (or (map-elt shell-pager--config :previous)
                           (error "No way to get previous item")))
         (get-current (or (map-elt shell-pager--config :current)
                          (error "No way to get current item")))
         (shell-buffer (shell-pager--shell-buffer))
         (window (get-buffer-window shell-buffer))
         (previous (with-current-buffer shell-buffer
                     (if window
                         (with-selected-window window
                           (funcall get-previous))
                       (funcall get-previous))
                     (funcall get-current))))
    (shell-pager--initialize previous)
    previous))

(defun shell-pager--current ()
  "Show next interaction (command / output)."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let* ((get-current (or (map-elt shell-pager--config :current)
                          (error "No way to get current item")))
         (shell-buffer (shell-pager--shell-buffer)))
    (when get-current
      (with-current-buffer shell-buffer
        (funcall get-current)))))

(defun shell-pager--shell-items ()
  "Return a list of all items."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let* ((shell-buffer (shell-pager--shell-buffer))
         (move-next (or (map-elt shell-pager--config :next)
                        (error "No way to move to next item")))
         (get-current (or (map-elt shell-pager--config :current)
                          (error "No way to get current item")))
         (first-item)
         (items '()))
    (with-current-buffer shell-buffer
      (save-excursion
        (goto-char (point-min))
        (setq first-item (funcall get-current))
        (unless (or (string-empty-p (map-elt first-item :command))
                    (string-empty-p (map-elt first-item :output)))
          (push first-item items))
        (while (funcall move-next)
          (let ((current-item (funcall get-current)))
            (push current-item items)))
        (nreverse items)))))

(defun shell-pager--position ()
  "Return the position in history.

In the form:

1 out of 3 = (1 . 3)."
  (let* ((current (shell-pager--current))
         (history (shell-pager--shell-items))
         (pos (seq-position history current #'equal)))
    (when (and current history pos)
      (cons (1+ pos) (length history)))))

(defun shell-pager--history-label ()
  "Return the position in history of the primary shell buffer."
  (let ((pos (or (shell-pager--position)
                 (cons 1 1))))
    (propertize (format "[%d/%d]\n\n" (car pos) (cdr pos))
                'ignore t
                'read-only t
                'face font-lock-comment-face
                'rear-nonsticky t)))


(defun shell-pager--initialize (item)
  "Initialize pager with ITEM.

Item is of the form:

\(:command \"ls\"
 :output \"shell-pager.el\")"
  (let ((inhibit-read-only t))
    (save-excursion
      (erase-buffer)
      (insert
       (shell-pager--history-label)
       (propertize (or (map-elt item :command) "")
                           'rear-nonsticky t
                           'command t
                           'face 'comint-highlight-input)
       "\n"
       (or (map-elt item :output) "")))))

(cl-defun shell-pager--make-config (&key shell-buffer
                                         current
                                         next previous)
  "Make pager config.

Requires SHELL-BUFFER as well as CURRENT, NEXT and PREVIOUS functions."
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
    (when current
      (setq config
            (map-insert config
                        :current current)))
    config))

(defun shell-pager--resolve-shell-buffer (buffer)
  "Resolve BUFFER to a pager config."
  (with-current-buffer buffer
    (cond ((derived-mode-p 'eshell-mode)
           (shell-pager--make-config
            :shell-buffer buffer
            :next #'shell-pager--eshell-next
            :previous #'shell-pager--eshell-previous
            :current #'shell-pager--eshell-current-item))
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

Return non-nil if point moved"
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (let ((point (point))
        (new-point))
    (save-excursion
      (eshell-next-prompt)
      (when (and (not (eq point (point)))
                 (not (eobp)))
        (setq new-point (point))))
    (when new-point
      (goto-char new-point))))

(defun shell-pager--eshell-previous ()
  "Move `eshell' to previous prompt and return item.

Return non-nil if point moved"
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (let ((line (line-number-at-pos))
        (new-point))
    (save-excursion
      (eshell-previous-prompt)
      ;; eshell may have a banner (no prompt)
      ;; go to point-min instead.
      (when (and (eq line (line-number-at-pos))
                 (save-excursion
                   (goto-char (point-min))
                   (not (eq (line-number-at-pos) line))))
        (setq new-point (point-min)))
      (unless (eq line (line-number-at-pos))
        (setq new-point (point))))
    (when new-point
      (goto-char new-point))))

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
        (if-let ((match (text-property-search-forward 'field nil nil t)))
            (progn
              (setq command-end (prop-match-beginning match))
              (setq command (buffer-substring-no-properties
                             command-start
                             command-end)))
          (setq command-start nil)))
      (when command-end
        (goto-char command-end))
      (setq output-start (point))
      (when (re-search-forward eshell-prompt-regexp nil t)
        (setq output-end (match-beginning 0))
        (setq output (buffer-substring
                      output-start
                      output-end)))
      (when (or command output)
        (list :command command
              :command-start command-start
              :command-end command-end
              :output output
              :output-start output-start
              :output-end output-end)))))

;;; shell-pager.el ends here
