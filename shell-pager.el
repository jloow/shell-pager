;;; shell-pager.el --- A pager for shells  -*- lexical-binding: t -*-


;;; Commentary:
;;

(require 'em-prompt)
(require 'comint)

;;; Code:

(defvar-local shell-pager--config nil)

(defvar shell-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'shell-pager-submit)
    (define-key map (kbd "C-c C-k") #'shell-pager-reset)
    map))

(define-derived-mode shell-pager-mode fundamental-mode "shell pager"
  "Major mode for shell paging."
  :keymap shell-pager-mode-map)

(defvar shell-pager-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'shell-pager-quit)
    (define-key map (kbd "n") #'shell-pager-next)
    (define-key map (kbd "p") #'shell-pager-previous)
    (define-key map (kbd "c") #'shell-pager-compose-command)
    (define-key map (kbd "o") #'shell-pager-other-buffer)
    (define-key map (kbd "C-c C-c") #'shell-pager-interrupt)
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
  (unless (or (derived-mode-p 'eshell-mode)
	      (derived-mode-p 'inferior-ess-r-mode))
    (user-error "Not in a supported shell"))
  (let ((shell-buffer (current-buffer))
        (entry)
	(config))
    (with-current-buffer (shell-pager--buffer)
      (shell-pager-mode)
      (shell-pager-view-mode +1)
      (setq config (shell-pager--resolve-shell-buffer shell-buffer))
      (setq shell-pager--config config)
      (setq entry (with-current-buffer shell-buffer
		    (funcall (map-elt config :current))))
      (let ((inhibit-read-only t))
        (shell-pager--initialize entry)))
    (when (map-elt config :subscribe)
      (funcall (map-elt config :subscribe)
               (shell-pager--buffer)))
    (switch-to-buffer (shell-pager--buffer))))

(defun shell-pager-next ()
  "Show next interaction (command / output)."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let* ((move-next (or (map-elt shell-pager--config :next)
                        (error "No way to move to next item")))
         (get-current (or (map-elt shell-pager--config :current)
                          (error "No way to get current item")))
         (subscribe (map-elt shell-pager--config :subscribe))
         (shell-buffer (shell-pager--shell-buffer))
         (page-buffer (shell-pager--buffer))
         (window (get-buffer-window shell-buffer))
         (last (car (last (shell-pager--shell-items))))
         (next (with-current-buffer shell-buffer
                 (if window
                     (with-selected-window window
                       (funcall move-next))
                   (funcall move-next))
                 (funcall get-current))))
    (shell-pager--initialize next)
    (when (and subscribe
               (equal (map-elt next :command-start)
                      (map-elt last :command-start)))
      (with-current-buffer shell-buffer
        (funcall subscribe page-buffer)))
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
         (page-buffer (shell-pager--buffer))
         (unsubscribe (map-elt shell-pager--config :unsubscribe))
         (shell-buffer (shell-pager--shell-buffer))
         (window (get-buffer-window shell-buffer))
         (previous (with-current-buffer shell-buffer
                     (if window
                         (with-selected-window window
                           (funcall get-previous))
                       (funcall get-previous))
                     (funcall get-current))))
    (when unsubscribe
      (with-current-buffer shell-buffer
        (funcall unsubscribe page-buffer)))
    (shell-pager--initialize previous)
    previous))

(defun shell-pager-compose-command ()
  "Compose a new shell command."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (interactive)
  (unless (and (map-elt shell-pager--config :submit)
               (map-elt shell-pager--config :subscribe))
    (error "Composing not supported for %s"
           (with-current-buffer (map-elt shell-pager--config :shell-buffer)
             major-mode)))
  (shell-pager-view-mode -1)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     (shell-pager--make-compose-buffer-content))))

(defun shell-pager-submit ()
  "Submit composed shell command."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (unless (and (map-elt shell-pager--config :submit)
               (map-elt shell-pager--config :subscribe))
    (error "Composing not supported for %s"
           (with-current-buffer (map-elt shell-pager--config :shell-buffer)
             major-mode)))
  ;; TODO: Confirm with user if aborting is needed.
  (let ((command (shell-pager--compose-text))
        (history-length (length (shell-pager--shell-items)))
        (shell-buffer (map-elt shell-pager--config :shell-buffer))
        (inhibit-read-only t))
    (when (string-empty-p command)
      (user-error "Nothing to send"))
    (erase-buffer)
    (insert
     (shell-pager--make-buffer-content
      :label-override (format "%d/%d"
                              (1+ history-length)
                              (1+ history-length))
      :command command)
     "\n")
    (goto-char (point-min))
    (text-property-search-forward 'command t)
    (shell-pager-view-mode)
    (when (map-elt shell-pager--config :subscribe)
      (with-current-buffer shell-buffer
        (funcall (map-elt shell-pager--config :subscribe)
                 (shell-pager--buffer))))
    (funcall (map-elt shell-pager--config :submit)
             command)))

(defun shell-pager-interrupt ()
  "Interrupt current shell command."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  ;; TODO: Check if busy and confirm interruption with user.
  (let* ((interrupt (or (map-elt shell-pager--config :interrupt)
                        (error "No way to interrupt shell")))
         (shell-buffer (shell-pager--shell-buffer)))
    (when interrupt
      (with-current-buffer shell-buffer
        (funcall interrupt)))))

(defun shell-pager-quit ()
  "Quit shell pager."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

(defun shell-pager-other-buffer ()
  "Show other buffer (the actual shell)."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let ((shell-buffer (or (map-elt shell-pager--config :shell-buffer)
                          (error "No shell available"))))
    (switch-to-buffer shell-buffer)))

(defun shell-pager-reset ()
  "Exit compose buffer."
  (interactive)
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let ((shell-buffer (or (map-elt shell-pager--config :shell-buffer)
                          (error "No shell available"))))
    (with-current-buffer shell-buffer
      (shell-pager))))

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
         (move-previous (or (map-elt shell-pager--config :previous)
                            (error "No way to move to previous item")))
         (get-current (or (map-elt shell-pager--config :current)
                          (error "No way to get current item")))
         (first-item)
         (items '()))
    (with-current-buffer shell-buffer
      (save-excursion
        (goto-char (point-min))
        ;; Normalize moving to first item
        ;; by moving once down and two up.
        (funcall move-next)
        (funcall move-previous)
        (funcall move-previous)
        (setq first-item (funcall get-current))
        (when first-item
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
         (pos (seq-position history current
                            ;; Must compare against locations as
                            ;; commands and even outputs can yield
                            ;; false positives.
                            (lambda (lhs rhs)
                              (or
                               (equal (map-elt lhs :output-start)
                                      (map-elt rhs :output-start))
                               (equal (map-elt lhs :command-start)
                                      (map-elt rhs :command-start)))))))
    (when (and current history pos)
      (cons (1+ pos) (length history)))))

(cl-defun shell-pager--history-label (&key override)
  "Return the position in history of the primary shell buffer.

Can OVERRIDE position to be rendered."
  (let ((text (or override
                  (when-let ((pos (shell-pager--position)))
                    (format "%d/%d" (car pos) (cdr pos)))
                  "?/?")))
    (propertize (format "[%s]\n\n" text)
                'ignore t
                'read-only t
                'face font-lock-comment-face
                'rear-nonsticky t)))

(defun shell-pager--compose-text ()
  "Get the compose buffer text (excluding header)."
  (unless (eq (current-buffer) (shell-pager--buffer))
    (error "Not in a pager buffer"))
  (let ((text (buffer-string))
        (result "")
        (pos 0))
    (while (< pos (length text))
      (let ((next (or (next-single-property-change pos 'ignore text)
                      (length text))))
        (unless (get-text-property pos 'ignore text)
          (setq result (concat result (substring text pos next))))
        (setq pos next)))
    (string-trim result)))

(defun shell-pager--initialize (item)
  "Initialize pager with ITEM.

Item is of the form:

\(:command \"ls\"
 :output \"shell-pager.el\")"
  (let ((inhibit-read-only t))
    (save-excursion
      (erase-buffer)
      (insert
       (shell-pager--make-buffer-content
        :command (map-elt item :command)
        :output (map-elt item :output))))))

(cl-defun shell-pager--make-buffer-content (&key label-override
                                                 command
                                                 output)
  "Make buffer content with POSITION, COMMAND, and OUTPUT."
  (concat
   (shell-pager--history-label :override label-override)
   (when command
     (propertize command
                 'rear-nonsticky t
                 'command t
                 'face 'comint-highlight-input))
   (when command
     "\n")
   (when output
     output)))

(defun shell-pager--make-compose-buffer-content ()
  "Make buffer content with POSITION, COMMAND, and OUTPUT."
  (propertize "[compose]\n\n"
              'ignore t
              'read-only t
              'face 'font-lock-escape-face
              'rear-nonsticky t))

(cl-defun shell-pager--make-config (&key shell-buffer
                                         page-buffer
                                         current
                                         next previous
                                         subscribe unsubscribe
                                         submit interrupt)
  "Make pager config.

Requires SHELL-BUFFER/PAGE-BUFFER as well as CURRENT, NEXT and PREVIOUS
functions.

For a richer experience provide SUBSCRIBE, UNSUBSCRIBE, SUBMIT and INTERRUPT
functions."
  (let ((config))
    (when shell-buffer
      (setq config
            (map-insert config
                        :shell-buffer shell-buffer)))
    (when page-buffer
      (setq config
            (map-insert config
                        :page-buffer page-buffer)))
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
    (when subscribe
      (setq config
            (map-insert config
                        :subscribe subscribe)))
    (when unsubscribe
      (setq config
            (map-insert config
                        :unsubscribe unsubscribe)))
    (when submit
      (setq config
            (map-insert config
                        :submit submit)))
    (when interrupt
      (setq config
            (map-insert config
                        :interrupt interrupt)))
    config))

(defun shell-pager--resolve-shell-buffer (buffer)
  "Resolve BUFFER to a pager config."
  (with-current-buffer buffer
    (cond ((derived-mode-p 'eshell-mode)
           (shell-pager--make-config
            :shell-buffer buffer
            :page-buffer (shell-pager--buffer)
            :next #'shell-pager--eshell-next
            :previous #'shell-pager--eshell-previous
            :current #'shell-pager--eshell-current-item
            :subscribe #'shell-pager--eshell-subscribe
            :unsubscribe #'shell-pager--eshell-unsubscribe
            :submit #'shell-pager--eshell-submit
            :interrupt #'shell-pager--eshell-interrupt))
	  ((derived-mode-p 'inferior-ess-r-mode)
           (shell-pager--make-config
            :shell-buffer buffer
            :page-buffer (shell-pager--buffer)
            :next #'shell-pager--ess-r-next
            :previous #'shell-pager--ess-r-previous
            :current #'shell-pager--ess-r-current-item
            :subscribe #'shell-pager--ess-r-subscribe
            :unsubscribe #'shell-pager--ess-r-unsubscribe
            :submit #'shell-pager--ess-r-submit
            :interrupt #'shell-pager--ess-r-interrupt))
          (t
           (error "Don't know how to page %s" major-mode)))))

(defun shell-pager--shell-buffer ()
  "Get the pager's shell buffer."
  (or (map-elt shell-pager--config :shell-buffer)
      (error "No shell available")))

(defun shell-pager--buffer ()
  "Get the available shell pager buffer."
  (get-buffer-create "*shell pager*"))

(defun shell-pager--eshell-interrupt ()
  "Interrupt eshell's ongoing command."
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (eshell-interrupt-process))

(defun shell-pager--eshell-subscribe (page-buffer)
  "Subscribe PAGE-BUFFER to `eshell' output."
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (let ((config (with-current-buffer page-buffer
                  shell-pager--config)))
    (cl-assert config nil "Must have a shell-pager config")
    (setq-local shell-pager--config config)
    (add-hook 'eshell-output-filter-functions
              #'shell-pager--eshell-output-filter
              nil t)))

(defun shell-pager--eshell-unsubscribe (page-buffer)
  "Unsubscribe PAGE-BUFFER from `eshell' output."
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (let ((config (with-current-buffer page-buffer
                  shell-pager--config)))
    (cl-assert config nil "Must have a shell-pager config")
    (setq-local shell-pager--config config)
    (remove-hook 'eshell-output-filter-functions
                 #'shell-pager--eshell-output-filter t)))

(defun shell-pager--eshell-output-filter ()
  "Handle `eshell' output as per `eshell-output-filter-functions'."
  (unless (eq major-mode 'eshell-mode)
    (error "Not in an eshell buffer"))
  (when-let* ((config shell-pager--config)
              (live-pager (buffer-live-p (map-elt config :page-buffer)))
              (output (replace-regexp-in-string
                       eshell-prompt-regexp ""
                       (buffer-substring
                        eshell-last-output-start
                        eshell-last-output-end))))
    (with-current-buffer (or (map-elt config :page-buffer)
                             (error "No pager available"))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert output)))))
  nil)

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

(defun shell-pager--eshell-submit (command)
  "Submit COMMAND to eshell for execution."
  (with-current-buffer (shell-pager--shell-buffer)
    (goto-char (point-max))
    (insert command)
    (eshell-send-input)))

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
      (if (re-search-forward eshell-prompt-regexp nil t)
          (progn
            (setq output-end (match-beginning 0))
            (setq output (buffer-substring
                          output-start
                          output-end)))
        ;; No next prompt.
        ;; Process is likely alive.
        ;; Grab output until end of buffer.
        (setq output-end (point-max))
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

;;; Generic/comint 



(defun shell-pager--next-prompt-regexp (regexp)
  "Move to next prompt based REGEXP."
  (let ((prev-line (line-number-at-pos)))
    (re-search-forward regexp nil t)
    (goto-char (match-end 0))
    ;; If we didn't move line, "nudge" and try again
    (when (eq (line-number-at-pos) prev-line)
      (forward-line)
      (re-search-forward regexp nil t)
      (goto-char (match-end 0)))))

(defun shell-pager--previous-prompt-regexp (regexp)
  "Move to previous prompt based REGEXP."
  (let ((prev-point (point))
	(current-line (line-number-at-pos))
	(new-point))
    (re-search-backward regexp nil t)
    (goto-char (match-end 0))
    ;; If we didn't move, "nudge" and try again
    (when (eq (point) prev-point)
      (setq prev-point (point))
      (backward-char)
      (re-search-backward regexp nil t)
      (setq new-point (match-end 0))
      ;; Don't circle back around. (Can this be specified in
      ;; `re-search-backward'?)
      (when (< new-point prev-point)
	(goto-char (match-end 0))))))

;;; ESS/R

;; There is also `inferior-ess-prompt' but it doesn't match in a way
;; that work for `shell-pager'
(defvar shell-pager--ess-r-prompt-regexp "^\\(?:+ \\)*> "
  "Regexp for ESS/R shell prompt.")

(defun shell-pager--ess-r-next ()
  "Move ESS/R shell to next prompt and return item.

Return non-nil if point moved"
  (unless (eq major-mode 'inferior-ess-r-mode)
    (error "Not in an ESS/R buffer"))
  (let ((point (point))
        (new-point))
    (save-excursion
      (shell-pager--next-prompt-regexp shell-pager--ess-r-prompt-regexp)
      (when (and (not (eq point (point)))
                 (not (eobp)))
        (setq new-point (point))))
    (when new-point
      (goto-char new-point))))

(defun shell-pager--ess-r-previous ()
  "Move ESS/R shell to previous prompt and return item.

Return non-nil if point moved"
  (unless (eq major-mode 'inferior-ess-r-mode)
    (error "Not in an ESS/R buffer"))
  (let ((line (line-number-at-pos))
        (new-point))
    (save-excursion
      (shell-pager--previous-prompt-regexp shell-pager--ess-r-prompt-regexp)
      (when (and (eq line (line-number-at-pos))
                 (save-excursion
                   (goto-char (point-min))
                   (not (eq (line-number-at-pos) line))))
        (setq new-point (point-min)))
      (unless (eq line (line-number-at-pos))
        (setq new-point (point))))
    (when new-point
      (goto-char new-point))))

(defun shell-pager--ess-r-current-item ()
  "Return current ESS/R shell item."
  (unless (eq major-mode 'inferior-ess-r-mode)
    (error "Not in an ESS/R buffer"))
  (save-excursion
    (let ((command-start)
          (command-end)
          (command)
	  (next-prompt)
          (output-start)
          (output-end)
          (output))
      ;; There is probably a better way of implementing this
      (when (re-search-backward shell-pager--ess-r-prompt-regexp nil t)
        (setq command-start (match-end 0))
	(goto-char command-start)
	(end-of-line)
	(setq command-end (point))
	(re-search-forward shell-pager--ess-r-prompt-regexp nil t)
	(setq next-prompt (match-beginning 0))
	(goto-char command-end)
	;; Each line of a "continued command" starts with +
	(while-let ((_ (re-search-forward "^+ " nil t))
		    (cont-point (match-end 0))
		    (__ (< cont-point next-prompt)))
	  (goto-char cont-point)
	  (end-of-line)
	  (setq command-end (point)))
        (setq command (buffer-substring-no-properties
                       command-start
                       command-end))
	(forward-line)
	(beginning-of-line))
      (when command-end
	(goto-char command-end))
      (setq output-start (point))
      (if (re-search-forward shell-pager--ess-r-prompt-regexp nil t)
          (progn
            (setq output-end (match-beginning 0))
            (setq output (buffer-substring
                          output-start
                          output-end)))
        (setq output-end (point-max))
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

(defun shell-pager--ess-r-submit (command)
  "Submit COMMAND to `R' for execution."
  (with-current-buffer (shell-pager--shell-buffer)
     (ess-send-string (ess-get-process) command t)))

(defun shell-pager--ess-r-interrupt ()
  "Interrupt ESS/R's ongoing command."
  (unless (derived-mode-p 'eshell-mode)
    (error "Not in an ESS/R buffer"))
  (ess-interrupt))

(defun shell-pager--ess-r-subscribe (page-buffer)
  "Subscribe PAGE-BUFFER to `R' output."
  (unless (derived-mode-p 'inferior-ess-r-mode)
    (error "Not in an R buffer"))
  (let ((config (with-current-buffer page-buffer
                  shell-pager--config)))
    (cl-assert config nil "Must have a shell-pager config")
    (setq-local shell-pager--config config)
    (add-hook 'comint-output-filter-functions
              #'shell-pager--ess-r-output-filter
              nil t)))

(defun shell-pager--ess-r-unsubscribe (page-buffer)
  "Unsubscribe PAGE-BUFFER from `R' output."
  (unless (derived-mode-p 'inferior-ess-r-mode)
    (error "Not in an R buffer"))
  (let ((config (with-current-buffer page-buffer
                  shell-pager--config)))
    (cl-assert config nil "Must have a shell-pager config")
    (setq-local shell-pager--config config)
    (remove-hook 'comint-output-filter-functions
                 #'shell-pager--ess-r-output-filter t)))

(defun shell-pager--ess-r-output-filter (string)
  "Handle `R' output as per `comint-output-filter-functions'."
  (unless (derived-mode-p 'inferior-ess-r-mode)
    (error "Not in an R buffer"))
  (when-let* ((config shell-pager--config)
              (live-pager (buffer-live-p (map-elt config :page-buffer)))
	      (output (replace-regexp-in-string
                       shell-pager--ess-r-prompt-regexp ""
                string)))
    (with-current-buffer (or (map-elt config :page-buffer)
                             (error "No pager available"))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert output)))))
  string)

;;; shell-pager.el ends here
