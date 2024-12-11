;;; shell-pager.el --- A pager for shells  -*- lexical-binding: t -*-


;;; Commentary:
;;

(require 'em-prompt)
(require 'comint)

;;; Code:

(defcustom shell-pager-process-header-prompt
  (lambda (prompt)
    "Remove trailing `$`, ignore trailing spaces."
    (replace-regexp-in-string "[ \t]*\\$[ \t]*$" "" prompt))
  "How long to wait for a request to time out in seconds."
  :type 'function
  :group 'shell-pager)

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
    (define-key map (kbd "i") #'shell-pager-compose-command)
    (define-key map (kbd "r") #'shell-pager-compose-command)
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
  (unless (derived-mode-p 'eshell-mode)
    (user-error "Not in eshell"))
  (let ((shell-buffer (current-buffer))
        (entry (shell-pager--eshell-current-item))
        (config))
    (with-current-buffer (shell-pager--buffer)
      (shell-pager-mode)
      (shell-pager-view-mode +1)
      (setq config (shell-pager--resolve-shell-buffer shell-buffer))
      (setq shell-pager--config config)
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
      :header (shell-pager--propertize-header
               (format "%s\n\n" (shell-pager--page-num :new-candidate t)))
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
             command)
    ;; Execute after current run loop to allow shell to update itself.
    (run-at-time 0.01 nil
                 (lambda ()
                   (shell-pager--initialize (shell-pager--current))))))

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

(defun shell-pager--prompt ()
  "Get the shell's prompt for current environment."
  (when-let* ((prompt (map-elt shell-pager--config :prompt))
              (valid (functionp prompt)))
    (with-current-buffer (shell-pager--shell-buffer)
      (if shell-pager-process-header-prompt
          (funcall shell-pager-process-header-prompt (funcall prompt))
        (funcall prompt)))))

(defun shell-pager--current ()
  "Get current shell item."
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

(cl-defun shell-pager--position (&key new-candidate)
  "Return the position in history.

If NEW-CANDIDATE is non-nil, return position at length + 1.

In the form:

1 out of 3 = (1 . 3)."
  (let* ((current (shell-pager--current))
         (history (shell-pager--shell-items))
         (length (length history))
         (pos (if new-candidate
                  (1+ length)
                (seq-position history current
                              ;; Must compare against locations as
                              ;; commands and even outputs can yield
                              ;; false positives.
                              (lambda (lhs rhs)
                                (or
                                 (equal (map-elt lhs :output-start)
                                        (map-elt rhs :output-start))
                                 (equal (map-elt lhs :command-start)
                                        (map-elt rhs :command-start))))))))
    (if new-candidate
        (cons pos (1+ length))
      (when (and current history pos)
        (cons (1+ pos) (length history))))))

(cl-defun shell-pager--page-num (&key new-candidate)
  "Generate page number text in the form: [1/3].

If NEW-CANDIDATE is non-nil, return position at length + 1."
  (if-let ((pos (shell-pager--position :new-candidate new-candidate)))
      (format "[%d/%d]" (car pos) (cdr pos))
    "[?/?]"))

(defun shell-pager--propertize-header (text)
  "Propertize TEXT as header."
  (propertize text
              'ignore t
              'read-only t
              'face font-lock-comment-face
              'rear-nonsticky t))

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
        :header (shell-pager--propertize-header
                 (concat (shell-pager--page-num)
                         (when (map-elt item :prompt)
                           (concat " "
                                   (if shell-pager-process-header-prompt
                                       (funcall shell-pager-process-header-prompt
                                                (map-elt item :prompt))
                                     (map-elt item :prompt)) " "))
                         "\n\n"))
        :command (map-elt item :command)
        :output (map-elt item :output))))
    (if (map-elt item :command)
        (progn
          (goto-char (point-min))
          (text-property-search-forward 'command t))
      (goto-char (point-min))
      (text-property-search-forward 'ignore nil))))

(cl-defun shell-pager--make-buffer-content (&key header
                                                 command
                                                 output)
  "Make buffer content with HEADER, COMMAND, and OUTPUT."
  (concat
   header
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
  (let* ((last-item (car (last (shell-pager--shell-items))))
         (prompt (shell-pager--prompt)))
    (propertize (concat
                 "[new command]"
                 (when prompt
                   " ")
                 (when prompt
                   prompt)
                 (when prompt
                   " ")
                 "\n\n")
                'ignore t
                'read-only t
                'face 'font-lock-escape-face
                'rear-nonsticky t)))

(cl-defun shell-pager--make-config (&key shell-buffer
                                         page-buffer
                                         current
                                         next previous
                                         subscribe unsubscribe
                                         submit interrupt
                                         prompt)
  "Make pager config.

Requires SHELL-BUFFER/PAGE-BUFFER as well as CURRENT, NEXT and PREVIOUS
functions.

For a richer experience provide SUBSCRIBE, UNSUBSCRIBE, SUBMIT, INTERRUPT
and PROMPT functions."
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
    (when prompt
      (setq config
            (map-insert config
                        :prompt prompt)))
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
            :interrupt #'shell-pager--eshell-interrupt
            :prompt #'shell-pager--eshell-prompt))
          (t
           (error "Don't know how to page %s" major-mode)))))

(defun shell-pager--shell-buffer ()
  "Get the pager's shell buffer."
  (or (map-elt shell-pager--config :shell-buffer)
      (error "No shell available")))

(defun shell-pager--buffer ()
  "Get the available shell pager buffer."
  (get-buffer-create "*shell pager*"))

(defun shell-pager--eshell-prompt ()
  "Generate prompt for shell environment."
  (when eshell-prompt-function
    (funcall eshell-prompt-function)))

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
    (let ((prompt-start)
          (prompt-end)
          (prompt)
          (command-start)
          (command-end)
          (command)
          (output-start)
          (output-end)
          (output))
      (when (re-search-backward eshell-prompt-regexp nil t)
        (setq prompt-start (match-beginning 0))
        (setq prompt-end (match-end 0))
        (when (and prompt-start
                   prompt-end)
          (setq prompt (buffer-substring-no-properties
                        prompt-start
                        prompt-end)))
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
        (list :prompt prompt
              :prompt-start prompt-start
              :prompt-end prompt-end
              :command command
              :command-start command-start
              :command-end command-end
              :output output
              :output-start output-start
              :output-end output-end)))))

;;; shell-pager.el ends here
