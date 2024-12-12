;;; shell-pager-ess.el --- Support for ESS  -*- lexical-binding: t -*-

;; Author: Joel Lööw <joel@joelloow.se>

;;; License
;;
;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;

;;; Code:


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

(provide 'shell-pager-ess)

;;; shell-pager-ess. ends here
