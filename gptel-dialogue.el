;;; gptel-dialogue.el --- Dedicated buffer for gptel dialogue -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kang Tu

;; Author: YOUR NAME
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a dedicated buffer for gptel dialogue.

;;; Code:

(require 'gptel)

(defvar gptel-dialogue-buffer-name "*gptel-dialogue*"
  "Name of the dedicated gptel dialogue buffer.")

(define-derived-mode gptel-dialogue-mode special-mode "gptel-dialogue"
  "Major mode for gptel dialogue buffers."
  :group 'gptel
  (setq-local transient-mark-mode nil)
  (setq-local mode-name "gptel-dialogue")
  (define-key gptel-dialogue-mode-map (kbd "SPC") #'gptel-dialogue-ask)
  (run-mode-hooks 'gptel-dialogue-mode-hook))

(defalias 'gptel-dialogue-read-string 'read-string)

(defun gptel-dialogue-switch-to-buffer ()
  "Switch to the gptel dialogue buffer's window if the current buffer is not the gptel dialogue buffer."
  (interactive)
  (let ((buffer (get-buffer gptel-dialogue-buffer-name)))
    (when (and buffer (not (eq (current-buffer) buffer)))
      (switch-to-buffer-other-window buffer))))

(defun gptel-dialogue-ask ()
  "Ask a question to gptel and display the response in the dedicated dialogue buffer in another window."
  (interactive)
  (let* ((question (gptel-dialogue-read-string "Ask gptel: "))
         (region-active-p (use-region-p))
         (region-text (when region-active-p
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (final-question (if region-active-p
                             (format "%s: %s" question region-text)
                           question)))
    (let ((buffer (get-buffer-create gptel-dialogue-buffer-name)))
      (with-current-buffer buffer
        (gptel-dialogue-mode)
        (read-only-mode 0)
        (goto-char (point-max))
        (when (> (point) (point-min))
          (insert "\n\n"))
        (insert (propertize (format-time-string "%H:%M:%S") 'face 'italic) "\n")
        (insert (propertize "Q: " 'face '(:foreground "red" :weight bold)) (format "%s\n" final-question))
        (insert (propertize "A: " 'face '(:foreground "green" :weight bold)))
        (gptel-request
         final-question
         :buffer buffer
         :stream t
         :position (point)
         :callback (lambda (response info)
                     (with-current-buffer (plist-get info :buffer)
                       (goto-char (point-max))
                       (when response
                         (insert (format "%s" response)))
                       (when (and (eq response t) (plist-get info :error))
                         (insert (format "Error: %s\n" (plist-get info :error))))
                       (goto-char (point-max))))))
      (gptel-dialogue-switch-to-buffer))))

;; TODO
;; [X] 1. use helm-read-string
;; [X] 2. support region read. if there is region, concat the prompt (if it is available) and region
;; [X] 3. space inside dialogue buffer trigger input

(provide 'gptel-dialogue)
;;; gptel-dialogue.el ends here
