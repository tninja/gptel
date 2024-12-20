;;; gptel-dialogue.el --- Dedicated buffer for gptel dialogue -*- lexical-binding: t; -*-

;; Copyright (C) 2024  YOUR NAME

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
  (run-mode-hooks 'gptel-dialogue-mode-hook))

(defun gptel-dialogue-ask-question ()
  "Ask a question to gptel and display the response in the dedicated dialogue buffer in another window."
  (interactive)
  (let ((question (read-string "Ask gptel: ")))
    (let ((buffer (get-buffer-create gptel-dialogue-buffer-name)))
      (with-current-buffer buffer
        (gptel-dialogue-mode)
        (read-only-mode 0)
        (goto-char (point-max))
        (when (> (point) (point-min))
          (insert "\n"))
        (insert (format "%s: %s\n" (gptel-prompt-prefix-string) question))
        (gptel-request
         question
         :buffer buffer
         :position (point)
         :callback (lambda (response info)
                     (with-current-buffer (plist-get info :buffer)
                       (goto-char (point-max))
                       (when response
                         (insert (format "%s: %s\n" (gptel-response-prefix-string) response)))
                       (when (and (eq response t) (plist-get info :error))
                         (insert (format "Error: %s\n" (plist-get info :error))))
                       (goto-char (point-max)))))
      (switch-to-buffer-other-window buffer)))))


(provide 'gptel-dialogue)
;;; gptel-dialogue.el ends here
