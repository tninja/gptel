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

(defun gptel-dialogue-mode ()
  "Major mode for gptel dialogue buffers."
  (interactive)
  (kill-all-local-variables)
  (change-major-mode 'special-mode)
  (setq-local mode-name "gptel-dialogue")
  (setq-local transient-mark-mode nil)
  (gptel-mode)
  (setq-local major-mode 'gptel-dialogue-mode)
  (run-mode-hooks))

(define-derived-mode gptel-dialogue-mode special-mode "gptel-dialogue"
  "Major mode for gptel dialogue buffers."
  :group 'gptel
  (setq-local transient-mark-mode nil)
  (gptel-mode))

(defun gptel-dialogue ()
  "Switch to or create a dedicated buffer for gptel dialogue."
  (interactive)
  (let ((buffer (get-buffer-create gptel-dialogue-buffer-name)))
    (with-current-buffer buffer
      (unless gptel-dialogue-mode
        (gptel-dialogue-mode)))
    (switch-to-buffer buffer)))

(defun gptel-dialogue-send ()
  "Send the current prompt to the LLM in the dedicated gptel dialogue buffer."
  (interactive)
  (unless (eq (current-buffer) (get-buffer gptel-dialogue-buffer-name))
    (user-error "This command is only available in the gptel dialogue buffer"))
  (call-interactively #'gptel-send))

(provide 'gptel-dialogue)
;;; gptel-dialogue.el ends here
