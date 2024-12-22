;;; gptel-dialogue.el --- Dedicated buffer for gptel dialogue -*-
;;; lexical-binding: t; -*-

;; Copyright (C) 2024  Kang Tu

;; Author: Kang Tu
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
  (define-key gptel-dialogue-mode-map (kbd "SPC") #'gptel-dialogue-discuss)
  (run-mode-hooks 'gptel-dialogue-mode-hook))

(defalias 'gptel-dialogue-read-string 'read-string)

(defun gptel-dialogue--get-buffer-name ()
  "Generate a buffer name for gptel dialogue based on the current project or a default name.

The buffer name is determined as follows:
- If a version control root directory is found, the buffer name is generated based on the abbreviated file name of the root directory.
- If no version control root directory is found, the default buffer name `*gptel-dialogue*' is used."
  (let ((vc-root (vc-root-dir))
        (buffer-name (if vc-root
                         (format "*gptel:%s*" (abbreviate-file-name vc-root))
                       gptel-dialogue-buffer-name)))
    buffer-name))

(defun gptel-dialogue-switch-to-buffer ()
  "Switch to the gptel dialogue buffer in another window.

The buffer name is determined by `gptel-dialogue--get-buffer-name'."
  (interactive)
  (switch-to-buffer-other-window (gptel-dialogue--get-buffer-name)))

(defun gptel-dialogue-discuss ()
  "Ask a question to gptel and display the response in the dedicated dialogue buffer in another window."
  (interactive)
  (let* ((question (gptel-dialogue-read-string "Ask gptel: "))
         (region-active-p (use-region-p))
         (region-text (when region-active-p
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (final-question (if region-active-p
                             (format "%s: %s" question region-text) question))
        (buffer-name (gptel-dialogue--get-buffer-name)))
   (gptel buffer-name nil final-question nil)
   (let ((buffer (get-buffer buffer-name)))
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert final-question)
        (gptel-send)
        )
      (display-buffer buffer '((display-buffer-pop-up-window)
                                (inhibit-same-window . t))))))

(provide 'gptel-dialogue)
;;; gptel-dialogue.el ends here
