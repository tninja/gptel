;;; gptel-assistant.el --- Dedicated buffer for gptel assistant -*-
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

;; This file provides a dedicated buffer for gptel assistant.
;; It aim to provide an assistant to file or buffer edited inside emacs. You can discuss with the assitant, with context

;;; Code:

(require 'gptel)

(defvar gptel-assistant-buffer-name "*gptel-assistant*"
  "Name of the dedicated gptel assistant buffer.")

(defalias 'gptel-assistant-read-string 'read-string)

(defun gptel-assistant--get-buffer-name ()
  "Generate a buffer name for gptel assistant based on the current project or a default name.
The buffer name is determined as follows:
- If the current buffer is a gptel buffer, its name is returned.
- If a version control root directory is found, the buffer name is generated based on the abbreviated file name of the root directory.
- If no version control root directory is found, the default buffer name `*gptel-assistant*' is used."
  (let* ((current-buffer-name (buffer-name))
         (vc-root (vc-root-dir))
         (gptel-buffer-name (if (and current-buffer-name (string-match-p "^\\*gptel" current-buffer-name))
                                current-buffer-name
                              (if vc-root
                                  (format "*gptel:%s*" (abbreviate-file-name vc-root))
                                gptel-assistant-buffer-name))))
    gptel-buffer-name))

(defun gptel-assistant-switch-to-buffer ()
  "Switch to the gptel assistant buffer in another window.
The buffer name is determined by `gptel-assistant--get-buffer-name'.
Create a new buffer if needed."
  (interactive)
  (let ((buffer-name (gptel-assistant--get-buffer-name)))
    (unless (get-buffer buffer-name)
      (gptel buffer-name)))
  (switch-to-buffer-other-window (gptel-assistant--get-buffer-name)))
s
(defun gptel-assistant-question ()
  "Ask a question to gptel and display the response in the dedicated assistant buffer in another window."
  (interactive)
  (let* ((question (gptel-assistant-read-string "Ask gptel: "))
         (region-active-p (use-region-p))
         (region-text (when region-active-p
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (final-question (if region-active-p
                             (format "%s: %s" question region-text) question))
         (buffer-name (gptel-assistant--get-buffer-name)))
   (gptel buffer-name nil final-question nil)
   (let ((buffer (get-buffer buffer-name)))
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert final-question)
        (gptel-send)
        )
      (display-buffer buffer '((display-buffer-pop-up-window)
                                (inhibit-same-window . t))))))
s
(evil-define-key 'normal gptel-mode-map (kbd "SPC") 'gptel-assistant-question)

(provide 'gptel-assistant)
;;; gptel-assistant.el ends here
