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

(defun gptel-dialogue-discuss ()
  "Ask a question to gptel and display the response in the dedicated dialogue buffer in another window."
  (interactive)
  (let* ((question (gptel-dialogue-read-string "Ask gptel: "))
         (region-active-p (use-region-p))
         (region-text (when region-active-p
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (final-question (if region-active-p
                             (format "%s: %s" question region-text)
                           question))
         (vc-root (vc-root-dir))
         (buffer-name (if vc-root
                          (format "*gptel:%s*" (abbreviate-file-name vc-root))
                        "*gptel-dialogue*")))
    (gptel buffer-name nil final-question nil) ; 使用 gptel 函数创建/切换缓冲区
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
