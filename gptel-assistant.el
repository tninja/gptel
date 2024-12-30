;;; gptel-assistant.el --- Dedicated buffer for gptel assistant -*-
;;; lexical-binding: t; -*-

;; Copyright (C) 2024  Kang Tu

;; Author: Kang Tu
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (gptel "0.9.7") (helm "3.0") (transient "0.4.0") (evil "1.0.0"))

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
(require 'helm)
(require 'cl-lib)  ; For `cl-subseq`
(require 'evil)

(defvar gptel-assistant-buffer-name "*gptel-assistant*"
  "Name of the dedicated gptel assistant buffer.")

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
Create a new buffer if needed. If the current buffer is already a gptel session buffer, do nothing."
  (interactive)
  (unless (string-match-p "^\\*gptel" (buffer-name))  ; Check if current buffer is a gptel session buffer
    (let ((buffer-name (gptel-assistant--get-buffer-name)))
      (unless (get-buffer buffer-name)
        (gptel buffer-name))
      (switch-to-buffer-other-window buffer-name)
      (goto-char (point-max)))))

(defun gptel-assistant-question ()
  "Ask a question to gptel and display the response in the dedicated assistant buffer in another window."
  (interactive)
  (let* ((question (gptel-helm-read-string "Ask gptel: "))
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
                                (inhibit-same-window . t)))
      (gptel-assistant-switch-to-buffer)
      )))

(defun gptel-assistant-helm-read-string (prompt)
  "Read a string with Helm completion for gptel, showing historical inputs."
  (helm-read-string-with-history prompt "gptel-helm-read-string-history.el"))

(defun gptel-assistant-helm-read-string-with-history (prompt history-file-name &optional initial-input)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string."
  ;; Load history from file
  (let* ((history-file (expand-file-name history-file-name user-emacs-directory))
         (history (when (file-exists-p history-file)
                    (with-temp-buffer
                      (insert-file-contents history-file)
                      (delete-dups (read (buffer-string))))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 history
                 :must-match nil
                 :name "Helm Read String"
                 :fuzzy t
                 :initial-input initial-input)))
    ;; Add to history if non-empty and save
    (unless (string-empty-p input)
      (push input history)
      (with-temp-file history-file
        (let ((history-entries (cl-subseq history
                                          0 (min (length history)
                                                 10000))))  ; Keep last 10000 entries
          (insert (prin1-to-string history-entries)))))
    input))

(defun gptel-assistant-call-gptel (prompt)
  "Call gptel using Gemini backend, and return the response string.
Arguments:
PROMPT: The prompt string to send to Gemini."
  (let ((response-buffer (get-buffer-create "*gptel-response*"))
        (result-string ""))
    (message (concat "Sending request to gptel... " prompt))
    (gptel-request prompt
      :buffer response-buffer
      :callback (lambda (response info)
                  (if response
                      (with-current-buffer response-buffer
                        (goto-char (point-max))
                        (insert response)
                        (setq result-string
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                    (message "Error: %s" (plist-get info
                                                    :status)))))
    ;; Wait for the request to finish, until response-buffer is non-empty
    (while (string-empty-p result-string)
      (sleep-for 0.1))
    ;; Clean up the temporary buffer
    (kill-buffer response-buffer)
    result-string))

(defun gptel-assistant-rewrite-region-smerge (prompt-prefix)
  "Call gptel with a prompt constructed from the given prefix and the current region text,
   and use smerge format to rewrite the region with the response from gptel.
   PROMPT-PREFIX: The prefix of the prompt to send to gptel."
  (interactive)
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (concat prompt-prefix region-text))
         (response-text (gptel-assistant-call-gptel prompt)))
    (if (not (string= region-text response-text))
        (progn
          (delete-region (region-beginning) (region-end))
          (insert (format "<<<<<<< before\n%s\n=======\n%s\n>>>>>>> after\n" region-text response-text))
          (smerge-mode 1))
      (message "No changes needed after processing."))))

(defun gptel-assistant-proof-reading-english-region-smerge ()
  "Proofread the English text in the currently selected region using gptel-assistant-call-gptel,
   and replace the selected region with the proofread text in smerge format."
  (interactive)
  (let ((prompt-prefix "Please proofread the following English text and correct any grammatical or spelling errors, please always keep the format and indentation for each line, only output the result:\n\n"))
    (gptel-rewrite-region-smerge prompt-prefix)))

(defun gptel-assistant-doc-rewrite-region-smerge ()
  "Proofread the English text in the currently selected region using gptel-assistant-call-gptel,
   and replace the selected region with the proofread text in smerge format."
  (interactive)
  (let ((prompt-prefix (gptel-helm-read-string "Input prompt for doc rewriting: ")))
    (gptel-rewrite-region-smerge (concat prompt-prefix ", for the following text, output the result only: \n\n"))))

(defun gptel-assistant-proofread-current-line-with-smerge ()
  "Proofread the current line and apply smerge formatting using gptel-proof-reading-english-region-smerge."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (call-interactively 'gptel-proof-reading-english-region-smerge)))

(defun gptel-assistant-proofread-english-with-smerge ()
  "Proofread English text using gptel-assistant-call-gptel and apply smerge formatting.
If a region is active, proofread the selected region.
Otherwise, proofread the current line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'gptel-proof-reading-english-region-smerge)
    (gptel-proofread-current-line-with-smerge)))

(defun gptel-helm-read-string (prompt)
  "Read a string with Helm completion for gptel, showing historical inputs."
  (helm-read-string-with-history prompt "gptel-helm-read-string-history.el"))

;; need to test and fix following two functions.. seems working now, with gpt-4o-mini

(defun gptel-assistant-get-current-language ()
  "Get the current programming language from major mode."
  (let ((major-mode-str (symbol-name major-mode)))
    (replace-regexp-in-string "-mode$" ""
                            (replace-regexp-in-string "-ts-mode$" "" major-mode-str))))

(defun gptel-assistant-clean-markdown-response (response-text)
  "Remove markdown code block markers from response."
  (replace-regexp-in-string
   "^[ \t]*```[[:alpha:]-]*[ \t]*\n\\|[ \t]*```[ \t]*$"
   ""
   response-text))

(defun gptel-assistant-coding-rewrite-region-smerge ()
  "Get the prompt prefix using gptel-helm-read-string, and call gptel-rewrite-region-smerge to update the region with smerge format."
  (interactive)
  (let* ((requirement (gptel-helm-read-string "Code change requirement: "))
         (lang (gptel-get-current-language))
         (prefix "You are a professional software engineer. please help me")
         (suffix (format "in %s programming language, output the code only" lang))
         (prompt (format "%s %s. %s: \n\n" prefix requirement suffix))
         (region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (full-prompt (concat prompt "\n\n" region-text))
         (response-text (gptel-clean-markdown-response
                        (gptel-assistant-call-gptel full-prompt))))
    (if (not (string= region-text response-text))
        (progn
          (delete-region (region-beginning) (region-end))
          (insert (format "<<<<<<< before\n%s\n=======\n%s\n>>>>>>> after\n"
                         region-text response-text))
          (smerge-mode 1))
      (message "No changes needed after processing."))))

(defun gptel-assistant-coding-insert-response-smerge ()
  "Get the prompt using gptel-helm-read-string, call gptel, and insert the response
   in smerge format with an empty 'before' block."
  (interactive)
  (let* ((requirement (gptel-helm-read-string "New code requirement: "))
         (lang (gptel-get-current-language))
         (prefix "You are a professional software engineer. please help me")
         (suffix (format "in %s programming language, output the code only" lang))
         (prompt (format "%s %s. %s" prefix requirement suffix))
         (response-text (gptel-clean-markdown-response
                        (gptel-assistant-call-gptel prompt))))
    (insert (format "<<<<<<< before\n\n=======\n%s\n>>>>>>> after\n" response-text))
    (smerge-mode 1)))

(defun gptel-assistant-coding-smart-smerge ()
  "If region is active, call region update function, otherwise call insert function."
  (interactive)
  (if (region-active-p)
      (call-interactively 'gptel-coding-rewrite-region-smerge)
    (call-interactively 'gptel-coding-insert-response-smerge)))

(defun gptel-assistant-ask-and-insert-answer ()
  "If there is a selected region, prompt the user for a question, send both th
 question and region content to gptel,
    and stream the response to the next line of the selected region.
    If there is no selected region, prompt the user for a question, send the
 question to gptel,
    and insert the response at the current point."
  (interactive)
  (if (use-region-p)
      (let* ((region-start (region-beginning))
             (region-end (region-end))
             (region-content (buffer-substring-no-properties region-start
                                                             region-end))
             (prompt (gptel-helm-read-string "Please enter your question: "))
             (full-prompt (format "%s: %s" prompt region-content))
             (insert-point (save-excursion
                             (goto-char region-end)
                             (forward-line 1)
                             (point))))
        (message (concat "Sending request to gptel... " full-prompt))
        (gptel-request full-prompt :position insert-point :stream t))
    (let ((prompt (gptel-helm-read-string "Please enter your question: ")))
      (message (concat "Sending request to gptel... " prompt))
      (gptel-request prompt :position (point) :stream t))))

(transient-define-prefix gptel-assistant-transient-menu ()
  "Transient menu for my gptel commands."
  ["gptel: my personal assistant for doc / code editing."
    ("d" "Discuss with gptel in assistant buffer" gptel-assistant-question)
    ("i" "Ask (on region) and insert answer to current buffer" gptel-assistant-ask-and-insert-answer)
    ;; ("r" "Rewrite doc in region in smerge format" gptel-rewrite)
    ("r" "Rewrite doc in region in smerge format" gptel-assistant-doc-rewrite-region-smerge)
    ("e" "Proofread English in smerge format" gptel-assistant-proofread-english-with-smerge)
    ("p" "Generate / update code in smerge format" gptel-assistant-coding-smart-smerge)
    ("z" "Switch to gptel assistant buffer" gptel-assistant-switch-to-buffer)
    ("s" "Abort / reset gptel" gptel-abort)
   ])

(global-set-key (kbd "C-c g") 'gptel-assistant-transient-menu)

(add-hook 'gptel-mode-hook 'evil-normal-state)

(evil-define-key 'normal gptel-mode-map (kbd "SPC") 'gptel-assistant-question)

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "C-c <") 'smerge-keep-upper)
  (define-key smerge-mode-map (kbd "C-c >") 'smerge-keep-lower)
  (define-key smerge-mode-map (kbd "C-c =") 'smerge-ediff))

(provide 'gptel-assistant)
;;; gptel-assistant.el ends here

