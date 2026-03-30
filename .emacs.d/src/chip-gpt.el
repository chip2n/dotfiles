;;; chip-code-gpt.el -*- lexical-binding: t -*-

;; Copyright (C) 2022  Andreas Arvidsson
;;
;; Author: Andreas Arvidsson <andreas@arvidsson.io>
;; Keywords: config
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(use-package gptel
  :config
  (setq gptel-api-key private/openai-key)
  (setq-default gptel-model "gpt-4o")
  (setq-default gptel-default-mode 'org-mode)
  (setq-default gptel-track-media t)
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "# ")
          (org-mode . "* ")
          (text-mode . "# ")))
  (gptel-make-anthropic "Claude" :stream t :key private/anthropic-key))

(use-package agent-shell
  ;; :ensure-system-package
  ;; Add agent installation configs here
  ;; ((claude . "brew install claude-code")
  ;;  (claude-code-acp . "npm install -g @zed-industries/claude-code-acp"))
  :config
  (setq agent-shell-header-style 'text)

  ;; Unbind C-<tab> keybinding
  (define-key agent-shell-mode-map (kbd "C-<tab>") nil)

  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
  (setq agent-shell-session-strategy 'prompt)
  (setq agent-shell-buffer-name-format 'kebab-case)

  (defun c/project-agent-shell--annotate (buf)
    "Return annotation string for agent-shell buffer BUF."
    (with-current-buffer buf
      (let* ((state agent-shell--state)
             (busy (agent-shell--active-requests-p state))
             (mode-id (map-nested-elt state '(:session :mode-id)))
             (mode-name (when mode-id
                          (or (agent-shell--resolve-session-mode-name
                               mode-id
                               (agent-shell--get-available-modes state))
                              mode-id)))
             (status (if busy
                         (propertize "busy" 'face '(:foreground "red"))
                       (propertize "idle" 'face '(:foreground "green"))))
             (parts (list status)))
        (when mode-name
          (push mode-name parts))
        (concat "  " (string-join (nreverse parts) " | ")))))

  (defun c/project-agent-shell-switch ()
    "Switch to an agent-shell buffer in the current project.
If the user types a name that matches an existing shell, switch to it.
If the user types a new name, start a new shell and rename the buffer."
    (interactive)
    (let* ((bufs (agent-shell-project-buffers))
           (candidates (mapcar (lambda (buf)
                                 (cons (buffer-name buf) buf))
                               bufs))
           (completion-extra-properties
            (list :annotation-function
              (lambda (name)
                (when-let ((buf (cdr (assoc name candidates))))
                  (c/project-agent-shell--annotate buf)))))
           (choice (completing-read "Shell: " candidates nil nil)))
      (if-let ((existing (cdr (assoc choice candidates))))
          (switch-to-buffer existing)
        (let ((agent-shell-buffer-name-format (lambda (&rest rest) (concat "agent: " choice))))
          (switch-to-buffer
           (agent-shell--start :config (or (agent-shell--resolve-preferred-config)
                                           (agent-shell-select-config
                                            :prompt "Start new agent: ")
                                           (error "No agent config found"))
                               :no-focus t
                               :new-session t
                               :session-strategy 'new)))))))

(use-package agent-review
  :straight (:host github :repo "nineluj/agent-review"))

(use-package agent-shell-manager
  :straight (:host github :repo "jethrokuan/agent-shell-manager"))

(provide 'chip-gpt)

;;; chip-code-gpt.el ends here
