;;; chip-code-dbg.el -*- lexical-binding: t -*-

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

(require 's)

(use-package dap-mode
  :defer t
  :config
  (setq dap-auto-configure-features '(sessions locals tooltip))
  (require 'dap-gdb-lldb))

(define-minor-mode c/dap-session-mode
  "A mode for adding keybindings to running sessions"
  :lighter nil
  :keymap `((,(kbd "n") . dap-next)
            (,(kbd "e") . dap-eval)
            (,(kbd "i") . dap-step-in)
            (,(kbd "o") . dap-step-out)
            (,(kbd "c") . dap-continue)
            (,(kbd "b") . dap-breakpoint-toggle)
            (,(kbd "q") . dap-disconnect))

  ;; Always turn off zoom-mode to preserve UI windows for session
  (when zoom-mode
    (zoom-mode 0))

  (if c/dap-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (c/dap-session-mode -1))))
        (read-only-mode 1)
        (evil-emacs-state))
    (read-only-mode 0)
    (evil-normal-state)))

(add-hook 'dap-session-created-hook 'c/dap-session-mode)
(add-hook 'dap-stopped-hook 'c/dap-session-mode)
(add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                          (when (dap--session-running session)
                                            (c/dap-session-mode 1))))

(defun c/profiler-start ()
  (interactive)
  (profiler-start 'cpu))

(general-define-key
 "C-c y" 'c/profiler-start
 "C-c Y" 'profiler-stop)

(use-package realgud
  :defer t
  :config
  (setq realgud-safe-mode nil))

(defvar c/gdb-last-path nil)

(defun c/gdb (path)
  (interactive
   (let ((path (file-truename (read-file-name "Path to executable: " c/gdb-last-path c/gdb-last-path nil))))
     (message path)
     (setq c/gdb-last-path path)
     (list path)))
  (let ((source-buf (current-buffer)))
    (delete-other-windows)
    (gdb (format "gdb -i=mi %s" path))
    (split-window-vertically)
    (switch-to-buffer source-buf)
    (chip/window-zoom)
    (other-window 1)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer
     (gdb-get-buffer-create 'gdb-inferior-io))
    (other-window 2)
    ))

(setq gdb-restore-window-configuration-after-quit t)

;; (defun c/gdb (path)
;;   (interactive "FPath to executable: ")
;;   (let ((source-buf (current-buffer)))
;;     (delete-other-windows)
;;     (realgud:gdb (format "gdb %s" path))
;;     (realgud:attach-source-buffer source-buf)
;;     (let ((gdb-buf (current-buffer)))
;;       (split-window-horizontally)
;;       (switch-to-buffer source-buf)
;;       (other-window 1)
;;       (switch-to-buffer gdb-buf))))

(defun c/gdb-attach ()
  (interactive)
  (let ((pid (c/get-pid))
        (source-buf (current-buffer)))
    (save-window-excursion
      (gdb (format "gdb -i=mi attach %s" pid)))
    (switch-to-buffer-other-window (gdb-get-buffer 'gdbmi))))

(defun c/realgud-gdb-attach ()
  (interactive)
  (let ((pid (c/get-pid))
        (source-buf (current-buffer)))
    (save-window-excursion
      (realgud:gdb-pid pid)
      (realgud:attach-source-buffer source-buf))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer (realgud:gdb-find-command-buffer pid))))

(defun c/get-pid ()
  "Get PID by name read from completing-read."
  (unless (c/process-attach-permitted)
    (let ((default-directory "/sudo::/"))
      (shell-command "echo 0 | tee /proc/sys/kernel/yama/ptrace_scope")))
  (let* ((output (s-lines (shell-command-to-string "ps ax -o pid,s,comm | awk '{if ($2 != \"Z\") { print } }'")))
         (lines (-filter (-compose #'not #'s-blank?) (cdr output)))
         (processes (mapcar (lambda (line)
                              (let* ((split (s-split " " (s-trim line)))
                                     (pid (string-to-number (s-trim (car split))))
                                     (comm (s-trim (caddr split))))
                                (cons comm pid)))
                            lines)))
    (-> (completing-read "Attach to process:" processes)
        (assoc processes)
        (cdr))))

(defun c/process-attach-permitted ()
  "Check if attaching to process is allowed."
  (with-temp-buffer
    (insert-file-contents "/proc/sys/kernel/yama/ptrace_scope")
    (string-equal (s-trim-right (buffer-string)) "0")))

(defhydra c/gdb-hydra (:color amaranth :hint nil)
  "
^Stepping^         ^Breakpoints^
^^^^^^^^-------------------------------------
_n_: next          _b_: put breakpoint
_s_: step          _k_: remove breakpoint
"
  ("n" gud-next)
  ("s" gud-step)
  ("b" gud-break)
  ("k" gud-remove)
  ("q" nil))

(provide 'chip-code-dbg)

;;; chip-code-dbg.el ends here
