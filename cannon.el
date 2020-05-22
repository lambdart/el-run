;;; cannon.el --- Simple dynamic command launcher
;;
;; -*- lexical-binding: t -*-
;;
;; Author: Isaac "esac" <esac-io@tutanota.com>
;; Version: 0.1
;; URL: https://github.com/esac-io/cannon
;; Compatibility: GNU Emacs 25.x
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 esac
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; Simulate Dmenu command line program to dynamic/interactive
;; launch applications/commands regarding $PATH
;; environment variable using Emacs vanilla builtin
;; facilities (minibuffer/completions).
;;
;; PS: Works pretty well with icomplete (rocks)!
;;
;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'comint)

(defgroup cannon nil
  "Very simple (exec-path) command launcher."
  :group 'extensions
  :group 'convenience)

(defcustom cannon-prompt "Cannon: "
  "String to display in the initial `minibuffer' prompt."
  :type 'string
  :group 'cannon)

(defcustom cannon-args-prompt "Args: "
  "String to display in the arguments prompt."
  :type 'string
  :group 'cannon)

(defcustom cannon-history-size 8
  "Determines on how many recently executed commands should be recorded."
  :type 'integer
  :group 'cannon)

(defvar cannon-cmd-history-list nil
  "Commands history list.")

(defvar cannon-cmd-candidates-list nil
  "Commands candidates list.")

(defvar cannon-mode nil
  "Predicate to verify if cannon was initialized.")

(defcustom cannon-cache-file
  (locate-user-emacs-file "cannon")
  "Cannon cache file.

File in which the launch candidates are saved.
This will provide persistence between Emacs
sessions, variables stored are:
`cannon-cmd-candidates-list' and
`cannon-cmd-history-list'."

  :type 'string
  :group 'cannon)

(defun cannon--check-default-directory ()
  "Check and return a proper `default-directory'.
If default directory is at a remote location the command will
be executed with TRAMP, this behavior isn't desired."
  (if (or (null default-directory)
        (file-remote-p default-directory))
    temporary-file-directory
    default-directory))

(defun cannon-set-process-sentinel ()
  "Set process sentinel to correctly kill the created buffer when process was killed."
  (set-process-sentinel (get-buffer-process (current-buffer))
    (lambda (process event)
      (when (eq 'exit (process-status process))
        (kill-buffer (process-buffer process))))))

(defun cannon-adjust-history-list-size ()
  "Adjust `cannon-cmd-history-list' size, if necessary."
  (when (> (length cannon-cmd-history-list) cannon-history-size)
    (setcdr (nthcdr (- cannon-history-size 1) cannon-cmd-history-list) nil)))

(defun cannon-add-cmd-history-list (cmd)
  "Add CMD to `cannon-cmd-history-list'."
  (unless (member cmd cannon-cmd-history-list)
    (push cmd cannon-cmd-history-list)
    (cannon-adjust-history-list-size)))

(defun cannon-set-cmd-candidates-list ()
  "Scan $PATH (`exec-path') for names of executable files.
Save the list of command candidates in `cannon-cmd-candidates-list'."
  (interactive)
  (let* ((valid-exec-path (seq-uniq
                            (cl-remove-if-not #'file-exists-p
                              (cl-remove-if-not #'stringp exec-path))))
          (files (cl-mapcan
                   (lambda (dir) (directory-files dir t nil nil))
                   valid-exec-path))
          (executable-files (mapcar #'file-name-nondirectory
                              (cl-remove-if #'file-directory-p
                                (cl-remove-if-not #'file-executable-p
                                  files)))))
    ;; unique and sorted command candidates
    (setq cannon-cmd-candidates-list
      (seq-uniq (sort executable-files #'string<)))))

(defun cannon-write-cache-file ()
  "Save cannon list to cache file.
The two list `cannon-cmd-candidates-list' and `cannon-cmd-history-list'
will be saved on cannon cache file `cannon-cache-file'."
  (interactive)
  (with-temp-file (expand-file-name cannon-cache-file)
    (prin1 cannon-cmd-candidates-list (current-buffer))
    (prin1 cannon-cmd-history-list (current-buffer))))

;;;###autoload
(defun cannon-launch (&optional prefix)
  "Interactive launch a command with PREFIX asks for command arguments.
The candidates (executable names) will be parsed from
$PATH environment variable a.k.a (`exec-path').

PREFIX will trigger a secondary prompt that asks for supplementary
command's arguments (no completions are available)."
  (interactive "p")
  (unless cannon-cmd-candidates-list
    (cannon-set-cmd-candidates-list))
  (let* ((line (completing-read cannon-prompt
                 cannon-cmd-candidates-list
                 nil
                 'confirm
                 cannon-cmd-history-list))
          (cmd (car (split-string line)))
          (args (when (= prefix 4)
                  (split-string-and-unquote
                    (read-string cannon-args-prompt)))))
    ;; verify if command was found
    (unless (executable-find cmd)
      (error "Command %s not found" cmd))
    ;; save command name to history list
    (cannon-push-cmd-to-history-list line)
    ;; switch to command buffer (comint mode, subprocess related)
    (switch-to-buffer
      (let*
        ((cmd-list (split-string-and-unquote line))
          (program (car cmd-list))
          (switches (append (cdr cmd-list) args))
          (default-directory (cannon--check-default-directory)))
        ;; execute command: (create a comint in buffer)
        (apply #'make-comint-in-buffer cmd nil program nil switches)))
    ;; set proper process sentinel (will handle process exists)
    (cannon--set-process-sentinel)))

(defun cannon-initialize ()
   "Initialize cannon resources."
   (interactive)
   (add-hook 'kill-emacs-hook 'cannon-write-cache-file))

;;;###autoload
(defun turn-on-cannon-mode ()
  "Turn on cannon-mode unconditionally."
  (interactive)
  (cannon-mode 1))

;;;###autoload
(defun turn-off-cannon-mode ()
  "Turn off cannon-mode unconditionally."
  (interactive)
  (cannon-mode 0))

(provide 'cannon)
;;; cannon.el ends here
