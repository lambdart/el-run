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
  "Very simple and dynamic command/app launcher."
  :group 'extensions
  :group 'convenience)

(defcustom cannon-items-file
  (locate-user-emacs-file "cannon-items")
  "File in which the cannon state is saved between Emacs sessions.
Variables stored are: `cannon-cmd-candidates-list', `cannon-cmd-history-list'.
Must be set before initializing Cannon."
  :type 'string
  :group 'cannon)

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

(defvar cannon-initialized-p nil
  "Predicate to verify if cannon was already initialized.")

(defvar cannon-cmd-history-list nil
  "Commands history list.")

(defvar cannon-cmd-candidates-list nil
  "Commands candidates list.")

(defun cannon--default-directory ()
  "Check and return a proper `default-directory'.
If default directory is at a remote location the command will
be executed with TRAMP, this behavior isn't desire."
  (if (or (null default-directory)
        (file-remote-p default-directory))
    temporary-file-directory
    default-directory))

(defun cannon--process-sentinel ()
  "Set process sentinel to correctly kill the created buffer when process was killed."
  (set-process-sentinel (get-buffer-process (current-buffer))
    (lambda (process event)
      (when (eq 'exit (process-status process))
        (kill-buffer (process-buffer process))))))

(defun cannon--save-cmd-in-history-list (cmd)
  "Save CMD to `cannon-history-list'."
  ;; if command is a member of candidates list, save it in list history
  (if (member (car (split-string cmd)) cannon-cmd-candidates-list)
    (setq cannon-cmd-history-list
      ;; avoid to save the same item on history list
      (cons cmd (remove cmd cannon-cmd-history-list))))
  (cond
    ;; if custom history size is less then 1, set list to nil
    ;; used to avoid unexpected errors
    ((< cannon-history-size 1)
      (setq cannon-cmd-history-list nil))
    ;; if history list length is greater then custom history size
    ;; replace item (command) in the history list
    ((> (length cannon-cmd-history-list) cannon-history-size)
      (setcdr (nthcdr (- cannon-history-size 1) cannon-cmd-history-list) nil))))

(defun cannon-set-cmd-candidates-list ()
  "Scan $PATH (`exec-path') for names of executable files.
Save the list of command candidates in `cannon-cmd-candidates-list'."
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

(defun cannon-parse-cmd-candidates ()
  "Parse launch candidates from history and executable lists."
  (unless cannon-cmd-candidates-list
    (cannon-set-cmd-candidates-list))
  (let ((candidates
          (append cannon-cmd-history-list
            (cl-remove-if (lambda (x)
                            (member x cannon-cmd-history-list))
              cannon-cmd-candidates-list))))
    candidates))

(defun cannon-get-last-cmd ()
  "Get last command executed (if any) from `connan-history-list'."
  (if cannon-cmd-history-list
    (car cannon-cmd-history-list)
    nil))

(defun cannon-save-items ()
  "Save cannon list to cache file.
The two list `cannon-cmd-candidates-list' and `cannon-cmd-history-list'
will be saved on cannon cache file `cannon-items-file'."
  (interactive)
  (with-temp-file (expand-file-name cannon-items-file)
    (prin1 cannon-cmd-candidates-list (current-buffer))
    (prin1 cannon-cmd-history-list (current-buffer))))

(defun cannon-initialize ()
  "Initialize cannon resources."
  (interactive)
  (cannon-initialize-lists)
  (add-hook 'kill-emacs-hook 'cannon-save-items)
  (setq cannon-initialized-p t))

(defun cannon-initialize-lists ()
  "Initialize candidates and history lists."
  (let ((items-file (expand-file-name cannon-items-file)))
    (if (file-readable-p items-file)
      (with-temp-buffer
        (insert-file-contents items-file)
        (ignore-errors
          (setq cannon-cmd-candidates-list (read (current-buffer))
            cannon-cmd-history-list (read (current-buffer)))))
      (setq cannon-cmd-history-list nil
        cannon-cmd-candidates-list nil))))

;;;###autoload
(defun cannon (&optional prefix)
  "Interactive launch a command with PREFIX asks for command arguments.
The candidates (executable names) will be parsed from
$PATH environment variable a.k.a (`exec-path').

PREFIX will trigger a secondary prompt that asks for supplementary
command's arguments (no completions are available)."
  (interactive "p")
  (unless cannon-initialized-p
    (cannon-initialize))
  (let ((cmd (completing-read cannon-prompt
               (cannon-parse-cmd-candidates) nil t))
         (args (when (= prefix 4)
                 (split-string-and-unquote
                   (read-string cannon-args-prompt)))))
    (if (executable-find cmd)
      (progn
        (cannon--save-cmd-in-history-list cmd)
        (switch-to-buffer
          (let* ((cmd-list (split-string-and-unquote cmd))
                  (buffer (generate-new-buffer-name (concat "*" cmd "*")))
                  (program (car cmd-list))
                  (switches (append (cdr cmd-list) args))
                  (default-directory (cannon--default-directory)))
            (apply #'make-comint-in-buffer
              cmd buffer program nil switches)))
        (cannon--process-sentinel))
      (message "Executable %s not found" cmd))))

;;;###autoload
  (defun cannon-alt ()
    "Call `cannon' with prefix argument."
    (interactive)
    (cannon 4))

  (provide 'cannon)
;;; cannon.el ends here
