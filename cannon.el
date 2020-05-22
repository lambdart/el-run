;;; cannon.el --- Simple dynamic command launcher
;;
;; -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.2
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
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
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
;; Simulate Dmenu command cmd-line program to dynamic/interactive
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
  :group 'cannon
  :safe t)

(defcustom cannon-args-prompt "Args: "
  "String to display in the arguments prompt."
  :type 'string
  :group 'cannon
  :safe t)

(defcustom cannon-history-size 8
  "Determines on how many recently executed commands should be recorded."
  :type 'integer
  :group 'cannon
  :safe t)

(defcustom cannon-switch-to-buffer-flag t
  "Non-nil means will automatically switch to the created buffer."
  :type 'bool
  :group 'cannon
  :safe t)

(defcustom cannon-cache-file
  (expand-file-name "cannon" user-emacs-directory)
  "Cannon cache file.

File in which the launch candidates are save.
This will provide persistence between Emacs sessions,
variables stored are: `cannon-cmd-list' and `cannon-cmd-history-list'."

  :type 'string
  :group 'cannon
  :safe nil)

(defvar cannon-cmd-history-list nil
  "Commands history list.")

(defvar cannon-cmd-list nil
  "Commands list.")

(defvar cannon-internal-vars
  '(cannon-cmd-list
     cannon-cmd-history-list)
  "Predicate to verify if cannon was initialized.")

(defvar cannon-mode nil
  "Predicate to verify if cannon was initialized.")

(defun cannon--check-default-directory ()
  "Check and return a proper `default-directory'.

If default directory is at a remote location the command will
be executed with TRAMP, this behavior isn't desired."

  (if (or (null default-directory)
        (file-remote-p default-directory))
    temporary-file-directory
    default-directory))

(defun cannon--set-process-sentinel (buffer)
  "Set process sentinel related to BUFFER.
Correctly handle process exit status, etc.."
  (set-process-sentinel (get-buffer-process buffer)
    (lambda (process event)
      (when (eq 'exit (process-status process))
        (kill-buffer (process-buffer process))))))

(defun cannon--make-comint-process (cmd cmd-line &optional args)
  "Start the process defined by CMD, using `apply' and `make-comint-in-buffer'.

CMD primary command to be executed
CMD-LINE command plus arguments (command line)
ARGS optional command arguments (switches, etc)"

  ;; parse command list from command line
  (let* ((cmd-list (split-string-and-unquote cmd-line))
          (program (car cmd-list)) ; parse program name
          (buffer (generate-new-buffer-name (concat "*" cmd "*")))
          (switches (append (cdr cmd-list) nil)) ; set program switches
          ;; check default-directory to avoid remote execution
          ;; through tramp
          (default-directory (cannon--check-default-directory)))
    ;; execute command: (create a comint in buffer)
    (apply #'make-comint-in-buffer cmd buffer program nil switches)))

(defun cannon--adjust-history-size ()
  "Adjust history list size."
  (when (> (length cannon-cmd-history-list) cannon-history-size)
    (setcdr (nthcdr (- cannon-history-size 1) cannon-cmd-history-list) nil)))

(defun cannon-set-cmd-history-list ()
  "Initialize `cannon-cmd-history-list' from `cannon-cache-file'."
  (let ((cache-file (expand-file-name cannon-cache-file)))
    (when (file-readable-p cache-file)
      (with-temp-buffer (insert-file-contents cache-file)
        (ignore-errors
          (setq cannon-cmd-history-list (read (current-buffer))))))))

(defun cannon-set-cmd-list ()
  "Scan $PATH (`exec-path') for names of executable files.
Side effect: save the command list in `cannon-cmd-list'."
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
    (setq cannon-cmd-list
      (seq-uniq (sort executable-files #'string<)))))

(defun cannon-candidates ()
  "Return history plus commands candidates."
  (let ((candidates (append cannon-cmd-history-list
                      (cl-remove-if
                        (lambda (x)
                          (member x cannon-cmd-history-list))
                        cannon-cmd-list))))
    candidates))

(defun cannon-add-cmd-to-history (cmd)
  "Add CMD (command) to `cannon-cmd-history-list'."
  (unless (member cmd cannon-cmd-history-list)
    (push cmd cannon-cmd-history-list)
    (cannon--adjust-history-size)))

(defun cannon-save-history ()
  "Save cannon history list to cache file."
  (with-temp-file (expand-file-name cannon-cache-file)
    (prin1 cannon-cmd-history-list (current-buffer))))

;;;###autoload
(defun cannon-launch (&optional prefix)
  "Interactive launch a command with PREFIX asks for command arguments.

The candidates (executable names) will be parsed from
$PATH environment variable a.k.a (`exec-path').

PREFIX will trigger a secondary prompt that asks for supplementary
command's arguments (no completions are available)."
  (interactive "p")
  ;; set command candidates if necessary
  (unless cannon-mode
    (cannon-mode 1))
  ;; get command line from minibuffer prompt
  ;; verify command and maybe set args
  (let* ((cmd-line
           (completing-read cannon-prompt (cannon-candidates)
             nil 'confirm nil `(cannon-cmd-history-list . 0)))
          (cmd (car (split-string cmd-line)))
          (args (when (= prefix 4)
                  (split-string-and-unquote
                    (read-string cannon-args-prompt)))))
    ;; verify if command from command line was found
    (unless (executable-find cmd)
      (error "Command %s not found" cmd))
    ;; save command line to history list
    (cannon-add-cmd-to-history cmd)
    ;; execute command (side effect: return buffer)
    ;; and switch to buffer
    (let* ((buffer
             (cannon--make-comint-process cmd cmd-line args)))
      ;; verify if buffer was created
      (unless buffer
        (error "Was not possible to create *%s* buffer" cmd))
      ;; set process sentinel
      (cannon--set-process-sentinel buffer)
      ;; if switch to buffer if flag is non-nil
      (when cannon-switch-to-buffer-flag
        (switch-to-buffer buffer)))))

;;;###autoload
(define-minor-mode cannon-mode
  "Toggle cannon-mode on/off." nil "Cannon" nil
  (if cannon-mode
    ;; Enabling:
    (condition-case error
      (progn
        ;; set lists
        (cannon-set-cmd-list)
        (cannon-set-cmd-history-list))
      ;; add hooks
      (add-hook 'kill-emacs-hook 'cannon-save-history)
      (error
        (cannon-mode 0)
        (signal (car error) (cdr error))))
    ;; Disabling:
    (remove-hook 'kill-emacs-hook 'cannon-save-history)
    (dolist (var cannon-internal-vars)
      (set var nil))))

(provide 'cannon)
;;; cannon.el ends here
