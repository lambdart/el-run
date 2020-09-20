;;; cannon.el --- Simple dynamic command launcher
;;
;; -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.0.3 Alpha
;; URL: https://github.com/esac-io/cannon
;; Compatibility: GNU Emacs 26.3
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
;; Simulate Dmenu application launcher program
;; to dynamic/interactive launch applications/commands
;; regarding $PATH environment variable using Emacs vanilla builtin
;; facilities (minibuffer/completions).
;;
;; If you use =Exwm=, i.e, =Emacs= also as your =X Window Manager=
;; this package will be a good choice to launch =X11= applications,
;; like: =mpv=, =mupdf=, lxrandr, etc, empowering even more the
;; user experience.
;;
;; In more details, a simple lightweight application launcher that
;; has no outside dependencies (we rely on the =completing-read=
;; built-in =C= function), but if you use some narrowing/selecting
;; framework, like - =helm=, =ivy=, etc - it's easy to use =cannon= as
;; a source for candidates, just call/invoke the function
;; =cannon-get-cmd-candidates= at some point inside you =Elisp=
;; interface code to get the executable candidates.
;;
;; The name is a joke about launching =Chrome= web browser inside
;; =Emacs/Exwm=, a heavily metal ball which causes nightmares.
;;
;; PS: Works pretty well with Icomplete! (rules)!
;;
;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'comint)
(require 'easy-mmode)

(defgroup cannon nil
  "Very simple (exec-path) command launcher."
  :group 'extensions
  :group 'convenience)

(defcustom cannon-minor-mode-string (purecopy " Cannon")
  "String to be display on mode-line."
  :type '(choice string (const :tag "None" nil))
  :group 'cannon)

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

(defcustom cannon-history-size 16
  "A number to determinate how many recently commands should be recorded."
  :type 'integer
  :group 'cannon
  :safe t)

(defcustom cannon-switch-to-buffer-flag t
  "Non-nil means switch to the created process buffer."
  :type 'bool
  :group 'cannon
  :safe t)

(defcustom cannon-kill-buffer-flag t
  "Non-nil means automatically kill buffer when process exits."
  :type 'bool
  :group 'cannon
  :safe t)

(defcustom cannon-debug-message-flag t
  "Non-nil means show debug messages."
  :type 'bool
  :group 'cannon
  :safe t)

(defcustom cannon-cache-file
  (expand-file-name "cannon" user-emacs-directory)
  "Cannon cache file, were the generated list will be saved.

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
  '(cannon-cmd-list cannon-cmd-history-list)
  "List of internal variables.")

(defvar cannon-mode nil
  "Just indicates if `cannon' minor mode was initialized.
Setting this variable has no effect, use \\[cannon-mode] command.")

(defun cannon--message (fmt &rest args)
  "If cannon-debug-message-flag is non-nil invoke `message' \
passing FMT and ARGS."
  (when cannon-debug-message-flag
    (apply 'message fmt args)))

(defun cannon--clean-internal-vars ()
  "Clean `cannon-internal-vars'."
  (dolist (var cannon-internal-vars)
    (set var nil)))

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
  (set-process-sentinel
   (get-buffer-process buffer)
   (lambda (process event)
     (cond
      ;; handle exit process status
      ((eq 'exit (process-status process))
       (when cannon-kill-buffer-flag
         (kill-buffer (process-buffer process))))
      ;; default: do nothing
      (t nil)))))

(defun cannon--make-comint-process (cmd cmd-line &optional args)
  "Start the process defined by CMD, using `apply' and `make-comint-in-buffer'.

CMD      primary command to be executed
CMD-LINE command plus arguments (command line)
ARGS     optional command arguments (switches, etc)"

  ;; parse command list from command line
  (let* ((cmd-list (split-string-and-unquote cmd-line))
         (program (car cmd-list)) ; parse program name
         (buffer (generate-new-buffer-name (concat "*" cmd "*")))
         (switches (append (cdr cmd-list) args)) ; set program switches
         ;; check default-directory to avoid remote execution
         ;; through tramp
         (default-directory (cannon--check-default-directory)))
    ;; execute command: (create a comint in buffer)
    (apply #'make-comint-in-buffer cmd buffer program nil switches)))

(defun cannon--adjust-cmd-history-list ()
  "Adjust (set-rest) `cannon-cmd-history-list' based on \
`cannon-history-size' value."
  (let ((gt (> (length cannon-cmd-history-list) cannon-history-size))
        (n  (- cannon-history-size 1)))
    ;; if greater, adjust the rest of the list (cdr)
    (when gt (setcdr (nthcdr n cannon-cmd-history-list) nil))))

(defun cannon-set-cmd-history-list ()
  "Initialize `cannon-cmd-history-list' from `cannon-cache-file'."
  (let ((cache-file (expand-file-name cannon-cache-file)))
    (when (file-readable-p cache-file)
      (with-temp-buffer
        (insert-file-contents cache-file)
        (ignore-errors
          (setq cannon-cmd-history-list (read (current-buffer))))))))

(defun cannon-set-cmd-list ()
  "Scan $PATH, i.e, \\[exec-path] for names of executable files.
Side effect, save the commands in `cannon-cmd-list' list."
  (let* (
         ;; set variable and filter
         ;; get the unique valid paths and clean
         ;; if isn't a valid a string
         (valid-exec-path
          (seq-uniq (cl-remove-if-not #'file-exists-p
                                      (cl-remove-if-not #'stringp exec-path))))
         ;; return a list of names of files in a directory
         (files (cl-mapcan
                 (lambda (dir)
                   (directory-files dir t nil nil))
                 valid-exec-path))
         ;; filter: clean non-executable and non-regular files
         (executable-files
          (mapcar #'file-name-nondirectory
                  (cl-remove-if #'file-directory-p
                                (cl-remove-if-not
                                 #'file-executable-p files)))))
    ;; side effect: return (set command list),
    ;; unique and sorted command candidates
    (setq cannon-cmd-list
          (seq-uniq
           (sort executable-files #'string<)))))

(defun cannon-initialize-cmd-lists ()
  "Initialize commands lists."
  (cannon-set-cmd-list)
  (cannon-set-cmd-history-list))

(defun cannon-cmd-candidates ()
  "Get command candidates.
Return history plus commands candidates."
  ;; initialize commands lists, if necessary
  (unless cannon-mode (cannon-initialize-cmd-lists))
  ;; get candidates
  (let* ((cmd-history-list
          (cl-loop for cmd in cannon-cmd-history-list
                   collect (car (split-string cmd))))
         (candidates (append cmd-history-list cannon-cmd-list)))
    candidates))

(defun cannon-add-cmd-line-to-history (cmd-line)
  "Add CMD-LINE (command line) to `cannon-cmd-history-list'."
  (unless (member cmd-line cannon-cmd-history-list)
    (push cmd-line cannon-cmd-history-list)
    (cannon--adjust-cmd-history-list)))

(defun cannon-save-cache-file ()
  "Save cannon history list to cache file."
  (with-temp-file (expand-file-name cannon-cache-file)
    (prin1 cannon-cmd-history-list (current-buffer))))

;;;###autoload
(defun cannon-launch (cmd-line &optional args)
  "Launch a system application defined by CMD-LINE.

If \\[universal-argument] prefix, asks for the application
ARGS - arguments in a secondary prompt.

The candidates (executable names) will be parsed from
$PATH environment variable, i.e, \\[exec-path]."

  (interactive
   (list
    ;; map function arguments, if this functions was called interactively
    (completing-read
     cannon-prompt (cannon-cmd-candidates) nil 'confirm nil
     `(cannon-cmd-history-list . 0))

    ;; if prefix, asks for arguments
    (when current-prefix-arg
      (read-string cannon-args-prompt))))

  ;; turn on cannon-mode (if necessary)
  (turn-on-cannon-mode)

  ;; get command line from minibuffer prompt
  (let* ((cmd (car (split-string cmd-line)))
         (args (and args (split-string-and-unquote args))))
    ;; verify if command from command line was found
    (if (or (not cmd) (not (executable-find cmd)))
        (cannon--message "[Cannon]: Error, executable not found")
      ;; execute command (side effect: process buffer created)
      (let ((buffer (cannon--make-comint-process cmd cmd-line args)))
        (cond
         ;; verify if buffer was created
         (buffer
          ;; save command line to history list
          (cannon-add-cmd-line-to-history cmd-line)
          ;; set process sentinel
          (cannon--set-process-sentinel buffer)
          ;; switch to buffer if flag is non-nil
          (when cannon-switch-to-buffer-flag
            (switch-to-buffer buffer)))
         ;; default
         (t (cannon--message "[Cannon]: Error, fail to create *%s* buffer" cmd)))))))

;;;###autoload
(define-minor-mode cannon-mode
  "Define a new minor mode `cannon-mode'.

This defines the toggle command `cannon-mode' and (by default)
a control variable `cannon-mode'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."
  :group cannon
  :lighter cannon-minor-mode-string
  (cond
   (cannon-mode
    ;; initialize commands lists (history and executable)
    (cannon-initialize-cmd-lists)
    ;; add hook to call cannon-save-cache-file
    (add-hook 'kill-emacs-hook 'cannon-save-cache-file)
    ;; set cannon-mode indicator variable to true
    (setq cannon-mode t))
   (t
    ;; save cache
    (cannon-save-cache-file)
    ;; clean internal variables
    (cannon--clean-internal-vars)
    ;; remove hook
    (remove-hook 'kill-emacs-hook 'cannon-save-cache-file)
    ;; set cannon-mode indicator variable to nil (false)
    (setq cannon-mode nil))))

;;;###autoload
(defun turn-on-cannon-mode ()
  "Turn on `cannon-mode'.
See `cannon-launch' for more details."
  (interactive)
  (unless cannon-mode (cannon-mode 1)))

;;;###autoload
(defun turn-off-cannon-mode ()
  "Turn off `cannon-mode'."
  (interactive)
  (when cannon-mode (cannon-mode 0)))

(provide 'cannon)
;;; cannon.el ends here
