;;; cannon.el --- Simple dynamic command launcher -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; Homepage: https://github.com/lambdart/cannon
;; Keywords: app, launch, unix, dmenu
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
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

(defcustom cannon-prompt "Run: "
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

(defcustom cannon-mode-hook nil
  "Hooks to un after the mode was turned on."
  :group 'cannon
  :type 'hook)

(defcustom cannon-history-file
  (expand-file-name "cache/cannon-history" user-emacs-directory)
  "Cache history command file, were the command history will be saved."
  :type 'string
  :group 'cannon
  :safe t)

(defcustom cannon-cache-file
  (expand-file-name "cache/cannon-cache" user-emacs-directory)
  "Cache command file, were the generated command list will be saved."
  :type 'string
  :group 'cannon
  :safe t)

(defcustom cannon-message-prefix "[Cannon]: "
  "Cannon debug message prefix."
  :type 'string
  :group 'cannon
  :safe t)

(defvar cannon-cmd-history-list nil
  "Commands history list.")

(defvar cannon-cmd-list nil
  "Commands list.")

(defvar cannon-internal-vars
  '(cannon-cmd-list
    cannon-cmd-history-list)
  "List of internal variables.")

(defvar cannon-mode nil
  "Just indicates if `cannon' minor mode was initialized.
Setting this variable has no effect, use \\[cannon-mode] command instead.")

(defmacro cannon--debug-message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when cannon-debug-message-flag
     (message (concat cannon-message-prefix ,fmt) ,@args)))

(defun cannon--clean-internal-vars ()
  "Clean `cannon-internal-vars'."
  (dolist (var cannon-internal-vars)
    (set var nil)))

(defun cannon--default-directory ()
  "Return a proper `default-directory'.
If default directory is an remote one the command will
be executed with TRAMP, this behavior isn't desired."
  (if (or (null default-directory)
          (file-remote-p default-directory))
      temporary-file-directory
    default-directory))

(defun cannon--process-sentinel (process _event)
  "Cannon default PROCESS handler sentinel function."
  ;; handle exit process status
  (and (eq 'exit (process-status process))
       cannon-kill-buffer-flag
       (kill-buffer (process-buffer process))))

(defun cannon--set-process-sentinel (buffer)
  "Set process sentinel related to BUFFER.
Correctly handle process exit status, etc.."
  (set-process-sentinel (get-buffer-process buffer)
                        'cannon--process-sentinel))

(defun cannon--make-comint-process (cmd cmd-line &optional args)
  "Start the process defined by CMD, using `apply' and `make-comint-in-buffer'.
It's possible to use the CMD-LINE (command plus arguments).
Optional command list of ARGS (switches)."
  ;; parse command list from command line
  (let* ((cmd-list (split-string-and-unquote cmd-line))
         (program (car cmd-list)) ; parse program name
         (buffer (generate-new-buffer-name (concat "*" cmd "*")))
         (switches (append (cdr cmd-list) args)) ; set program switches
         ;; check default-directory to avoid remote execution
         (default-directory (cannon--default-directory)))
    ;; execute command: (create a comint in buffer)
    (apply #'make-comint-in-buffer cmd buffer program nil switches)))

(defun cannon--adjust-cmd-history-size ()
  "Adjust `cannon-cmd-history-list' based on `cannon-history-size' value."
  (and (> (length cannon-cmd-history-list) cannon-history-size)
       (setcdr (nthcdr (- cannon-history-size 1)
                       cannon-cmd-history-list)
               nil)))

(defun cannon--write-cache-file (file entries)
  "Write ENTRIES in the target cache FILE."
  (with-temp-file (expand-file-name file)
    (prin1 entries (current-buffer))))

(defun cannon--read-cached-entries (file)
  "Read cache FILE entries."
  (let ((cache-file (expand-file-name file)))
    (and (file-readable-p cache-file)
         (read (with-temp-buffer
                 (insert-file-contents cache-file)
                 (buffer-substring-no-properties (point-min)
                                                 (point-max)))))))

(defun cannon--parse-cmd-entries ()
  "Parse command entries defined in $PATH environment variable."
  (let* ((dirs (cl-remove-duplicates
                (cl-remove-if-not #'file-exists-p
                                  (cl-remove-if-not #'stringp
                                                    exec-path))))
         ;; parse regular files
         (files (cl-mapcan (lambda (dir)
                             (directory-files dir t nil nil))
                           dirs))
         ;; set only executable files: commands
         (cmds (mapcar #'file-name-nondirectory
                       (cl-remove-if #'file-directory-p
                                     (cl-remove-if-not
                                      #'file-executable-p files)))))
    (cl-remove-duplicates (sort cmds #'string<))))

(defun cannon-set-cmd-list ()
  "Set command list from `cannon-cache-file' or create it."
  (or cannon-cmd-list
      (let ((cached-entries (cannon--read-cached-entries cannon-cache-file)))
        (setq cannon-cmd-list
              (or cached-entries
                  (cannon--write-cache-file cannon-cache-file
                                            (cannon--parse-cmd-entries)))))))

(defun cannon-set-cmd-history-list ()
  "Set command list from `cannon-history-file'."
  (or cannon-cmd-history-list
      (setq cannon-cmd-history-list
            (cannon--read-cached-entries cannon-history-file))))

(defun cannon-cmd-completions ()
  "Return command completions entries merged."
  (cl-union (cannon-set-cmd-history-list)
            (cannon-set-cmd-list)))

(defun cannon-add-cmd-line-to-history (cmd-line)
  "Add CMD-LINE to `cannon-cmd-history-list'."
  (unless (member cmd-line cannon-cmd-history-list)
    (push cmd-line cannon-cmd-history-list)
    (cannon--adjust-cmd-history-size)))

(defun cannon-save-history ()
  "Save history to `cannon-history-file'."
  (cannon--write-cache-file cannon-history-file
                            cannon-cmd-history-list))

(defun cannon-minibuffer-read (&optional arg)
  "Read 'cmd-line' and its arguments if ARG is non-nil."
  ;; get command line from minibuffer prompt
  (let ((cmd-line (completing-read cannon-prompt
                                   (cannon-cmd-completions)
                                   nil 'confirm nil))
        ;; asks for arguments if necessary
        (args (and arg (read-string cannon-args-prompt))))
    ;; return cmd-line and args list
    (list cmd-line args)))

(defun cannon-launch (cmd-line &optional args)
  "Launch a system application defined by CMD-LINE.

If ARGS is non-nil, asks for the application
arguments in a secondary prompt.

The candidates (executable names) will be parsed from
$PATH environment variable, i.e, \\[exec-path]."
  ;; maps command argument using the minibuffer facility
  (interactive (cannon-minibuffer-read current-prefix-arg))
  ;; function body:
  (let* ((cmd (car (split-string cmd-line)))
         (args (and args (split-string-and-unquote args)))
         (default-directory (cannon--default-directory)))
    ;; verify if command from command line was found
    (if (or (not cmd) (not (executable-find cmd)))
        (cannon--debug-message "Error, executable not found")
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
          (and cannon-switch-to-buffer-flag (switch-to-buffer buffer)))
         ;; default: debug message
         (t (cannon--debug-message "Error, fail to create *%s* buffer" cmd)))))))

;;;###autoload
(defun cannon-echo-mode-state ()
  "Show cannon minor mode state: on|off."
  (interactive)
  (message "[Cannon]: status: %s" (if cannon-mode "on" "off")))

;;;###autoload
(define-minor-mode cannon-mode
  "Define a new minor mode `cannon-mode'.

This defines the toggle command `cannon-mode' and (by default)
a control variable `cannon-mode'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :group 'cannon
  :global t
  :lighter cannon-minor-mode-string
  (cond
   (cannon-mode
    ;; add hook to save the commands history before Emacs was killed
    (add-hook 'kill-emacs-hook 'cannon-save-history)
    ;; run hooks
    (run-hooks cannon-mode-hook)
    ;; set cannon-mode indicator variable to true
    (setq cannon-mode t))
   (t
    ;; save commands history
    (cannon-save-history)
    ;; clean internal variables
    (cannon--clean-internal-vars)
    ;; remove hook
    (remove-hook 'kill-emacs-hook 'cannon-save-history)
    ;; set cannon-mode indicator variable to nil (false)
    (setq cannon-mode nil))))

;;;###autoload
(defun turn-on-cannon-mode ()
  "Activate the minor mode."
  (interactive)
  ;; turn on if wasn't already initialized
  (cannon-mode 1)
  ;; show cannon mode state
  (cannon-echo-mode-state))

(defun turn-off-cannon-mode ()
  "Deactivate the minor mode."
  (interactive)
  ;; turn off if necessary
  (cannon-mode 0)
  ;; show cannon mode state
  (cannon-echo-mode-state))

(provide 'cannon)

;;; cannon.el ends here
