;;; cannon.el --- Simple dynamic command launcher
;;
;; -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.2
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
;; Simulate Dmenu command cmd-line program to dynamic/interactive
;; launch applications/commands regarding $PATH
;; environment variable using Emacs vanilla builtin
;; facilities (minibuffer/completions).
;;
;; If you use =Exwm=, i.e, =Emacs= also as your =Xorg Window Manager=
;; this package will be a good choice to launch =X11= applications,
;; like: =mpv=, =mupdf=, empowering even more the user experience.
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

;;;###autoload
(defcustom cannon-minor-mode-string (purecopy " Cannon")
  "String to display in mode line when Cannon Mode is enabled; nil for none."
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
  "Indicates if Cannon Mode was initialized.")

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
         (switches (append (cdr cmd-list) nil)) ; set program switches
         ;; check default-directory to avoid remote execution
         ;; through tramp
         (default-directory (cannon--check-default-directory)))
    ;; execute command: (create a comint in buffer)
    (apply #'make-comint-in-buffer cmd buffer program nil switches)))

(defun cannon--clean-cmd-history-list ()
  "Clean command history list, adjust its size."
  (when (> (length cannon-cmd-history-list)
           cannon-history-size)
    (setcdr (nthcdr (- cannon-history-size 1)
                    cannon-cmd-history-list)
            nil)))

(defun cannon-set-cmd-history-list ()
  "Initialize `cannon-cmd-history-list' from `cannon-cache-file'."
  (let ((cache-file (expand-file-name cannon-cache-file)))
    (when (file-readable-p cache-file)
      (with-temp-buffer
        (insert-file-contents cache-file)
        (ignore-errors
          (setq cannon-cmd-history-list
                (read (current-buffer))))))))

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

(defun cannon-get-cmd-candidates ()
  "Get command candidates.
Return history plus commands candidates."
  (let* ((cmd-history-list
          (cl-loop for cmd in cannon-cmd-history-list
                   collect (car (split-string cmd))))
         (candidates
          (append cmd-history-list
                  (cl-remove-if
                   (lambda (cmd)
                     (member cmd cmd-history-list))
                   cannon-cmd-list))))
    candidates))

(defun cannon-add-cmd-line-to-history (cmd-line)
  "Add CMD-LINE (command line) to `cannon-cmd-history-list'."
  (unless (member cmd-line cannon-cmd-history-list)
    (push cmd-line cannon-cmd-history-list)
    (cannon--clean-cmd-history-list)))

(defun cannon-save-cache-file ()
  "Save cannon history list to cache file."
  (with-temp-file (expand-file-name cannon-cache-file)
    (prin1 cannon-cmd-history-list (current-buffer))))

;;;###autoload
(defun cannon-launch (&optional prefix)
  "Launch system application.

With universal \\[universal-argument] PREFIX
asks for the application arguments in a
secondary prompt.

The candidates (executable names) will be parsed from
$PATH environment variable, i.e, \\[exec-path]."

  (interactive "P")
  ;; set command candidates if necessary
  (unless cannon-mode (cannon-mode 1))
  ;; get command line from minibuffer prompt
  (let* ((cmd-line (completing-read cannon-prompt
                                    (cannon-get-cmd-candidates)
                                    nil 'confirm nil
                                    `(cannon-cmd-history-list . 0)))
         ;; set cmd
         (cmd (car (split-string cmd-line)))
         ;; verify universal argument
         (args (when prefix
                 (split-string-and-unquote
                  (read-string cannon-args-prompt)))))

    ;; verify if command from command line was found
    (if (or (not cmd) (not (executable-find cmd)))
        (message "Command not found")
      ;; execute command (side effect: process buffer created)
      (let* ((buffer (cannon--make-comint-process cmd cmd-line args)))
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
         (t (message "Creating *%s* buffer fail" cmd)))))))

;;;###autoload
(define-minor-mode cannon-mode
  "Toggle cannon-mode application launcher.
With a prefix argument ARG, enable Cannon-mode if ARG
is positive, and disable it otherwise."
  :group cannon :lighter cannon-minor-mode-string
  ;; "Cannon" nil
  (cond
   (cannon-mode
    ;; initialize lists
    (cannon-set-cmd-list)
    (cannon-set-cmd-history-list)
    ;; add hook to call cannon-save-cache-file
    (add-hook 'kill-emacs-hook 'cannon-save-cache-file))
   (t
    ;; clean internal variables
    (dolist (var cannon-internal-vars)
      (set var nil))
    ;; save cache
    (cannon-save-cache-file)
    ;; remove hook
    (remove-hook 'kill-emacs-hook 'cannon-save-cache-file))))

;;;###autoload
(defun turn-on-cannon-mode ()
  "Turn on `cannon-mode'.
See `cannon-launch' for more details."
  (interactive)
  (unless cannon-mode (cannon-mode 1)))

;;;###autoload
(defun turn-off-cannon-mode ()
  "Turn on `cannon-mode'.
See `cannon-launch' for more details."
  (interactive)
  (when cannon-mode (cannon-mode 0)))

(provide 'cannon)
;;; cannon.el ends here
