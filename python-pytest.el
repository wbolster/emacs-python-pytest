;;; python-pytest.el --- helpers to run pytest -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (magit-popup "2.12.0") (projectile "0.14.0") (s "1.12.0"))
;; Keywords: pytest, test, python, languages, processes, tools
;; URL: https://github.com/wbolster/emacs-python-pytest
;;
;; This file is not part of GNU Emacs.

;;; License:

;; 3-clause "new bsd"; see readme for details.

;;; Commentary:

;;; This package provides helpers to run pytest inside Emacs.

;;; Code:

(require 'comint)
(require 'python)

(require 'dash)
(require 'magit-popup)
(require 'projectile)
(require 's)

(defgroup python-pytest nil
  "pytest integration for emacs"
  :group 'python
  :prefix "python-pytest-")

(defcustom python-pytest-confirm nil
  "Whether to edit the command in the minibuffer before execution.

By default, pytest will be executed without showing a minibuffer prompt.
This can be changed on a case by case basis by using a prefix argument
\(\\[universal-argument]\) when invoking a command.

When t, this toggles the behaviour of the prefix argument."
  :group 'pytest
  :type 'boolean)

(defcustom python-pytest-executable "pytest"
  "The name of the pytest executable."
  :group 'pytest
  :type 'string)

(defcustom python-pytest-started-hooks nil
  "Hooks to run before a pytest process starts."
  :group 'pytest
  :type 'hook)

(defcustom python-pytest-finished-hooks nil
  "Hooks to run after a pytest process finishes."
  :group 'pytest
  :type 'hook)

(defcustom python-pytest-buffer-name "*pytest*"
  "Name of the pytest output buffer."
  :group 'pytest
  :type 'string)

(defcustom python-pytest-project-name-in-buffer-name t
  "Whether to include the project name in the buffer name.

This is useful when working on multiple projects simultaneously."
  :group 'pytest
  :type 'boolean)

(defcustom python-pytest-pdb-track t
  "Whether to automatically track output when pdb is spawned.

This results in automatically opening source files during debugging."
  :group 'pytest
  :type 'boolean)

(defvar python-pytest--history nil
  "History for pytest invocations.")

(defvar python-pytest--last-command nil
  "Command line used for the last run.")

;;;###autoload (autoload 'python-pytest-popup "pytest" nil t)
(magit-define-popup python-pytest-popup
  "Show popup for running pytest."
  'python-pytest
  :switches
  '((?c "color" "--color" t)
    (?d "debug on error" "--pdb")
    (?f "failed first" "--failed-first")
    (?l "show locals" "--showlocals")
    (?q "quiet" "--quiet")
    (?s "do not capture output" "--capture=no")
    (?t "do not cut tracebacks" "--full-trace")
    (?v "verbose" "--verbose")
    (?x "exit after first failure" "--exitfirst"))
  :options
  '((?k "only names matching expression" "-k")
    (?m "only marks matching expression" "-m")
    (?t "traceback style" "--tb=" python-pytest--choose-traceback-style)
    (?x "exit after N failures or errors" "--maxfail="))
  :actions
  '("Run tests"
    (?t "Test all" python-pytest)
    (?x "Test last-failed" python-pytest-last-failed)
    "Run tests for current context"
    (?f "Test file" python-pytest-file-dwim)
    (?F "Test this file  " python-pytest-file)
    (?d "Test def/class" python-pytest-function-dwim)
    "Repeat tests"
    (?r "Repeat last test run" python-pytest-repeat))
  :max-action-columns 3
  :default-action 'python-pytest-repeat)

;;;###autoload
(defun python-pytest (&optional args)
  "Run pytest with ARGS.

With a prefix argument, allow editing."
  (interactive (list (python-pytest--arguments)))
  (python-pytest-run
   :args args
   :edit current-prefix-arg))

;;;###autoload
(defun python-pytest-file (file &optional args)
  "Run pytest on FILE, using ARGS.

Additional ARGS are passed along to pytest.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (python-pytest--arguments)))
  (when (file-name-absolute-p file)
    (setq file (file-relative-name file (python-pytest--project-root))))
  (python-pytest-run
   :args args
   :file file
   :edit current-prefix-arg))

;;;###autoload
(defun python-pytest-file-dwim (file &optional args)
  "Run pytest on FILE, intelligently finding associated test modules.

When run interactively, this tries to work sensibly using
the current file.

Additional ARGS are passed along to pytest.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (python-pytest--arguments)))
  (python-pytest-file (python-pytest--sensible-test-file file) args))

;;;###autoload
(defun python-pytest-function-dwim (file func args)
  "Run pytest on FILE with FUNC (or class).

When run interactively, this tries to work sensibly using
the current file and function around point.

Additional ARGS are passed along to pytest.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (python-pytest--current-defun)
    (python-pytest--arguments)))
  (unless func
    (user-error "No class/function found"))
  (let ((test-file (python-pytest--sensible-test-file file)))
    (when (file-name-absolute-p test-file)
      (setq test-file (file-relative-name test-file (python-pytest--project-root))))
    (unless (python-pytest--test-file-p file)
      (setq func (python-pytest--make-test-name func)))
    (setq func (s-replace "." "::" func))
    (python-pytest-run
     :args args
     :file test-file
     :func func
     :edit current-prefix-arg)))

;;;###autoload
(defun python-pytest-last-failed (&optional args)
  "Run pytest, only executing previous test failures.

Additional ARGS are passed along to pytest.
With a prefix argument, allow editing."
  (interactive (list (python-pytest--arguments)))
  (python-pytest-run
   :args (-snoc args "--last-failed")
   :edit current-prefix-arg))

;;;###autoload
(defun python-pytest-repeat ()
  "Run pytest with the same argument as the most recent invocation.

With a prefix ARG, allow editing."
  (interactive)
  (unless python-pytest--last-command
    (user-error "No previous pytest run"))
  (python-pytest-run-command
   :command python-pytest--last-command
   :edit current-prefix-arg))


;; internal helpers

(define-derived-mode python-pytest-mode
  comint-mode "pytest"
  "Major mode for pytest sessions (derived from comint-mode).")

(cl-defun python-pytest-run (&key args file func edit)
  "Run pytest for the given arguments."
  (let ((what))
    (setq args (cons python-pytest-executable args))
    (when file
      (setq what (shell-quote-argument file))
      (when func
        (setq what (format "%s::%s" what (shell-quote-argument func))))
      (setq args (-snoc args what)))
    (python-pytest-run-command
     :command (s-join " " args)
     :edit edit)))

(cl-defun python-pytest-run-command (&key command edit)
  "Run a pytest command line."
  (let* ((default-directory (python-pytest--project-root)))
    (when python-pytest-confirm
      (setq edit (not edit)))
    (when edit
      (setq command
            (read-from-minibuffer
             "Command: "
             command nil nil 'python-pytest--history)))
    (setq python-pytest--last-command command)
    (python-pytest-run-as-comint command)))

(defun python-pytest-run-as-comint (command)
  "Run a pytest comint session for COMMAND."
  (let* ((buffer (get-buffer-create (python-pytest--make-buffer-name)))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (when (comint-check-proc buffer)
        (unless (or compilation-always-kill
                    (yes-or-no-p "Kill running pytest process?"))
          (user-error "Aborting; pytest still running")))
      (when process
        (delete-process process))
      (erase-buffer)
      (kill-all-local-variables)
      (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
      (python-pytest-mode)
      (when python-pytest-pdb-track
        (add-hook
         'comint-output-filter-functions
         'python-pdbtrack-comint-output-filter-function
         nil t))
      (make-comint-in-buffer "pytest" buffer "sh" nil "-c" command)
      (run-hooks 'python-pytest-started-hooks)
      (setq process (get-buffer-process buffer))
      (set-process-sentinel process #'python-pytest--process-sentinel)
      (display-buffer buffer))))

(defun python-pytest--shell-quote (s)
  "Quote S for use in a shell command. Like `shell-quote-argument', but prettier."
  (if (s-equals-p s (shell-quote-argument s))
      s
    (format "'%s'" (s-replace "'" "'\"'\"'" s))))

(defun python-pytest--make-buffer-name ()
  "Make a buffer name for the compilation buffer."
  (let ((name python-pytest-buffer-name))
    (when python-pytest-project-name-in-buffer-name
      (setq name (format "%s<%s>" name (python-pytest--project-name))))
    name))

(defun python-pytest--process-sentinel (proc _state)
  "Process sentinel helper to run hooks after PROC finishes."
  (with-current-buffer (process-buffer proc)
    (run-hooks 'python-pytest-finished-hooks)))

(defun python-pytest--arguments ()
  "Return the current arguments in a form understood by pytest."
  (let ((args (python-pytest-arguments)))
    (setq args (python-pytest--switch-to-option
                args "--color" "--color=yes" "--color=no"))
    args))

(defun python-pytest--switch-to-option (args name on-replacement off-replacement)
  "Look in ARGS for switch NAME and turn it into option with a value.

When present ON-REPLACEMENT is substituted, else OFF-REPLACEMENT is appended."
  (if (-contains-p args name)
      (-replace name on-replacement args)
    (-snoc args off-replacement)))

(defun python-pytest--choose-traceback-style (prompt _value)
  "Helper to choose a pytest traceback style using PROMPT."
  (completing-read
   prompt '("long" "short" "line" "native" "no") nil t))


;; python helpers

(defun python-pytest--current-defun ()
  "Detect the current function/class (if any)."
  (save-excursion
    (let ((name (python-info-current-defun)))
      (unless name
        ;; jumping seems to make it work on empty lines.
        ;; todo: this could perhaps be improved.
        (python-nav-beginning-of-defun)
        (python-nav-forward-statement)
        (setq name (python-info-current-defun)))
      name)))

(defun python-pytest--make-test-name (func)
  "Turn function name FUNC into a name (hopefully) matching its test name.

Example: ‘MyABCThingy.__repr__’ becomes ‘test_my_abc_thingy_repr’."
  (-as->
   func s
   (s-replace "." "_" s)
   (s-snake-case s)
   (s-replace-regexp "_\+" "_" s)
   (s-chop-suffix "_" s)
   (s-chop-prefix "_" s)
   (format "test_%s" s)))


;; file/directory helpers

(defun python-pytest--project-name ()
  "Find the project name."
  (projectile-project-name))

(defun python-pytest--project-root ()
  "Find the project root directory."
  (projectile-project-root))

(defun python-pytest--test-file-p (file)
  "Tell whether FILE is a test file."
  (projectile-test-file-p file))

(defun python-pytest--find-test-file (file)
  "Find a test file associated to FILE, if any."
  (let ((test-file (projectile-find-matching-test file)))
    (unless test-file
      (user-error "No test file found"))
    test-file))

(defun python-pytest--sensible-test-file (file)
  "Return a sensible test file name for FILE."
  (if (python-pytest--test-file-p file)
      (file-relative-name file (python-pytest--project-root))
    (python-pytest--find-test-file file)))

(provide 'python-pytest)
;;; python-pytest.el ends here
