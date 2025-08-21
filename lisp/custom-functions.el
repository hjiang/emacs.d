;;; custom-functions.el --- Custom utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains custom utility functions extracted from init.el
;; for better organization and maintainability.

;;; Code:

;;; Editing Utilities

(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun smart-split ()
  "Split the window into 100-column sub-windows."
  (interactive)
  (cl-labels ((smart-split-helper (w)
                                  (if (> (window-width w) 180)
                                      (let ((w2 (split-window w 100 t)))
                                        (smart-split-helper w2)))))
    (smart-split-helper nil)))

;;; Environment and Configuration

(defun load-env-file (file)
  "Read and set envvars from FILE."
  (if (null (file-exists-p file))
      (signal 'file-error
              (list "No envvar file exists." file
                    "See https://github.com/hjiang/envel."))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let (env (read (current-buffer)))
        (let ((tz (getenv-internal "TZ")))
          (setq-default
           process-environment
           (append env (default-value 'process-environment))
           exec-path
           (append (split-string (getenv "PATH") path-separator t)
                   (list exec-directory))
           shell-file-name
           (or (getenv "SHELL")
               (default-value 'shell-file-name)))
          (when-let (newtz (getenv-internal "TZ"))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

(defun reload-env (&optional env-file)
  "Reload environment variables from ENV-FILE.
If ENV-FILE is not provided, uses ~/.emacs.d/.local/env.el"
  (interactive)
  (let ((file (or env-file "~/.emacs.d/.local/env.el")))
    (if (file-readable-p file)
        (progn
          (load-env-file file)
          (message "Environment variables reloaded from %s" file))
      (message "Environment file not found: %s" file))))

(defun setup-custom-file ()
  "Set up and load custom file."
  (setq custom-file "~/.emacs.d/.local/custom.el")
  (when (file-readable-p custom-file)
    (load custom-file)))

(defun auto-compile-init-file ()
  "Automatically compile init file when it's saved."
  (when (equal buffer-file-name (expand-file-name "~/.emacs.d/init.el"))
    (byte-compile-file buffer-file-name)))

;;; UI Setup

(defun cleanup-clutter ()
  "Remove UI clutter (menu bar, tool bar, scroll bar, startup messages)."
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (if (display-graphic-p)
      (scroll-bar-mode -1))
  (setq inhibit-startup-message t
        initial-scratch-message nil))

(defun setup-fonts ()
  "Set up fonts for different operating systems."
  ;; See https://www.emacswiki.org/emacs/SetFonts#h5o-16
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "FiraCode Nerd Font Mono")
    (set-face-attribute 'default nil :weight 'light)
    (set-face-attribute 'default nil :height 145))
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Fira Code"
                        :weight 'light
                        :height 135)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Noto Sans CJK SC"
                                   :weight 'regular
                                   :size 18)))))

(defun maybe-setup-macos ()
  "Set up macOS-specific configuration."
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'super
          mac-command-modifier 'meta)
    (pixel-scroll-precision-mode 1)))

;;; Development Tools

(defun setup-tree-sitter ()
  "Set up tree-sitter language grammars and mode mappings."
  (interactive)
  (defvar treesit-language-source-alist)
  (setq treesit-language-source-alist
        '((cpp . "https://github.com/tree-sitter/tree-sitter-cpp")
	        (c . "https://github.com/tree-sitter/tree-sitter-c")
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                         "master" "typescript/src"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (gdscript . ("https://github.com/PrestonKnopp/tree-sitter-gdscript" "master" "src"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src"))))
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))
  (setq treesit-load-name-override-list
        '((c++ "libtree-sitter-cpp")))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode)))

(defun elixir-format-buffer ()
  "Format the current buffer using mix format."
  (when (and (or (eq major-mode 'elixir-mode)
                 (eq major-mode 'elixir-ts-mode))
             (executable-find "mix"))
    (let ((file (buffer-file-name)))
      (when file
        (call-process "mix" nil nil nil "format" file)
        (revert-buffer nil t t)))))

;;; File Utilities

(defun read-file-or-nil (filename)
  "Read file FILENAME, returning the contents as a string, or nil if it doesn't exist."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string))
    (file-error nil)))

(provide 'custom-functions)
;;; custom-functions.el ends here