(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun load-env-file (file)
  "Read and set envvars from FILE."
  (if (null (file-exists-p file))
      (signal 'file-error
              (list "No envvar file exists." file
                    "Run `emacs --script ~/.emacs.d/scripts/gen-env-file.el`."))
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

(let ((env-file "~/.emacs.d/local/env.el"))
  (when (file-readable-p env-file)
    (load-env-file env-file)))

(defun cleanup-clutter ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq inhibit-startup-message t
        initial-scratch-message nil))

(defun setup-fonts ()
  ;; See https://www.emacswiki.org/emacs/SetFonts#h5o-16
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "JetBrains Mono")
    (set-face-attribute 'default nil :weight 'light)
    (set-face-attribute 'default nil :height 140))
  )

(defun setup-tree-sitter ()
  (setq treesit-language-source-alist
        '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	        (c "https://github.com/tree-sitter/tree-sitter-c")))
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language (car lang))))
  (setq treesit-load-name-override-list
        '((c++ "libtree-sitter-cpp")))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

(defun setup-custom-file ()
  (setq custom-file "~/.emacs.d/local/custom.el")
  (when (file-readable-p custom-file)
    (load custom-file)))

(use-package emacs
  :ensure nil
  :straight nil
  :config
  (cleanup-clutter)
  (setq-default line-spacing 0.2
	              display-line-numbers-type 'relative
 	              indicate-empty-lines t
	              indent-tabs-mode nil
	              tab-width 2
                require-final-newline t
                file-preserve-symlinks-on-save t)
  (global-display-line-numbers-mode 1)
  (recentf-mode 1)
  (setq history-length 100)
  (savehist-mode 1)
  (save-place-mode 1)
  ;; Only affect clean buffers
  (global-auto-revert-mode 1)
  (electric-pair-mode)
  (setq global-auto-revert-non-file-buffers t)
  (setup-fonts)
  (setup-tree-sitter)
  (fido-mode)
  (setup-custom-file))

(use-package org
  :ensure nil
  :straight nil
  :init
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/agenda"))
  :config
  (setq org-adapt-indentation 'headline-data
        org-hide-leading-stars t))

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
	      ;; Make the mode line borderless
	      '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
	        ,@modus-themes-preset-overrides-warmer))
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package helm
  :bind (("M-i" . helm-imenu)
         ("M-s o" . helm-occur)
         (:map isearch-mode-map
               ("M-s o" . helm-occur-from-isearch))))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

(use-package magit)
