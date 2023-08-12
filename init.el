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

(setq-default line-spacing 0.2
	            display-line-numbers-type 'relative
 	            indicate-empty-lines t
	            indent-tabs-mode nil
	            tab-width 2
              require-final-newline t
              file-preserve-symlinks-on-save t)

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(recentf-mode 1)
(setq history-length 100)
(savehist-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
;; Only affect clean buffers
(setq global-auto-revert-non-file-buffers t)
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; See https://www.emacswiki.org/emacs/SetFonts#h5o-16
(when (eq system-type 'darwin)
  ;; default Latin font
  (set-face-attribute 'default nil :family "JetBrains Mono")
  (set-face-attribute 'default nil :weight 'light)
  (set-face-attribute 'default nil :height 140)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  ;;(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )

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

(load-env-file "~/.emacs.d/local/env.el")

(setq treesit-language-source-alist
      '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	      (c "https://github.com/tree-sitter/tree-sitter-c")))

(setq treesit-load-name-override-list
      '((c++ "libtree-sitter-cpp")))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))

(setq org-adapt-indentation t
      org-hide-leading-stars t)

(fido-mode)

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
	      ;; Make the mode line borderless
	      '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
	        ,@modus-themes-preset-overrides-warmer))
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package helm
  :bind (("M-i" . helm-imenu)
         ("M-s o" . helm-occur)
         (:map isearch-mode-map
               ("M-s o" . helm-occur-from-isearch))))

(use-package nov
  :confiog
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

(use-package magit)
