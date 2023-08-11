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
 	      indicate-empty-lines t)

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

;; See https://www.emacswiki.org/emacs/SetFonts#h5o-16
(when (eq system-type 'darwin)
  ;; default Latin font
  (set-face-attribute 'default nil :family "JetBrains Mono")
  (set-face-attribute 'default nil :weight 'thin)
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

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
	'(
	  ;; Make the mode line borderless
	  (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
	  ,@modus-themes-preset-overrides-warmer))

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))
