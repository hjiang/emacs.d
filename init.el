;; -*- mode: elisp; lexical-binding: t; -*-

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

(let ((env-file "~/.emacs.d/.local/env.el"))
  (when (file-readable-p env-file)
    (defun reload-env ()
      "Reload environment variables"
      (interactive)
      (load-env-file env-file))
    (reload-env)))

(defun cleanup-clutter ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (if (display-graphic-p)
      (scroll-bar-mode -1))
  (setq inhibit-startup-message t
        initial-scratch-message nil))

(defun setup-fonts ()
  ;; See https://www.emacswiki.org/emacs/SetFonts#h5o-16
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "FiraCode Nerd Font Mono")
    (set-face-attribute 'default nil :weight 'light)
    (set-face-attribute 'default nil :height 145)))

(defun setup-tree-sitter ()
  (setq treesit-language-source-alist
        '((cpp . "https://github.com/tree-sitter/tree-sitter-cpp")
	        (c . "https://github.com/tree-sitter/tree-sitter-c")
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                         "master" "typescript/src"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (gdscript . ("https://github.com/PrestonKnopp/tree-sitter-gdscript" "master" "src"))))
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

(defun setup-custom-file ()
  (setq custom-file "~/.emacs.d/.local/custom.el")
  (when (file-readable-p custom-file)
    (load custom-file)))

(defun maybe-setup-macos ()
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'super
          mac-command-modifier 'meta)
    (pixel-scroll-precision-mode 1)))

(defun smart-split ()
  "Split the window into 100-column sub-windows."
  (interactive)
  (cl-labels ((smart-split-helper (w)
                                  (if (> (window-width w) 180)
                                      (let ((w2 (split-window w 100 t)))
                                        (smart-split-helper w2)))))
    (smart-split-helper nil)))

(use-package emacs
  :ensure nil
  :straight nil
  :hook
  (before-save . delete-trailing-whitespace)
  :config
  (cleanup-clutter)
  (setq-default line-spacing 0.2
 	              indicate-empty-lines t
	              indent-tabs-mode nil
	              tab-width 2
                require-final-newline t
                file-preserve-symlinks-on-save t
                fill-column 80)
  (setq global-auto-revert-non-file-buffers t
        visible-bell t
        make-backup-files nil)
  (column-number-mode 1)
  (recentf-mode 1)
  (setq history-length 1000)
  (savehist-mode 1)
  (save-place-mode 1)
  ;; Only affect clean buffers
  (global-auto-revert-mode 1)
  (electric-pair-mode)
  (setup-fonts)
  (setup-tree-sitter)
  (fido-mode)
  (setup-custom-file)
  (maybe-setup-macos)
  (add-to-list 'auto-mode-alist '("\\.hujson\\'" . js-json-mode)))

(use-package lsp-mode
  :hook
  (erlang-mode . lsp))

(use-package lsp-ui)

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package org
  :ensure t
  :init
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/agenda"))
  :hook
  (visual-line-mode . org-mode)
  :config
  (setq org-adapt-indentation 'headline-data
        org-hide-leading-stars t)
  ;; (defun my-org-export-filter-cjk-spaces (text backend info)
  ;;   "Remove unwanted spaces between CJK characters."
  ;;   (when (org-export-derived-backend-p backend 'html 'latex 'odt)
  ;;     (replace-regexp-in-string "\\(\\cc\\) \\(\\cc\\)" "\\1\\2" text)))
  ;; (add-to-list 'org-export-filter-paragraph-functions
  ;;              'my-org-export-filter-cjk-spaces)
  )

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode))

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
	      ;; Make the mode line borderless
	      '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
	        ,@modus-themes-preset-overrides-faint))
  (load-theme 'modus-vivendi :no-confirm))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package vertico
  :init
  (vertico-mode))

(use-package prescient
  :init
  (add-to-list 'completion-styles 'prescient))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
         ("C-f" . copilot-accept-completion-by-line)
         ("C-e" . copilot-accept-completion)
         ("C-g" . copilot-clear-overlay))
  :config
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(ld-script-mode 2))
  (add-to-list 'copilot-indentation-alist '(elixir-mode 2))
  (add-to-list 'copilot-indentation-alist '(go-mode 2)))

(use-package magit)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package vterm)

(use-package eglot
  :ensure nil
  :straight nil
  :hook (c++-ts-mode . (lambda ()
                         (eglot-ensure)
                         (setq-local electric-indent-chars
                                     (remq ?\n electric-indent-chars))
                         (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

(use-package flycheck
  :init (global-flycheck-mode)
  :hook (c++-ts-mode . (lambda ()
                         (setq-local flycheck-clang-language-standard "c++20"))))

(use-package direnv
 :config
 (direnv-mode))

(use-package yaml-mode)

(use-package ansible
  :hook (yaml-mode . ansible))

(use-package bazel)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :hook (visual-line-mode . markdown-mode))

(use-package cmake-mode)

(use-package clojure-mode)

(use-package erlang)

(use-package zig-mode)

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package ox-hugo
  :after ox)

(defun read-file-or-nil (filename)
  "Read file FILENAME, returning the contents as a string, or nil if it doesn't exist."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string))
    (file-error nil)))

(use-package wakatime-mode
  :config
  (let ((apikey (string-trim (read-file-or-nil "~/.config/wakatime/apikey"))))
    (when apikey
      (setq wakatime-api-key apikey)
      (global-wakatime-mode))))

(use-package org-preview-html)

(defun elixir-format-buffer ()
  "Format the current buffer using mix format."
  (when (and (or (eq major-mode 'elixir-mode)
                 (eq major-mode 'elixir-ts-mode))
             (executable-find "mix"))
    (let ((file (buffer-file-name)))
      (when file
        (call-process "mix" nil nil nil "format" file)
        (revert-buffer nil t t)))))

(use-package go-mode
  :hook
  (before-save . gofmt-before-save)
  (go-mode . lsp-deferred))

(use-package elixir-ts-mode
  :ensure t
  :hook
  (after-save . elixir-format-buffer))

(use-package alchemist
  :ensure t)

;; Colorize compilation buffers
(if (>= emacs-major-version 28)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (progn
    (defun colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))
