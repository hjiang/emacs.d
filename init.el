;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration using straight.el and use-package.

;;; Code:

;;; ============================================================================
;;; Package Management Bootstrap
;;; ============================================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Suppress byte-compiler warnings for dynamic loading
(declare-function straight-use-package "straight")
(declare-function ansi-color-apply-on-region "ansi-color")

;; Configure use-package with straight.el
(straight-use-package 'use-package)
(require 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;;; ============================================================================
;;; Custom Functions and Local Configuration
;;; ============================================================================

;; Load custom functions
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'custom-functions)

;; Load environment variables if available
(let ((env-file "~/.emacs.d/.local/env.el"))
  (when (file-readable-p env-file)
    (load-env-file env-file)))

;; Load secrets (API keys, tokens, etc.)
(let ((secrets-file "~/.emacs.d/.local/secrets.el"))
  (when (file-readable-p secrets-file)
    (load-file secrets-file)))

;;; ============================================================================
;;; Core Emacs Configuration
;;; ============================================================================

(use-package emacs
  :straight (:type built-in)
  :hook
  (before-save . delete-trailing-whitespace)
  (after-save . auto-compile-init-file)
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

;;; ============================================================================
;;; Development Tools - LSP & Language Support
;;; ============================================================================

(use-package lsp-mode
  :straight t
  :hook
  (erlang-mode . lsp))

(use-package lsp-ui
  :straight t)

(use-package eglot
  :straight nil
  :hook (c++-ts-mode . (lambda ()
                         (eglot-ensure)
                         (setq-local electric-indent-chars
                                     (remq ?\n electric-indent-chars))
                         (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :hook (c++-ts-mode . (lambda ()
                         (setq-local flycheck-clang-language-standard "c++20"))))

;; Language-specific modes and tools
(use-package yaml-mode
  :straight t)

(use-package ansible
  :straight t
  :hook (yaml-mode . ansible-mode))

(use-package bazel
  :straight t)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package cmake-mode
  :straight t)

(use-package clojure-mode
  :straight t)

(use-package erlang
  :straight t)

(use-package zig-mode
  :straight t)

(use-package platformio-mode
  :straight t
  :hook (c++-ts-mode . (lambda ()
                         (platformio-conditionally-enable))))

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode"))

(use-package go-mode
  :straight t
  :hook
  (before-save . gofmt-before-save)
  (go-mode . lsp-deferred))

(use-package elixir-ts-mode
  :straight (:type built-in)
  :defer t
  :hook
  (after-save . elixir-format-buffer))

(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode)))

(use-package alchemist
  :straight t)

(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))

;;; ============================================================================
;;; UI & Editing Enhancement
;;; ============================================================================

(use-package which-key
  :straight t
  :init
  (which-key-mode 1))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;;; ============================================================================
;;; Org Mode & Knowledge Management
;;; ============================================================================

(use-package org
  :straight t
  :init
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/agenda"))
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
  :straight t
  :hook
  (org-mode . org-modern-mode))

(use-package visual-fill-column
  :straight t
  :hook
  (org-mode . visual-line-fill-column-mode)
  (markdown-mode . visual-line-fill-column-mode))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package org-preview-html
  :straight t)

;;; ============================================================================
;;; Themes & Visual Appearance
;;; ============================================================================

(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-common-palette-overrides
	      ;; Make the mode line borderless
	      '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
	        ,@modus-themes-preset-overrides-faint))
  (load-theme 'modus-vivendi :no-confirm))

(use-package ligature
  :straight t
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

;;; ============================================================================
;;; Completion & Search
;;; ============================================================================

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package prescient
  :straight t
  :init
  (add-to-list 'completion-styles 'prescient))

(use-package vertico-prescient
  :straight t
  :init
  (vertico-prescient-mode))

(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;; ============================================================================
;;; File Viewers & Readers
;;; ============================================================================

(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;; ============================================================================
;;; AI & Development Assistance
;;; ============================================================================

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
         ("C-<return>" . copilot-accept-completion-by-line)
         ("M-C-<return>" . copilot-accept-completion)
         ("C-g" . copilot-clear-overlay))
  :config
  ;; Check and install copilot language server if not present
  (unless (file-exists-p (expand-file-name "~/.emacs.d/.cache/copilot/bin/copilot-language-server"))
    (copilot-install-server))

  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(ld-script-mode 2))
  (add-to-list 'copilot-indentation-alist '(elixir-mode 2))
  (add-to-list 'copilot-indentation-alist '(go-mode 2))
  (add-to-list 'copilot-indentation-alist '(dockerfile-ts-mode 4)))

;;; ============================================================================
;;; Version Control & Project Management
;;; ============================================================================

(use-package magit
  :straight t)

(use-package projectile
  :straight t
  :hook (prog-mode . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;;; ============================================================================
;;; Terminal & System Integration
;;; ============================================================================

(use-package ansi-color
  :straight t
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package vterm
  :straight t)

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  :config
  (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode))

;; Colorize compilation buffers
(if (>= emacs-major-version 28)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (progn
    (defvar compilation-filter-start)
    (defun colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))
