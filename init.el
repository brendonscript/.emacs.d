;;; -*- lexical-binding: t -*-
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

(setq gc-cons-threshold 200000000)

(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-minimum-level :error)

(defun me/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'me/display-startup-time)

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode 10))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defconst IS-MAC     (eq system-type 'darwin)
  "If the host is running MacOS return true")
(defconst IS-LINUX   (eq system-type 'gnu/linux)
  "If the host is running Linux return true")
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos))
  "If the host is running Windows return true")
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix))
  "If the host is running BSD return true")

;; Initialize package sources
(require 'package)

(setq package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update nil)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :demand t
  :config

  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package emacs
  :ensure nil
  :preface
  ;; Fonts and Text ;;
  (defvar me/default-font-size 160)
  (defvar me/default-variable-font-size 160)
  (defun me/reset-text-size ()
    (interactive)
    (text-scale-set 0))

  ;; Transparency ;;
  (defvar me/frame-transparency '(95 . 95))

  ;; Buffers ;;
  (defun me/alternate-buffer ()
    "Go to previous buffer"
    (interactive)
    (switch-to-buffer (other-buffer)))

  (defun me/set-default-line-length-to (line-length)
    "Set the default line length to LINE-LENGTH."
    (setq-default fill-column line-length))

  ;; Saving ;;
  (defun me/save-all-unsaved ()
    "Save all unsaved files. no ask."
    (interactive)
    (save-some-buffers t))

  ;; Utility ;;
  (defun me/create-dir-if-not-exists ()
    "Offer to create directory when it doesn't exist"
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
          (make-directory dir t)))))

  (defun me/open-config ()
    "Open configuration file"
    (interactive)
    (find-file (expand-file-name (concat user-emacs-directory "README.org"))))

  ;; Scrolling
  (defun me/scroll-half-page (direction)
    "Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
    (let ((opos (cdr (nth 6 (posn-at-point)))))
      ;; opos = original position line relative to window
      (move-to-window-line nil)  ;; Move cursor to middle line
      (if direction
          (recenter-top-bottom -1)  ;; Current line becomes last
        (recenter-top-bottom 0))  ;; Current line becomes first
      (move-to-window-line opos)))  ;; Restore cursor/point position

  (defun me/scroll-half-page-down ()
    "Scrolls exactly half page down keeping cursor/point position."
    (interactive)
    (me/scroll-half-page nil))

  (defun me/scroll-half-page-up ()
    "Scrolls exactly half page up keeping cursor/point position."
    (interactive)
    (me/scroll-half-page t))

  :bind (("<escape>" . keyboard-escape-quit)
         ("C-c e e" . me/open-config)
         ("C-c e q" . save-buffers-kill-emacs)
         ("C-v" . me/scroll-half-page-down)
         ("M-v" . me/scroll-half-page-up)
         ("M-o" . other-window-prefix))
  :config
  (progn
    ;; Startup ;;
    (setq inhibit-startup-message t)
    (setq initial-scratch-message nil)

    ;; Bells and Ringers ;;
    (setq visible-bell nil)
    (setq ring-bell-function #'ignore)

    ;; History and persistence ;;
    (setq recentf-max-menu-items 40)
    (setq recentf-max-saved-items 250)
    (setq save-interprogram-paste-before-kill t)
    (customize-set-variable 'desktop-save 't)
    (desktop-save-mode 1)
    (recentf-mode 1)
    (with-eval-after-load 'no-littering
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      (add-to-list 'recentf-exclude no-littering-var-directory))
    (save-place-mode 1)
    (winner-mode 1)
    (global-auto-revert-mode t)

    ;; Completion ;;
    (setq read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t
          completion-ignore-case t
          completion-cycle-threshold 3
          tab-always-indent 'complete)

    ;; Use `consult-completion-in-region' if Vertico is enabled.
    ;; Otherwise use the default `completion--in-region' function.
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    (setq read-extended-command-predicate
          #'command-completion-default-include-p)

    ;; Minibuffer ;;
    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    ;; File Encoding ;;
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (set-file-name-coding-system 'utf-8)
    (set-clipboard-coding-system 'utf-8)
    (set-buffer-file-coding-system 'utf-8)

    ;; Fonts ;;
    (cond (IS-MAC (setq me/default-font-size 180) (setq me/default-variable-font-size 180))
          (IS-WINDOWS (setq me/default-font-size 90) (setq me/default-variable-font-size 90)))
    (set-face-attribute 'default nil :font "Fira Code Retina" :height me/default-font-size)
    (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height me/default-font-size)
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height me/default-variable-font-size :weight 'regular)
    (global-font-lock-mode t)

    ;; Transparency ;;
    (set-frame-parameter (selected-frame) 'alpha me/frame-transparency)
    (add-to-list 'default-frame-alist `(alpha . ,me/frame-transparency))

    ;; Loading ;;
    (setq load-prefer-newer t)

    ;; Saving ;;
    ;; Auto save
    (setq after-focus-change-function 'me/save-all-unsaved)
    (add-hook 'focus-out-hook 'me/save-all-unsaved)

    ;; Remove whitespace on save
    (add-hook 'before-save-hook 'delete-trailing-whitespace)

    ;; Make file executable if it's a script on save
    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p)

    ;; Create directory if it doesn't exist
    (add-hook 'before-save-hook 'me/create-dir-if-not-exists)

    ;; Version Control ;;
    (setq vc-follow-symlinks t)

    ;; Directories ;;
    (setq default-directory "~/")

    ;; Prog Mode ;;
    (add-hook 'prog-mode-hook 'subword-mode)

    ;; Formatting ;;
    (setq sentence-end-double-space nil)
    (me/set-default-line-length-to 80)

    ;; Behavior ;;
    (setq require-final-newline t)
    (setq show-paren-delay 0.0)

    ;; Confirmations
    (setq confirm-kill-emacs 'y-or-n-p)
    (fset 'yes-or-no-p 'y-or-n-p)

    ;; Modes
    (transient-mark-mode 1)
    (delete-selection-mode 1)
    (show-paren-mode 1)
    (column-number-mode 0)

    ;; Line Numbers

    ;; Disable line numbers for some modes
    (dolist (mode '(org-mode-hook
                    term-mode-hook
                    shell-mode-hook
                    treemacs-mode-hook
                    eshell-mode-hook
                    org-agenda-mode-hook
                    vterm-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))
    (global-display-line-numbers-mode 1)

    ;; Frames
    (setq ns-pop-up-frames nil)

    ;; Windows
    ;; Attempt to always use the same window size and stop resizing stuff weirdly
    (customize-set-variable 'display-buffer-base-action
                            '((display-buffer-reuse-window display-buffer-same-window)
                              (reusable-frames . t)))
    (customize-set-variable 'even-window-sizes nil)
    (setq resize-mini-windows t)

    ;; Mouse
    (setq mouse-yank-at-point t)

    ;; Apropos
    (setq apropos-do-all t)

    ;; Tab bar ;;
    (tab-bar-mode t)
    (customize-set-variable 'tab-bar-new-tab-choice '"*scratch*")
    (customize-set-variable 'tab-bar-show 't)

    ;; Mac OS ;;
    (when IS-MAC
      (setq mac-command-modifier 'control
            mac-option-modifier 'meta
            mac-control-modifier 'super
            mac-right-command-modifier 'control
            mac-right-option-modifier 'meta
            ns-function-modifier 'hyper))))

(use-package ediff
  :ensure nil
  :config
  (progn
    (setq ediff-diff-options "")
    (setq ediff-custom-diff-options "-u")
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-vertically)))

(use-package smerge-mode
  :ensure nil
  :init (setq smerge-command-prefix "")
  :config
  (progn
    (defhydra hydra/smerge
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue))

    (bind-key "C-c g d" 'hydra/smerge/body)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-dwim-target t)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer))
  ;; MacOS ;;
  (when IS-MAC
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  ;;:hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode)))

(use-package erc
  :ensure nil
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "geoffery"
        erc-user-full-name "Geoffery"
        erc-autojoin-timing 'ident
        erc-track-shorten-start 8
        erc-autojoin-channels-alist '(("libera.chat" "#org-mode" "#evil-mode" "#emacs-beginners" "#emacs-til" "#emacs" "#linux" "#fedora" "#archlinux" "##rust" "##programming"))
        erc-kill-buffer-on-part t
        erc-auto-query 'bury
        ;; Stop displaying channels in the mode line for no good reason.
        erc-track-exclude-type '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")
        erc-hide-list '("JOIN" "PART" "QUIT" "KICK" "NICK" "MODE" "333" "353")))

(use-package exec-path-from-shell
  :if IS-MAC
  :config
  (exec-path-from-shell-initialize))

(use-package dash
  :commands (global-dash-fontify-mode)
  :init (global-dash-fontify-mode)
  :config (dash-register-info-lookup))

(use-package s)

(use-package posframe
  :demand t)

(use-package persistent-scratch
  :disabled t
  :after (no-littering org)
  :custom ((persistent-scratch-autosave-interval 180))
  :config
  (add-hook 'after-init-hook 'persistent-scratch-setup-default))

(use-package meow
  :disabled t
  :after helpful
  :init (progn
          (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
          (meow-motion-overwrite-define-key
           '("j" . meow-next)
           '("k" . meow-prev)
           '("<escape>" . ignore))

          (meow-leader-define-key
           ;; SPC j/k will run the original command in MOTION state.
           '("j" . "H-j")
           '("k" . "H-k")
           ;; Use SPC (0-9) for digit arguments.
           '("1" . meow-digit-argument)
           '("2" . meow-digit-argument)
           '("3" . meow-digit-argument)
           '("4" . meow-digit-argument)
           '("5" . meow-digit-argument)
           '("6" . meow-digit-argument)
           '("7" . meow-digit-argument)
           '("8" . meow-digit-argument)
           '("9" . meow-digit-argument)
           '("0" . meow-digit-argument)
           '("b" . consult-buffer)
           '("/" . meow-keypad-describe-key)
           '("?" . meow-cheatsheet))
          (meow-normal-define-key
           '("0" . meow-expand-0)
           '("9" . meow-expand-9)
           '("8" . meow-expand-8)
           '("7" . meow-expand-7)
           '("6" . meow-expand-6)
           '("5" . meow-expand-5)
           '("4" . meow-expand-4)
           '("3" . meow-expand-3)
           '("2" . meow-expand-2)
           '("1" . meow-expand-1)
           '("-" . negative-argument)
           '(";" . meow-reverse)
           '("," . meow-inner-of-thing)
           '("." . meow-bounds-of-thing)
           '("[" . meow-beginning-of-thing)
           '("]" . meow-end-of-thing)
           '("a" . meow-append)
           '("A" . meow-open-below)
           '("b" . meow-back-word)
           '("B" . meow-back-symbol)
           '("c" . meow-change)
           '("d" . meow-delete)
           '("D" . meow-backward-delete)
           '("e" . meow-next-word)
           '("E" . meow-next-symbol)
           '("f" . meow-find)
           '("g" . meow-cancel-selection)
           '("G" . meow-grab)
           '("h" . meow-left)
           '("H" . meow-left-expand)
           '("i" . meow-insert)
           '("I" . meow-open-above)
           '("j" . meow-next)
           '("J" . meow-next-expand)
           '("k" . meow-prev)
           '("K" . meow-prev-expand)
           '("l" . meow-right)
           '("L" . meow-right-expand)
           '("m" . meow-join)
           '("n" . meow-search)
           '("o" . meow-block)
           '("O" . meow-to-block)
           '("p" . meow-yank)
           '("q" . meow-quit)
           '("Q" . meow-goto-line)
           '("r" . meow-replace)
           '("R" . meow-swap-grab)
           '("s" . meow-kill)
           '("t" . meow-till)
           '("u" . meow-undo)
           '("U" . undo-tree-redo)
           '("v" . meow-visit)
           '("w" . meow-mark-word)
           '("W" . meow-mark-symbol)
           '("x" . meow-line)
           '("X" . meow-goto-line)
           '("y" . meow-save)
           '("Y" . meow-sync-grab)
           '("z" . meow-pop-selection)
           '("'" . repeat)
           '("<escape>" . ignore)
           '("C-d" . me/scroll-half-page-down)
           '("C-b" . me/scroll-half-page-up))

          (setq meow-mode-state-list '((authinfo-mode . normal)
                                       (beancount-mode . normal)
                                       (bibtex-mode . normal)
                                       (cider-repl-mode . normal)
                                       (cider-test-report-mode . normal)
                                       (cider-browse-spec-view-mode . motion)
                                       (cargo-process-mode . normal)
                                       (conf-mode . normal)
                                       (deadgrep-edit-mode . normal)
                                       (deft-mode . normal)
                                       (diff-mode . normal)
                                       (ediff-mode . motion)
                                       (gud-mode . normal)
                                       (haskell-interactive-mode . normal)
                                       (help-mode . normal)
                                       (helpful-mode . normal)
                                       (json-mode . normal)
                                       (jupyter-repl-mode . normal)
                                       (mix-mode . normal)
                                       (occur-edit-mode . normal)
                                       (pass-view-mode . normal)
                                       (prog-mode . normal)
                                       (py-shell-mode . normal)
                                       (restclient-mode . normal)
                                       (telega-chat-mode . normal)
                                       (term-mode . normal)
                                       (text-mode . normal)
                                       (vterm-mode . normal)
                                       (Custom-mode . normal))))

  :config (progn
            (me/meow-keybinds)
            (meow-global-mode 1)
            (global-set-key (kbd "C-h k") 'helpful-key)))

(use-package key-chord
  :disabled t
  :config
  (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
  (key-chord-mode 1))

(use-package evil
  :demand t
  :preface
  (defun me/evil-record-macro ()
    (interactive)
    (if buffer-read-only
        (quit-window)
      (call-interactively 'evil-record-macro)))

  (defun me/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :init
  (progn
    (setq evil-want-integration t
          evil-want-keybinding nil
          evil-want-C-u-scroll t
          evil-want-C-i-jump t
          evil-respect-visual-line-mode t
          evil-undo-system 'undo-tree))

  :bind
  (:map evil-insert-state-map
        ("C-g" . evil-normal-state)
        ("C-u" . universal-argument)
        :map evil-normal-state-map
        ("q" . me/evil-record-macro)
        ("g ?" . nil)
        ("gu" . universal-argument)
        :map evil-visual-state-map
        ("gu" . universal-argument)
        :map evil-motion-state-map
        ("j" . evil-next-visual-line)
        ("k" . evil-previous-visual-line)
        ("L" . evil-end-of-line-or-visual-line)
        ("H" . evil-first-non-blank-of-visual-line)
        ("gu" . universal-argument))

  :config
  (progn
    (evil-mode 1)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (evil-ex-define-cmd "q" #'kill-this-buffer)
    (evil-ex-define-cmd "wq" #'me/save-and-kill-this-buffer)



    ))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :config
  (progn
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "jk")
    (setq evil-escape-delay 0.15)

    (add-hook 'evil-escape-inhibit-functions
              (defun +evil-inhibit-escape-in-minibuffer-fn ()
                (and (minibufferp)
                     (or (not (bound-and-true-p evil-collection-setup-minibuffer))
                         (evil-normal-state-p)))))))

(use-package evil-org
  :after org
  :config
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda () (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys nil t))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-goggles
  :after evil
  :config
  (progn
    (setq evil-goggles-pulse t)
    (setq evil-goggles-duration 0.4)
    (setq evil-goggles-blocking-duration 0.1)
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-owl
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))

(use-package general
  :demand t
  :after evil
  :config
  ;; this will bind the prefixes to `my-prefix-map'
  (general-define-key
   :states '(emacs insert normal)
   :prefix-map 'me/leader-prefix-map
   :global-prefix "C-c"
   :non-normal-prefix "M-SPC"
   :prefix "SPC")

  (general-create-definer leader-map
    :keymaps 'me/leader-prefix-map))

                                        ;(leader-map "A" 'org-agenda))

(use-package god-mode
  :disabled t
  :defer t)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package avy
  :after evil
  :demand t
  :bind (("C-S-f" . avy-resume)
         ("C-f" . avy-goto-char-timer)
         :map evil-motion-state-map
         ("C-S-f" . avy-resume)
         ("C-f" . avy-goto-char-timer))
  :config
  (progn
    (setq avy-timeout-seconds 0.5)
    (evil-define-key '(normal visual motion) 'global (kbd "s") 'avy-goto-char-timer)
    (evil-define-key '(normal visual motion) 'global (kbd "S") 'avy-resume)))

(use-package doom-themes
  :demand t
  :init
  (load-theme 'doom-vibrant t)
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (setq doom-themes-treemacs-theme "doom-atom")
    (doom-themes-treemacs-config)
    (doom-themes-org-config)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)
           (doom-modeline-bar-width 4)
           (doom-modeline-bar-width 4)
           (doom-modeline-minor-modes t)
           (doom-modeline-buffer-file-name-style 'truncate-except-project)
           (doom-modeline-minor-modes nil)
           (doom-modeline-modal-icon t))
  ;; This configuration to is fix a bug where certain windows would not display
  ;; their full content due to the overlapping modeline
  :config (advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t))))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :after all-the-icons)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (progn
    (setq which-key-allow-evil-operators t)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-use-C-h-commands nil)
    (setq which-key-idle-delay 0.5)))

(use-package vertico
  :init (vertico-mode)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-J" . vertico-next-group)
        ("C-k" . vertico-previous)
        ("C-K" . vertico-previous-group)
        ("M-RET" . minibuffer-force-complete-and-exit)
        ("M-TAB" . minibuffer-complete))
  :config
  (progn
    (advice-add #'vertico--format-candidate :around
                (lambda (orig cand prefix suffix index _start)
                  (setq cand (funcall orig cand prefix suffix index _start))
                  (concat
                   (if (= vertico--index index)
                       (propertize "» " 'face 'vertico-current)
                     "  ")
                   cand)))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :bind
  (("C-c s a" . consult-org-agenda)
   ("C-c s o" . consult-outline)
   ("C-c s s" . consult-org-heading)
   ("C-c r" . consult-recent-file)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("C-s" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history)
   :map evil-motion-state-map
   ("gb" . consult-buffer))
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (progn
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     :preview-key (kbd "M-."))

    (defvar-local consult-toggle-preview-orig nil)

    (defun consult-toggle-preview ()
      "Command to enable/disable preview."
      (interactive)
      (if consult-toggle-preview-orig
          (setq consult--preview-function consult-toggle-preview-orig
                consult-toggle-preview-orig nil)
        (setq consult-toggle-preview-orig consult--preview-function
              consult--preview-function #'ignore)))
    (bind-key "M-P" #'consult-toggle-preview 'vertico-map)
    (setq consult-narrow-key "<")))

(use-package consult-project-extra
  :bind
  ("C-c p f" . consult-project-extra-find)
  ("C-c p o" . consult-project-extra-find-other-window))

(use-package orderless
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
          ;;; Enable partial-completion for files.
          ;;; Either give orderless precedence or partial-completion.
          ;;; Note that completion-category-overrides is not really an override,
          ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        (consult-multi (styles orderless+initialism))
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :init
  (marginalia-mode)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun +embark-live-vertico ()
    "Shrink Vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))

  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package corfu-doc
    ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
    :after corfu
    :hook (corfu-mode . corfu-doc-mode)
    :bind (:map corfu-map
                ([remap corfu-show-documentation] . corfu-doc-toggle)
                ("M-n" . corfu-doc-scroll-up)
                ("M-p" . corfu-doc-scroll-down))
    :custom
    (corfu-doc-delay 0.75)
    (corfu-doc-max-width 70)
    (corfu-doc-max-height 20)
    (corfu-doc-display-within-parent-frame t)
    (corfu-echo-documentation nil))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package helpful
  :after evil
  :demand t
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-at-point)
  :bind
  ("H-d" . helpful-at-point)
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  (:map evil-motion-state-map
        ("K" . helpful-at-point)))

(use-package hydra
  :config
  (progn

    (defhydra me/hydra-text-scale (:timeout 4)
      "scale text"
      ("j" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("f" nil "finished" :exit t))

    (defhydra me/hydra-buffers (:color blue :hint nil)
      "
                                                                         ╭─────────┐
       Move to Window         Switch                  Do                 │ Buffers │
    ╭────────────────────────────────────────────────────────────────────┴─────────╯
             ^_k_^          [_b_] switch             [_d_] kill the buffer
             ^^↑^^          [_i_] ibuffer            [_r_] toggle read-only mode
         _h_ ←   → _l_      [_a_] alternate          [_u_] revert buffer changes
             ^^↓^^          [_o_] other              [_w_] save buffer
             ^_j_^
    --------------------------------------------------------------------------------
                "
      ("<tab>" hydra-master/body "back")
      ("<ESC>" nil "quit")
      ("a" me/alternate-buffer)
      ("b" consult-buffer)
      ("d" kill-current-buffer)
      ("i" ibuffer)
      ("o" other-window)
      ("h" windmove-left  :color red)
      ("k" windmove-up    :color red)
      ("j" windmove-down  :color red)
      ("l" windmove-right :color red)
      ("r" read-only-mode)
      ("u" revert-buffer)
      ("w" save-buffer))

    (defhydra me/hydra-windows (:color blue :hint nil)
      "
                                                                         ╭─────────┐
       Move to      Size    Scroll        Split                    Do    │ Windows │
    ╭────────────────────────────────────────────────────────────────────┴─────────╯
          ^_k_^           ^_K_^       ^_p_^    ╭─┬─┐^ ^        ╭─┬─┐^ ^         ↺ [_u_] undo layout
          ^^↑^^           ^^↑^^       ^^↑^^    │ │ │_v_ertical ├─┼─┤_b_alance   ↻ [_r_] restore layout
      _h_ ←   → _l_   _H_ ←   → _L_   ^^ ^^    ╰─┴─╯^ ^        ╰─┴─╯^ ^         ✗ [_d_] close window
          ^^↓^^           ^^↓^^       ^^↓^^    ╭───┐^ ^        ╭───┐^ ^         ⇋ [_w_] cycle window
          ^_j_^           ^_J_^       ^_n_^    ├───┤_s_tack    │   │_z_oom
          ^^ ^^           ^^ ^^       ^^ ^^    ╰───╯^ ^        ╰───╯^ ^
    --------------------------------------------------------------------------------
              "
      ("<tab>" hydra-master/body "back")
      ("<ESC>" nil "quit")
      ("n" scroll-other-window :color red)
      ("p" scroll-other-window-down :color red)
      ("b" balance-windows)
      ("d" delete-window)
      ("H" shrink-window-horizontally :color red)
      ("h" windmove-left :color red)
      ("J" shrink-window :color red)
      ("j" windmove-down :color red)
      ("K" enlarge-window :color red)
      ("k" windmove-up :color red)
      ("L" enlarge-window-horizontally :color red)
      ("l" windmove-right :color red)
      ("r" winner-redo :color red)
      ("s" split-window-vertically :color red)
      ("u" winner-undo :color red)
      ("v" split-window-horizontally :color red)
      ("w" other-window)
      ("z" delete-other-windows))

    (global-set-key (kbd "C-c W") 'me/hydra-windows/body)
    (global-set-key (kbd "C-c b") 'me/hydra-buffers/body)
    (global-set-key (kbd "C-c T f") 'me/hydra-text-scale/body)))

(use-package origami
  :demand t
  :config
  (progn
    (with-eval-after-load 'org-super-agenda
      (bind-key "<tab>" 'origami-toggle-node 'org-super-agenda-header-map))
    (defvar me/org-super-agenda-auto-hide-groups
      '("Done Today" "Clocked Today"))

    (defun me/org-super-agenda-origami-fold-default ()
      "Fold certain groups by default in Org Super Agenda buffer.
       To enable:
       `(add-hook 'org-agenda-finalize 'me/org-super-agenda-origami-fold-default)'"
      (forward-line 3)
      (--each me/org-super-agenda-auto-hide-groups
        (goto-char (point-min))
        (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
          (origami-close-node (current-buffer) (point)))))
    (add-hook 'org-agenda-finalize-hook 'me/org-super-agenda-origami-fold-default)
    (add-hook 'org-agenda-mode-hook 'origami-mode)))

(use-package org
  :demand t
  :preface
  ;; Functions ;;
  (defun me/org-mode-initial-setup ()
    (setq org-indent-mode-turns-on-hiding-stars t)
    (setq org-tags-column 0)
    (setq org-indent-indentation-per-level 2)
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (defun me/insert-timestamp ()
    (interactive)
    (let ((current-prefix-arg '(16))) (call-interactively 'org-time-stamp-inactive))) ; Universal Argument x2 - 4*4

  (defun me/org-agenda-place-point ()
    (goto-char (point-min)))

  (defun me/org-babel-tangle-config ()
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))


  ;; Directories ;;
  (defconst me/org-dir "~/Org/")

  ;; Files ;;
  (defconst me/org-todo-file (concat me/org-dir "todo.org"))
  (defconst me/org-inbox-file (concat me/org-dir "inbox.org"))
  (defconst me/org-archive-file (concat me/org-dir "archive.org"))
  (defconst me/org-emacs-config-file (concat user-emacs-directory "README.org"))

  ;; Archive ;;
  (defconst me/org-archive-location (concat me/org-archive-file "::* From %s"))

  ;; All Files ;;
  (defun me/refresh-all-org-files ()
    (load-library "find-lisp")
    (find-lisp-find-files "~/Org" "\.org$"))

  (defvar me/org-all-files (me/refresh-all-org-files))

  :bind
  (("C-c c" . org-capture)
   ("C-c a a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c o s" . org-save-all-org-buffers)
   ("C-c o c" . org-clock-goto)
   ("C-c o t" . me/insert-timestamp)
   :map org-mode-map
   ("C-c ?" . nil)
   ("C-c T ?" . org-table-field-info)
   :map org-agenda-mode-map
   ("C-c o l" . org-agenda-log-mode))

  :config
  (progn

    ;; Hooks ;;
    (add-hook 'org-mode-hook 'me/org-mode-initial-setup)

    ;; Directories ;;
    (setq org-directory me/org-dir)
    (setq org-archive-location me/org-archive-location)


    ;; Visuals ;;
    (setq org-ellipsis " ▼ ")
    (setq org-pretty-entities t)
    (setq org-fontify-todo-headline t)

    ;; Behavior ;;
    (setq org-cycle-emulate-tab 'white)
    (setq org-catch-invisible-edits 'smart)
    (setq org-link-search-must-match-exact-headline nil)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-log-state-notes-into-drawer t)
    (setq org-extend-today-until 4)
    (setq org-duration-format 'h:mm)
    (setq-default org-enforce-todo-dependencies t)


    ;; Source Editing ;;
    (setq org-edit-src-turn-on-auto-save t)
    (setq org-src-window-setup 'current-window)
    (push '("conf-unix" . conf-unix) org-src-lang-modes)

    ;; Time and Clock settings ;;
    (org-clock-persistence-insinuate)
    (setq org-clock-out-when-done t)
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    (setq org-clock-report-include-clocking-task t)

    ;; Use a function to decide what to change the state to.
    (defun me/switch-task-on-clock-start (task-state)
      (if (or (string= task-state "TODO")(string= task-state "NEXT"))
          "PROG"
        task-state))
    (setq org-clock-in-switch-to-state #'me/switch-task-on-clock-start)


    ;; Refile ;;
    (setq org-refile-use-outline-path nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-targets `((,(directory-files-recursively "~/Org/" "^[a-z0-9]*.org$") :maxlevel . 3)))


    ;; Fonts ;;
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.0)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0))))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)


    ;; Org Habits ;;
    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq
     org-habit-today-glyph ?◌
     org-habit-completed-glyph ?●
     org-habit-missed-glyph ?○
     org-habit-preceding-days 7
     org-habit-show-habits-only-for-today t
     org-habit-graph-column 65)


    ;; Org Todos ;;
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d!)" "CANCELLED(c!)")))

    (setq org-todo-keyword-faces
          '(("TODO" . (:foreground "#00ff66" :weight bold))
            ("NEXT" . (:foreground "#66ffff"
                                   :weight bold))
            ("PROG"  . (:foreground "#ff0066"
                                    :weight bold))
            ("DONE" . (:foreground "darkgrey"
                                   :weight bold))
            ("CANCELLED" . (:foreground "darkgrey"
                                        :weight bold))))


    ;; Org Tags ;;
    (setq org-tag-persistent-alist
          '((:startgroup)
            ("@errand" . ?e)
            ("@home" . ?h)
            ("@work" . ?w)
            ("@computer" . ?c)
            (:endgroup)
            ("ARCHIVE" . ?A)
            ("bookmark" . ?b)
            ("emacs" . ?E)
            ("idea" . ?i)
            ("inbox" . ?I)
            ("goal" . ?g)
            ("someday" . ?s)))

    ;; (setq org-tag-faces
    ;;       '(("@errand" . (:foreground "mediumPurple1" :weight bold))
    ;;         ("@home" . (:foreground "royalblue1" :weight bold))
    ;;         ("@work" . (:foreground "#1CC436" :weight bold))))


    ;; Org Agenda ;;

    ;; Agenda Evil Keybinds
    (evil-define-key 'motion org-agenda-mode-map (kbd "sf") 'org-agenda-filter)
    (evil-define-key 'motion org-agenda-mode-map (kbd "zc") 'evil-close-fold)
    (evil-define-key 'motion org-agenda-mode-map (kbd "zo") 'evil-open-fold)
    (evil-define-key 'motion org-agenda-mode-map (kbd "zr") 'evil-open-folds)
    (evil-define-key 'motion org-agenda-mode-map (kbd "zm") 'evil-close-folds)
    (evil-define-key 'motion org-agenda-mode-map (kbd "zO") 'evil-open-fold-rec)
    (evil-define-key 'motion org-agenda-mode-map (kbd "za") 'evil-toggle-fold)

    ;; Agenda Settings ;;

    ;; Open links in current window
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
    (setq org-agenda-files '("~/Org/inbox.org" "~/Org/todo.org"))
    ;;(directory-files-recursively "~/Org/" "^[a-z0-9]*.org$")
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-start-day nil)
    (setq org-agenda-todo-ignore-scheduled 'future)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-compact-blocks t)
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-use-time-grid nil)
    (setq org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
          org-agenda-time-grid '((daily today require-timed)
                                 (800 1000 1200 1400 1600 1800 2000)
                                 "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
          org-agenda-prefix-format '(
                                     (agenda . " %i %-12:c%?-12t% s")
                                     (todo . " %i  ")
                                     (tags . " %i  ")
                                     (search . " %i  ")))

    (setq org-agenda-hide-tags-regexp
          (concat org-agenda-hide-tags-regexp "\\|sometag"))

    (setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                        "\n"
                                                        (org-agenda-format-date-aligned date))))
    (setq org-cycle-separator-lines 0)
    (setq org-agenda-category-icon-alist
          `(("work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
            ("home" ,(list (all-the-icons-material "home")) nil nil :ascent center)
            ("computer" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
            ("errand" ,(list (all-the-icons-material "location_city")) nil nil :ascent center)
            ("groceries" ,(list (all-the-icons-material "shopping_basket")) nil nil :ascent center)
            ("health" ,(list (all-the-icons-material "local_hospital")) nil nil :ascent center)
            ("routine" ,(list (all-the-icons-material "repeat")) nil nil :ascent center)
            ("inbox" ,(list (all-the-icons-material "inbox")) nil nil :ascent center)
            ("calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)))
    (add-hook 'org-agenda-finalize-hook #'me/org-agenda-place-point 90)


    ;; Org Capture ;;
    (defun my-org-capture-place-template-dont-delete-windows (oldfun &rest args)
      (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
        (apply oldfun args)))

    (with-eval-after-load "org-capture"
      (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))

    (setq org-capture-templates
          '(("c" "Current" entry
             (file me/org-inbox-file)
             "* PROG %?\n%t\n" :prepend t :clock-in t :clock-keep t :clock-resume t)
            ("e" "Emacs Task" entry
             (file+headline me/org-todo-file "Emacs")
             "* TODO %?\n%U\n" :prepend t)
            ("t" "Task" entry
             (file me/org-inbox-file)
             "* TODO %?\n%U\n" :prepend t)
            ("T" "Task (Scheduled)" entry
             (file me/org-inbox-file)
             "* TODO %?\nSCHEDULED: %^T\n" :prepend t)
            ;; Work ;;
            ("w" "Work Captures")
            ("wt" "Work Task" entry
             (file me/org-inbox-file)
             "* TODO %? :work:\n%U\n" :prepend t)
            ("wT" "Work Task (Scheduled)" entry
             (file me/org-inbox-file)
             "* TODO %?\nSCHEDULED: %^T\n" :prepend t)))


    ;; Babel ;;
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)))

    ;; Automatically tangle our Emacs.org config file when we save it
    (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'me/org-babel-tangle-config)))))

(use-package org-contrib
  :after org)

(use-package org-pretty-tags
  :disabled t
  :commands (org-pretty-tags-global-mode)
  :init (org-pretty-tags-global-mode t)
  :config
  (progn
    (setq org-pretty-tags-surrogate-strings '(("@errand" "🛒")
                                              ("@home" "🏡")
                                              ("@work" "💼")
                                              ("@emacs" "⌨️")
                                              ("routine" "🔁")
                                              ("inbox" "📥")
                                              ("bookmark" "🔖")
                                              ("idea" "💡")
                                              ("distraction" "❓")
                                              ("ARCHIVE" "🗄️")
                                              ))))

(use-package org-super-agenda
  :after (evil evil-collection evil-org org)
  :demand t
  :bind
  (:map org-super-agenda-header-map
        ("z" . nil)
        ("j" . nil)
        ("k" . nil)
        ("g" . nil))
  :config
  (progn
    (org-super-agenda-mode 1)
    (setq org-super-agenda-groups '((:habit t :order 98)
                                    (:name "In Progress"
                                           :todo "PROG")
                                    (:name "Next to do"
                                           :todo "NEXT")
                                    (:name "Due Today"
                                           :deadline today)
                                    (:name "Due Soon"
                                           :deadline future)
                                    (:name "Overdue"
                                           :deadline past)
                                    (:name "Today"
                                           :scheduled today
                                           :date today)
                                    (:name "Scheduled Earlier"
                                           :scheduled past)
                                    (:name "Future"
                                           :scheduled future)
                                    (:name "Inbox"
                                           :tag "inbox")
                                    (:auto-group t :order 99)))

    (setq org-agenda-custom-commands '(("a" "POG AGENDA"
                                        ((agenda "" ((org-agenda-span 'day)
                                                     (org-agenda-log-mode t)
                                                     (org-super-agenda-groups
                                                      '((:order-multi (99
                                                                       (:name "Clocked Today" :log clock)
                                                                       (:name "Done Today" :todo ("DONE" "CANCELLED") :and (:log t))))
                                                        (:name "Habits" :habit t :order 98)
                                                        (:name "In Progress" :todo "PROG")
                                                        (:name "Next" :todo "NEXT")
                                                        (:name "Overdue" :deadline past)
                                                        (:name "Due Today" :deadline today)
                                                        (:name "Today"
                                                               :scheduled today
                                                               :date today)
                                                        (:name "Scheduled Earlier" :scheduled past)))))
                                         (alltodo "" ((org-agenda-overriding-header "\nAll Tasks")
                                                      (org-super-agenda-groups
                                                       '((:discard (:habit t))
                                                         (:discard (:tag "ARCHIVE"))
                                                         (:name "In Progress"
                                                                :todo "PROG")
                                                         (:name "Next to do"
                                                                :todo "NEXT")
                                                         (:name "Due Soon"
                                                                :deadline future)
                                                         (:name "Future"
                                                                :scheduled future)
                                                         (:name "Inbox"
                                                                :tag "inbox")
                                                         (:name "Emacs"
                                                                :tag "emacs")
                                                         (:auto-group t :order 99)))))))))))

(use-package org-ql
  :after org
  :demand t
  :bind
  (("C-c a s" . org-ql-search)
   ("C-c a v" . org-ql-view)
   ("C-c a t" . org-ql-view-sidebar)
   ("C-c a S" . org-ql-sparse-tree)
   ("C-c a r" . org-ql-view-recent-items)
   ("C-c a f" . org-ql-find)
   ("C-c a p" . org-ql-find-path)
   ("C-c a h" . org-ql-find-heading))
  :config
  (progn
    (setq org-ql-views nil)
    (evil-define-key 'motion org-ql-view-list-map (kbd "RET") 'org-ql-view-switch)
    ;; Add these to a hydra or something
    (defun me/org-ql-bookmarks ()
      (interactive)
      (org-ql-search me/org-all-files '(tags "bookmark")))

    (defun me/org-ql-ideas ()
      (interactive)
      (org-ql-search me/org-all-files '(tags "idea")))

    (defun me/org-ql-emacs ()
      (interactive)
      (org-ql-search me/org-all-files '(tags "emacs")))

    (add-to-list 'org-ql-views '("Inbox" :buffers-files org-agenda-files :query
                                 (and
                                  (not
                                   (done))
                                  (tags "inbox"))
                                 :sort
                                 (date priority)
                                 :super-groups org-super-agenda-groups :title "Inbox Items"))

    (add-to-list 'org-ql-views '("Super View" :buffers-files org-agenda-files :query
                                 (and
                                  (not
                                   (done))
                                  (and
                                   (todo)))
                                 :sort
                                 (date priority)
                                 :super-groups org-super-agenda-groups :title "SUPER VIEW"))

    (add-to-list 'org-ql-views '("NEXT tasks" :buffers-files org-agenda-files :query
                                 (todo "NEXT")
                                 :sort
                                 (date priority)
                                 :super-groups org-super-agenda-groups :title "Overview: NEXT tasks"))

    (add-to-list 'org-ql-views '("Archive" :buffers-files org-agenda-files :query
                                 (and (done))
                                 :sort
                                 (date priority)))

    (add-to-list 'org-ql-views '("Routines" :buffers-files org-agenda-files :query
                                 (and
                                  (not
                                   (done))
                                  (and
                                   (habit)
                                   (scheduled :to today)
                                   (or
                                    (ts-active :to today))))
                                 :sort
                                 (todo priority date)))))

(use-package org-sidebar
  :bind (("C-c a T" . org-sidebar-tree-toggle)))

(use-package org-modern
  :demand t
  :after org
  :init
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-insert-heading-respect-content t
        org-agenda-tags-column 0)
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Fira Code Retina")
  (setq org-modern-checkbox nil)
  (setq org-modern-keyword "‣")

  (setq org-modern-todo-faces
        (quote (("TODO" :inverse-video t :weight semibold :foreground "#00ff66" :inherit (org-modern-label))
                ("NEXT" :inverse-video t :weight semibold :foreground "#66ffff" :inherit (org-modern-label))
                ("PROG" :inverse-video t :weight semibold :foreground "#ff0066" :inherit (org-modern-label))
                ("DONE" :inverse-video t :weight semibold :foreground "darkgrey" :inherit (org-modern-label))
                ("CANCELLED" :inverse-video t :weight semibold :foreground "darkgrey" :inherit (org-modern-label)))))
  (global-org-modern-mode))

(use-package org-wild-notifier
  :after org
  :config
  (org-wild-notifier-mode))

(use-package org-roam
  :after org
  :demand t
  :custom
  (org-roam-directory (file-truename me/org-dir))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :config
  (progn
    (setq org-roam-dailies-directory (concat me/org-dir "journals"))
    (require 'org-roam-dailies)
    (setq org-id-link-to-org-use-id 'create-if-interactive)
    (setq org-roam-completion-everywhere t)
    (setq org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:10}" 'face 'org-tag)))
    (setq org-roam-mode-sections '(org-roam-backlinks-section org-roam-reflinks-section org-roam-unlinked-references-section))


    ;; Capture ;;
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry "* %<%I:%M %p>: %?"
             :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

    (setq org-roam-capture-templates
          '(("d" "Fleeting Note" plain
             "\n%?"
             :target (file+head "fleeting/%<%Y%m%d-%H%M%S>--${slug}.org"
                                "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n")
             :unnarrowed t)
            ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
             :if-new (file+head "projects/%<%Y%m%d%H%M%S>--${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :project:")
             :unnarrowed t)
            ("t" "topic" plain "\n%?"
             :if-new (file+head "topics/${slug}.org"
                                "#+TITLE: ${title}\n#+FILETAGS:\n")
             :unnarrowed t)))

    (org-roam-db-autosync-mode)))

(use-package org-pomodoro
  :after org
  :bind (("C-c o c" . org-pomodoro))
  :config
  (progn
    (setq org-pomodoro-manual-break t)
    (setq org-pomodoro-length 30)
    (setq org-pomodoro-short-break-length 10)
    (setq org-pomodoro-long-break-length 45)))

(use-package org-superstar
  :disabled t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (progn
    (cond (IS-MAC (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 200))
          (IS-WINDOWS (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 90)))

    (setq org-superstar-todo-bullet-alist
          '(("TODO" . ?λ)
            ("NEXT" . ?✰)
            ("PROG" . ?∞)
            ("DONE" . ?✔)
            ("CANCELLED" . ?✘)))

    (setq org-superstar-item-bullet-alist
          '((?* . ?•)
            (?+ . ?➤)
            (?- . ?•)))

    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
    (setq org-superstar-special-todo-items t)
    (setq org-superstar-leading-bullet " ")
    (org-superstar-restart)))

(use-package visual-fill-column
  :preface
  (defun me/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  :hook (org-mode . me/org-mode-visual-fill))

(use-package magit
  :bind (("C-c g s" . magit))
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit
  :init
  (setq forge-add-default-bindings t)
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package format-all
  :hook (prog-mode . format-all-mode)
  :bind(("C-c F" . format-all-buffer)))

(use-package vterm
  :commands vterm
  :bind (("C-c t" . vterm))
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "fish")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package fish-mode)

(use-package diminish
  :demand t)

(use-package dashboard
  :init (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :custom ((dashboard-agenda-sort-strategy '(todo-state-down)))
  :config
  (setq dashboard-week-agenda nil)
  (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-match-agenda-entry "-routine")
  (setq dashboard-image-banner-max-height 190)

  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-faicon "calendar" :height 1.1 :v-adjust 0.0)
            "Agenda"
            "Browse Agenda"
            (lambda (&rest _) (org-agenda)))
           (,(all-the-icons-faicon "list" :height 1.1 :v-adjust 0.0)
            "Views"
            "Browse Views"
            (lambda (&rest _) (org-ql-view-sidebar)))
           ("⚙" nil "Open Config" (lambda (&rest _) (me/open-config)) success))))

  (setq dashboard-projects-backend 'project-el)
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))
