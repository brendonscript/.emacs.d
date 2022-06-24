;;; -*- lexical-binding: t -*-
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

(setq gc-cons-threshold 200000000)

(setq native-comp-async-report-warnings-errors nil)
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



(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update nil)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package general
  :demand t
  :config
  (progn
    (general-evil-setup t)
    (general-auto-unbind-keys)

    (general-create-definer leader-map
      :keymaps 'override
      :states '(insert emacs normal hybrid motion visual operator)
      :global-prefix "C-c"
      :non-normal-prefix "M-SPC"
      :prefix "SPC")

    (general-create-definer local-leader-map
      :keymaps 'override
      :states '(insert emacs normal hybrid motion visual operator)
      :global-prefix "C-c m"
      :non-normal-prefix "M-SPC m"
      :prefix "SPC m")

    ;; Prefixes
    (leader-map
      ""   '(nil :which-key "my lieutenant general prefix")
      "b"  '(:ignore t :wk "buffers")
      "d"  '(:ignore t :wk "debug")
      "e"  '(:ignore t :wk "edit")
      "o"  '(:ignore t :wk "org")
      "f"  '(:ignore t :wk "files")
      "fe" '(:ignore t :wk "emacs")
      "g"  '(:ignore t :wk "git")
      "p"  '(:ignore t :wk "projects")
      "r"  '(:ignore t :wk "refactor")
      "s"  '(:ignore t :wk "search")
      "x"  '(:ignore t :wk "execute")
      "T"  '(:ignore t :wk "toggles"))

    (local-leader-map
      ""	'(nil :which-key "major mode"))

    ;; Sim Keys
    (leader-map
      "," (general-simulate-key "C-c")
      "C" (general-simulate-key "C-x")
      "M" (general-simulate-key "C-c C-x"))

    ;; Maps
    (leader-map
      "h" '(:keymap help-map :wk "help")
      "t" '(:keymap tab-prefix-map :wk "tabs"))

    ;; Base
    (leader-map
      ";"   'execute-extended-command
      ":"   'eval-expression
      "O"   'other-window-prefix
      "X"   '((lambda () (interactive) (switch-to-buffer "*scratch*")) :wk "scratch")
      "bd"  'bury-buffer
      "bp"  'me/alternate-buffer
      "bk"  'kill-this-buffer
      "bK"  'kill-some-buffers
      "bB"  'ibuffer-list-buffers
      "ea"  'align-regexp
      "eA"  'align
      "er"  'query-replace
      "ff"  'find-file
      "fs"  'save-buffer
      "fd"  'dired
      "fS"  'me/save-all-unsaved
      "fee" 'me/open-config
      "fer" 'me/reload-emacs-config
      "feq" 'save-buffers-kill-emacs
      "feQ" 'kill-emacs
      "xp"  'check-parens
      "xe"  'eval-last-sexp
      "xb"  'eval-buffer)

    (leader-map "C-h" '(which-key-C-h-dispatch :wk t))
    (local-leader-map "C-h" '(which-key-C-h-dispatch :wk t))

    (general-def
      "C-v" 'me/scroll-half-page-down
      "M-v" 'me/scroll-half-page-up)))

(use-package emacs
  :demand t
  :straight nil
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

  (defun me/reload-emacs-config ()
    (interactive)
    (load-file (expand-file-name (concat user-emacs-directory "init.el"))))

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

  (defun me/buffer-to-side-window ()
    "Place the current buffer in the side window at the bottom."
    (interactive)
    (let ((buf (current-buffer)))
      (display-buffer-in-side-window
       buf '((window-height . 0.25)
             (side . bottom)
             (slot . -1)
             (window-parameters . (no-delete-other-windows . t))))
      (delete-window)))



  :config
  (progn
    ;; Startup ;;
    (setq inhibit-startup-message t)
    (setq initial-scratch-message nil)

    ;; Bells and Ringers ;;
    (setq visible-bell nil)
    (setq ring-bell-function 'ignore)

    ;; History and persistence ;;
    (require 'recentf)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (setq recentf-max-menu-items 40)
    (setq recentf-max-saved-items 250)
    (setq save-interprogram-paste-before-kill t)
    (setq initial-buffer-choice t)
    ;; (require 'desktop)
    ;; (customize-set-variable 'desktop-save 't)
    ;; (desktop-save-mode 1)
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
  :straight nil
  :config
  (progn
    (setq ediff-diff-options "")
    (setq ediff-custom-diff-options "-u")
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-vertically)))

(use-package smerge-mode
  :straight nil
  :init (setq smerge-command-prefix "")
  :config
  (progn
    (with-eval-after-load 'hydra
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
      (with-eval-after-load 'general
        (leader-map smerge-mode-map
           "gm" 'hydra/smerge/body)))))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-dwim-target t)

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
  :straight nil
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

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t)
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (setq doom-themes-treemacs-theme "doom-atom")
    (with-eval-after-load 'treemacs
      (doom-themes-treemacs-config))
    (with-eval-after-load 'org
      (doom-themes-org-config))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-bar-width 3)
           (doom-modeline-minor-modes nil)
           (doom-modeline-buffer-file-name-style 'truncate-with-project)
           (doom-modeline-minor-modes nil)
           (doom-modeline-modal-icon t))
  ;; This configuration to is fix a bug where certain windows would not display
  ;; their full content due to the overlapping modeline
  :config (advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t))))

(use-package solaire-mode
  :config
  (solaire-global-mode))

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :after evil
  :demand t
  :config
  (progn
    (setq which-key-allow-evil-operators t)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-use-C-h-commands nil)
    (setq which-key-idle-delay 0.5)))

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
  :config
  (progn
    (evil-mode 1)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (with-eval-after-load 'general
      (imap "C-n" nil
        "C-p" nil)

      (nmap "R" 'evil-replace-state
        "q" 'me/evil-record-macro
        "g ?" 'nil
        "gu" 'universal-argument)

      (imap "C-g" 'evil-normal-state
        "C-u" 'universal-argument)

      (vmap "gu" 'universal-argument)

      (mmap "j" 'evil-next-visual-line
        "k" 'evil-previous-visual-line
        "L" 'evil-end-of-line-or-visual-line
        "H" 'evil-first-non-blank-of-visual-line
        "gu" 'universal-argument)

      (setq me/window-map (cons 'keymap evil-window-map))
      (leader-map "w" '(:keymap me/window-map :wk "windows"))

      (evil-ex-define-cmd "q" #'kill-this-buffer)
      (evil-ex-define-cmd "wq" #'me/save-and-kill-this-buffer))))

(use-package evil-collection
    :after evil
    :diminish evil-collection-unimpaired-mode
    :config
    (evil-collection-init)
(with-eval-after-load 'general
    (nmap
      "go" 'evil-collection-unimpaired-insert-newline-below
      "gO" 'evil-collection-unimpaired-insert-newline-above
      "gp" 'evil-collection-unimpaired-paste-below
      "gP" 'evil-collection-unimpaired-paste-above)))

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

(use-package evil-mc
  :after evil
  :config
  (progn
    (evil-define-local-var evil-mc-custom-paused nil
      "Paused functionality when there are multiple cursors active.")

    (defun evil-mc-pause-smartchr-for-mode (mode)
      "Temporarily disables the smartchr keys for MODE."
      (let ((m-mode (if (atom mode) mode (car mode)))
            (s-mode (if (atom mode) mode (cdr mode))))
        (let ((init (intern (concat "smartchr/init-" (symbol-name s-mode))))
              (undo (intern (concat "smartchr/undo-" (symbol-name s-mode)))))
          (when (eq major-mode m-mode)
            (funcall undo)
            (push `(lambda () (,init)) evil-mc-custom-paused)))))

    (defun evil-mc-before-cursors-setup-hook ()
      "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
      (mapc 'evil-mc-pause-smartchr-for-mode
            '(web-mode js2-mode java-mode (enh-ruby-mode . ruby-mode) css-mode))
      (when (boundp 'whitespace-cleanup-disabled)
        (setq whitespace-cleanup-disabled t)
        (push (lambda () (setq whitespace-cleanup-disabled nil)) evil-mc-custom-paused)))

    (defun evil-mc-after-cursors-teardown-hook ()
      "Hook to run after all cursors are deleted."
      (dolist (fn evil-mc-custom-paused) (funcall fn))
      (setq evil-mc-custom-paused nil))

    (add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
    (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)

    (defvar evil-mc-mode-line-prefix "ⓜ"
      "Override of the default mode line string for `evil-mc-mode'.")

    (global-evil-mc-mode 1)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package avy
  :after evil
  :config
  (progn
    (setq avy-timeout-seconds 0.3)

    (general-def :states '(normal visual motion)
      "gf"	'avy-goto-char-timer
      "gF"	'avy-resume)

    (general-def :states '(normal visual motion insert emacs)
      "M-S-f"	'avy-resume
      "M-f"	'avy-goto-char-timer
      "C-q" 'avy-goto-char-timer
      "C-S-q" 'avy-resume)))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :after all-the-icons)

(use-package vertico
  :init (vertico-mode)
  :config
  (progn
    (general-def vertico-map
      "C-d"	'vertico-scroll-up
      "C-u"	'vertico-scroll-down
      "C-v"	'vertico-scroll-up
      "M-v"	'vertico-scroll-down
      "C-j"	'vertico-next
      "C-J"	'vertico-next-group
      "C-k"	'vertico-previous
      "C-K"	'vertico-previous-group
      "M-RET"	'minibuffer-force-complete-and-exit
      "M-TAB"	'minibuffer-complete)

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
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-posframe
  :config
  (setq vertico-posframe-min-width 80)
  (setq vertico-posframe-poshandler 'posframe-poshandler-window-top-center)
  (vertico-posframe-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :general
  (leader-map
    "SPC"	'consult-buffer
    "sa"	'consult-org-agenda
    "so"	'consult-outline
    "sm"	'consult-mark
    "sl"	'consult-line
    "sL"	'consult-line-multi
    "sM"	'consult-global-map
    "sr"	'consult-ripgrep
    "fF"	'consult-recent-file
    "fb"	'consult-bookmark
    "bb"	'consult-buffer
    "bo"	'consult-buffer-other-window
    "bf"	'consult-buffer-other-frame
    "bm"	'consult-mode-command
    "bh"	'consult-history
    "xc"	'consult-complex-command
    "xk"	'consult-kmacro
    "pb"	'consult-project-buffer)

  (local-leader-map :keymaps 'org-mode-map
    "s"	'consult-org-heading)

  (general-def
    "C-x b"	'consult-buffer
    "C-x 4 b"	'consult-buffer-other-window
    "C-x 5 b"	'consult-buffer-other-frame
    "C-x M-:"	'consult-complex-command
    "C-x r b"	'consult-bookmark
    "C-x p b"	'consult-project-buffer
    "M-#"	'consult-register-load
    "M-'"	'consult-register-store
    "C-M-#"	'consult-register
    "M-y"	'consult-yank-pop
    "<help> a"	'consult-apropos
    "M-g e"	'consult-compile-error
    "M-g f"	'consult-flymake
    "M-g g"	'consult-goto-line
    "M-g M-g"	'consult-goto-line
    "M-g o"	'consult-outline
    "M-g m"	'consult-mark
    "M-g k"	'consult-global-mark
    "M-g i"	'consult-imenu
    "M-g I"	'consult-imenu-multi
    "M-s d"	'consult-find
    "M-s D"	'consult-locate
    "M-s g"	'consult-grep
    "M-s G"	'consult-git-grep
    "M-s r"	'consult-ripgrep
    "M-s L"	'consult-line-multi
    "M-s m"	'consult-multi-occur
    "M-s k"	'consult-keep-lines
    "M-s u"	'consult-focus-lines
    "M-s e"	'consult-isearch-history)

  (general-def '(motion emacs visual insert normal)
    "C-f" 'consult-line
    "C-S-f" 'consult-line-multi)

  (general-def :keymaps 'isearch-mode-map
    "M-e"	'consult-isearch-history
    "M-s e"	'consult-isearch-history
    "M-s l"	'consult-line
    "M-s L"	'consult-line-multi)

  (general-def :keymaps 'minibuffer-local-map
    "M-s"	'consult-history
    "M-r"	'consult-history)

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
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
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
    (corfu-doc-delay 0.1)
    (corfu-doc-max-width 70)
    (corfu-doc-max-height 30)
    (corfu-doc-display-within-parent-frame t)
    (corfu-echo-documentation nil))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :general
  (imap :keymaps 'org-mode-map
    "TAB" 'completion-at-point)

  (leader-map
    ("pp" 'completion-at-point) ;; capf
    ("pt" 'complete-tag)        ;; etags
    ("pd" 'cape-dabbrev)        ;; or dabbrev-completion
    ("ph" 'cape-history)
    ("pf" 'cape-file)
    ("pk" 'cape-keyword)
    ("ps" 'cape-symbol)
    ("pa" 'cape-abbrev)
    ("pi" 'cape-ispell)
    ("pl" 'cape-line)
    ("pw" 'cape-dict)
    ("p\\" 'cape-tex)
    ("p_" 'cape-tex)
    ("p^" 'cape-tex)
    ("p&" 'cape-sgml)
    ("pr" 'cape-rfc1345))
  :init
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

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
    (defhydra me/hydra-evil-windows (:hint nil
                                           :pre (winner-mode 1)
                                           :post (redraw-display))
      "
Movement & RESIZE^^^^
^ ^ _k_ ^ ^       _f__d_ file/dired  _o_nly win             ^Move _C-k_
_h_ ^✜^ _l_       _b__B_ buffer/alt  _x_ Delete this win    ^_C-w_ _C-j_
^ ^ _j_ ^ ^       _u_ _r_ undo/redo  _s_plit _v_ertically   ^_C-h_ _C-l_"
      ;; For some reason the evil
      ;; commands behave better than
      ;; the emacs ones
      ("j" evil-window-down)
      ("k" evil-window-up)
      ("l" evil-window-right)
      ("h" evil-window-left)
      ("J" evil-window-increase-height)
      ("K" evil-window-decrease-height)
      ("L" evil-window-increase-width)
      ("H" evil-window-decrease-width)
      ("u" winner-undo)
      ("r" (progn (winner-undo) (setq this-command 'winner-undo)))
      ("d" dired  :color blue)
      ("f" find-file)
      ("b" consult-buffer  :color blue)
      ("B" me/alternate-buffer)
      ("o" delete-other-windows :color blue)
      ("x" delete-window)
      ("s" split-window-horizontally)
      ("v" split-window-vertically)
      ("C-w" evil-window-next :color blue)
      ("C-k" evil-window-move-very-top :color blue)
      ("C-j" evil-window-move-very-bottom :color blue)
      ("C-h" evil-window-move-far-left :color blue)
      ("C-l" evil-window-move-far-right :color blue)
      ("SPC" balance-windows  :color blue))

    (leader-map "W" 'me/hydra-evil-windows/body)))

(use-package origami
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
    (global-origami-mode)
    (add-hook 'org-agenda-mode-hook 'origami-mode)))

(use-package ranger
  :disabled t
  :config
  (setq ranger-show-hidden 'format
        ranger-parent-depth 2
        ranger-preview-file nil
        ranger-dont-show-binary t
        ranger-cleanup-on-disable t
        ranger-cleanup-eagerly t))

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


  :config
  (progn

    ;; Keybinds ;;

    (leader-map
      "c"  'org-capture
      "a" 'org-agenda
      "l"  'org-store-link
      "fo" 'org-save-all-org-buffers
      "oc" 'org-clock-goto)

    ;; Org Mode Keybinds
    (general-unbind org-mode-map
      "C-c ?" nil)

    (local-leader-map org-mode-map
      "t" '(:ignore t "toggles")
      "T" '(:ignore t "tables")
      "TI" 'org-table-field-info
      "tt" 'org-set-tags-command
      "ts" 'me/insert-timestamp
      "1" 'org-insert-structure-template)

    (imap org-mode-map
      "TAB" 'completion-at-point)

    (general-def org-mode-map
      "C-s" 'consult-org-heading
      "C-S-s" 'consult-org-agenda)

    ;; Org Agenda Keybinds
    (local-leader-map org-agenda-mode-map
      "l" 'org-agenda-log-mode)

    ;; Org Src Keybinds
    (general-def org-src-mode-map
      [remap save-buffer] 'org-edit-src-exit)

    (local-leader-map org-src-mode-map
      "s" 'org-edit-src-exit)

    ;; Hooks ;;
    (org-clock-persistence-insinuate)
    (add-hook 'org-mode-hook 'me/org-mode-initial-setup)
    (add-hook 'org-src-mode-hook 'evil-normalize-keymaps)


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
    (setq org-clock-out-when-done t)
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    (setq org-clock-report-include-clocking-task t)
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

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
    (setq org-agenda-files '("~/Org/inbox.org" "~/Org/todo.org" "~/Org/projects/"))
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
    (setq org-deadline-warning-days 1)
    ;; (setq org-agenda-skip-timestamp-if-done t)
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
             "* TODO %?\nSCHEDULED: %^t\n" :prepend t)
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

(use-package org-super-agenda
  :after (evil evil-collection evil-org org)
  :config
  (progn
    (org-super-agenda-mode 1)
    (general-unbind org-super-agenda-header-map
      "z"
      "j"
      "k"
      "g"
      "SPC")

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
  :config
  (progn
    (leader-map
      "os" 'org-ql-search
      "ov" 'org-ql-view
      "To" 'org-ql-view-sidebar
      "oS" 'org-ql-sparse-tree
      "or" 'org-ql-view-recent-items
      "of" 'org-ql-find
      "op" 'org-ql-find-path
      "oh" 'org-ql-find-heading)

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

(use-package org-modern
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
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . nil)
                                       (no-delete-other-windows . t)))))
  :config
  (progn
    (require 'org-roam-dailies)
    (leader-map
      "Tr" 'org-roam-buffer-toggle
      "rf" 'org-roam-node-find
      "rg" 'org-roam-graph
      "ri" 'org-roam-node-insert
      "rc" 'org-roam-capture
      "C" 'org-roam-capture
      "rj" 'org-roam-dailies-capture-today
      "j" 'org-roam-dailies-capture-today
      "rd" '(:keymap org-roam-dailies-map :wk "dailies"))

    (general-def org-roam-dailies-map
      "Y" 'org-roam-dailies-capture-yesterday
      "T" 'org-roam-dailies-capture-tomorrow)


    (setq org-roam-dailies-directory (concat me/org-dir "journals"))
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
          '(("d" "fleeting" plain
             "\n%?"
             :target (file+head "fleeting/%<%Y%m%d-%H%M%S>--${slug}.org"
                                "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n")
             :unnarrowed t)
            ("r" "reference" plain
             "\n%?"
             :target (file+head "reference/%<%Y%m%d-%H%M%S>--${slug}.org"
                                "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n")
             :unnarrowed t)
            ("p" "project" plain "* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
             :if-new (file+head "projects/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :project:\n#+CATEGORY: inbox\n#+PROPERTY: agenda-group inbox\n")
             :unnarrowed t)
            ("t" "topic" plain "\n%?"
             :if-new (file+head "topics/${slug}.org"
                                "#+TITLE: ${title}\n#+FILETAGS:\n")
             :unnarrowed t)))



    (org-roam-db-autosync-mode)))

(use-package org-pomodoro
  :after org
  :config
  (progn
    (leader-map
      "oi" 'org-pomodoro
      "oI" 'org-clock-in
      "oo" 'org-clock-out)

    (setq org-pomodoro-manual-break t)
    (setq org-pomodoro-length 30)
    (setq org-pomodoro-short-break-length 10)
    (setq org-pomodoro-long-break-length 45)))

(use-package visual-fill-column
  :preface
  (defun me/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  :hook (org-mode . me/org-mode-visual-fill))

(use-package magit
  :general
  (leader-map
    "gs" 'magit-status
    "gS" 'magit-status-here)
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
  :general (leader-map "=" 'format-all-buffer))

(use-package yasnippet)

(use-package treemacs)

(use-package xref
  :straight nil
  :config
  (with-eval-after-load 'evil
    (mmap xref--xref-buffer-mode-map
      "<backtab" #'xref-prev-group
      "<return" #'xref-goto-xref
      "<tab>" #'xref-next-group)))

(use-package lsp-mode
  :hook
  ((c-mode          ; clangd
    c++-mode        ; clangd
    c-or-c++-mode   ; clangd
    java-mode       ; eclipse-jdtls
    js-mode         ; ts-ls (tsserver wrapper)
    js-jsx-mode     ; ts-ls (tsserver wrapper)
    typescript-mode ; ts-ls (tsserver wrapper)
    python-mode     ; pyright
    web-mode        ; ts-ls/HTML/CSS
    haskell-mode    ; haskell-language-server
    csharp-mode
    csharp-tree-sitter-mode
    ) . lsp-deferred)
  (lsp-completion-mode . me/lsp-mode-setup-completion)
  :commands lsp
  :custom (lsp-completion-provider :none)
  :init
  (defun me/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun me/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'me/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :config
  (setq lsp-auto-guess-root t)
  ;;(setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  ;;(setq lsp-enable-symbol-highlighting nil)
  ;;(setq lsp-enable-on-type-formatting nil)
  ;;(setq lsp-signature-auto-activate nil)
  ;;(setq lsp-signature-render-documentation nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-snippet t)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package flymake
  :straight nil
  :custom
  (flymake-fringe-indicator-position nil))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

;; (use-package dap-mode
;;   :disabled t
;;   :config
;;   (progn
;;     (dap-auto-configure-mode)
;;     (require 'dap-node)
;;     (dap-node-setup)
;;     (require 'dap-netcore)))



(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(use-package yaml-mode)

(use-package json-mode)

(use-package vterm
  :commands vterm
  :general (leader-map "`" 'vterm)
  :config
  (progn
    (setq vterm-buffer-name-string "vterm %s")
    (setq vterm-shell "fish")
    (setq vterm-max-scrollback 10000)))

(use-package fish-mode)

(use-package dashboard
  :init (add-hook 'desktop-no-desktop-file-hook (lambda () (setq initial-buffer-choice (get-buffer "*dashboard*"))))
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
