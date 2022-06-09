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

(defconst IS-MAC     (eq system-type 'darwin)
  "If the host is running MacOS return true")
(defconst IS-LINUX   (eq system-type 'gnu/linux)
  "If the host is running Linux return true")
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos))
  "If the host is running Windows return true")
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix))
  "If the host is running BSD return true")

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 50)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(winner-mode 1)

(prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
;  (set-w32-system-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)

(defvar me/default-font-size 160)
(defvar me/default-variable-font-size 160)

(cond (IS-MAC (setq me/default-font-size 180) (setq me/default-variable-font-size 180))
      (IS-WINDOWS (setq me/default-font-size 90) (setq me/default-variable-font-size 90)))

(defun me/set-fonts ()
  (set-face-attribute 'default nil :font "Fira Code Retina" :height me/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height me/default-font-size)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height me/default-variable-font-size :weight 'regular))

(me/set-fonts)

(defvar me/frame-transparency '(95 . 95))

(set-frame-parameter (selected-frame) 'alpha me/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,me/frame-transparency))

(defun me/alternate-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun me/save-all-unsaved ()
  "Save all unsaved files. no ask."
  (interactive)
  (save-some-buffers t))

  (add-hook 'focus-out-hook 'me/save-all-unsaved)
  (setq after-focus-change-function 'me/save-all-unsaved)

(defun me/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun me/reset-text-size ()
  (interactive)
  (text-scale-set 0))

(setq default-directory "~/")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'subword-mode)
(setq vc-follow-symlinks t)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq sentence-end-double-space nil)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))
(defun me/set-default-line-length-to (line-length)
  "Set the default line length to LINE-LENGTH."
  (setq-default fill-column line-length))

(me/set-default-line-length-to 80)


(transient-mark-mode t)
(delete-selection-mode t)
(setq require-final-newline t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq-default dired-listing-switches "-alh")
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)
(global-auto-revert-mode t)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq ns-pop-up-frames nil)
(setq mouse-yank-at-point t)
(global-set-key (kbd "M-;")
                'me/comment-or-uncomment-region-or-line)
(save-place-mode 1)
(show-paren-mode 1)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t)

(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

;; Line and column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                org-agenda-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(tab-bar-mode t)

(customize-set-variable 'tab-bar-new-tab-choice '"*scratch*")
(customize-set-variable 'tab-bar-show 't)

(setq resize-mini-windows t)

(customize-set-variable 'desktop-save 't)
(desktop-save-mode 1)

(defun me/open-config ()
    (interactive)
    (find-file (expand-file-name (concat user-emacs-directory "README.org"))))

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

(setq mac-command-modifier 'control
      mac-option-modifier 'meta
      mac-control-modifier 'super
      mac-right-command-modifier 'control
      mac-right-option-modifier 'meta
      ns-function-modifier 'hyper)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c e e") 'me/open-config)
(global-set-key (kbd "C-c e q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-v") 'me/scroll-half-page-down)
(global-set-key (kbd "M-v") 'me/scroll-half-page-up)

(defun me/evil-keybinds ()
  ;; Rebind Universal Argument
  (define-key evil-motion-state-map (kbd "M-u") 'universal-argument)
  (define-key evil-insert-state-map (kbd "C-u") 'universal-argument)

  ;; Exit insert with Emacs C-g bind
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; L and H To end/beginning of line respectively
  (evil-global-set-key 'motion "L" 'evil-end-of-line-or-visual-line)
  (evil-global-set-key 'motion "H" 'evil-first-non-blank-of-visual-line)

  (evil-global-set-key 'motion "gb" 'consult-buffer)
  (define-key evil-normal-state-map (kbd "q") 'my-evil-record-macro))

<<<<<<< HEAD
(defun me/avy-keybinds ()
  (bind-key "C-:" 'avy-resume)
  (bind-key "C-f" 'avy-goto-char-timer))
=======
(defun me/avy-keybinds ())
>>>>>>> meow

(defun me/vertico-keybinds ()
  (bind-key "C-j" 'vertico-next 'vertico-map)
  (bind-key "C-J" 'vertico-next-group 'vertico-map)
  (bind-key "C-k" 'vertico-previous 'vertico-map)
  (bind-key "C-K" 'vertico-previous-group 'vertico-map)
  (bind-key "M-RET" 'minibuffer-force-complete-and-exit 'vertico-map)
  (bind-key "M-TAB" 'minibuffer-complete 'vertico-map))

(defun me/vertico-directory-keybinds ()
  (bind-key "RET" 'vertico-directory-enter 'vertico-map)
  (bind-key "DEL" 'vertico-directory-delete-char 'vertico-map)
  (bind-key "M-DEL" 'vertico-directory-delete-word 'vertico-map))

(defun me/consult-keybinds ()
  (bind-key "C-c s a" 'consult-org-agenda)
  (bind-key "C-c s o" 'consult-outline)
  (bind-key "C-c s s" 'consult-org-heading)
  (bind-key "C-c r" 'consult-recent-file)
  (bind-key "C-c h" 'consult-history)
  (bind-key "C-c m" 'consult-mode-command)
  (bind-key "C-c k" 'consult-kmacro)
  (bind-key "C-x M-:" 'consult-complex-command)     ;; orig. repeat-complex-command
  (bind-key "C-x b" 'consult-buffer)                ;; orig. switch-to-buffer
  (bind-key "C-x 4 b" 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  (bind-key "C-x 5 b" 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  (bind-key "C-x r b" 'consult-bookmark)            ;; orig. bookmark-jump
  (bind-key "C-x p b" 'consult-project-buffer)      ;; orig. project-switch-to-buffer
  (bind-key "M-#" 'consult-register-load)
  (bind-key "M-'" 'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  (bind-key "C-M-#" 'consult-register)
  (bind-key "M-y" 'consult-yank-pop)                ;; orig. yank-pop
  (bind-key "<help> a" 'consult-apropos)            ;; orig. apropos-command
  (bind-key "M-g e" 'consult-compile-error)
  (bind-key "M-g f" 'consult-flymake)               ;; Alternative: consult-flycheck
  (bind-key "M-g g" 'consult-goto-line)             ;; orig. goto-line
  (bind-key "M-g M-g" 'consult-goto-line)           ;; orig. goto-line
  (bind-key "M-g o" 'consult-outline)               ;; Alternative: consult-org-heading
  (bind-key "M-g m" 'consult-mark)
  (bind-key "M-g k" 'consult-global-mark)
  (bind-key "M-g i" 'consult-imenu)
  (bind-key "M-g I" 'consult-imenu-multi)
  (bind-key "M-s d" 'consult-find)
  (bind-key "M-s D" 'consult-locate)
  (bind-key "M-s g" 'consult-grep)
  (bind-key "M-s G" 'consult-git-grep)
  (bind-key "M-s r" 'consult-ripgrep)
  (bind-key "C-s" 'consult-line)
  (bind-key "M-s L" 'consult-line-multi)
  (bind-key "M-s m" 'consult-multi-occur)
  (bind-key "M-s k" 'consult-keep-lines)
  (bind-key "M-s u" 'consult-focus-lines)

  (bind-key "M-s e" 'consult-isearch-history)
  (bind-key "M-e" 'consult-isearch-history 'isearch-mode-map)         ;; orig. isearch-edit-string
  (bind-key "M-s e" 'consult-isearch-history 'isearch-mode-map)       ;; orig. isearch-edit-string
  (bind-key "M-s l" 'consult-line 'isearch-mode-map)                  ;; needed by consult-line to detect isearch
  (bind-key "M-s L" 'consult-line-multi 'isearch-mode-map)            ;; needed by consult-line to detect isearch

  (bind-key "M-s" 'consult-history 'minibuffer-local-map)                 ;; orig. next-matching-history-element
  (bind-key "M-r" 'consult-history 'minibuffer-local-map))

(defun me/meow-keybinds ()
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
   '("C-b" . me/scroll-half-page-up)))

(defun me/exec-path-from-shell-config ()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun me/evil-init ()
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree))

(defun me/evil-config ()
  (evil-mode 1)

  ;; Initial states
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (defun my-evil-record-macro ()
    (interactive)
    (if buffer-read-only
        (quit-window)
      (call-interactively 'evil-record-macro)))
  (me/evil-keybinds))

(defun me/evil-escape-config ()
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq evil-escape-delay 0.15)

  (add-hook 'evil-escape-inhibit-functions
            (defun +evil-inhibit-escape-in-minibuffer-fn ()
              (and (minibufferp)
                   (or (not (bound-and-true-p evil-collection-setup-minibuffer))
                       (evil-normal-state-p))))))

(defun me/evil-org-config ()
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun me/avy-config ()
  (setq avy-timeout-seconds 0.5)
  (me/avy-keybinds))

(defun me/doom-themes-init ()
  (load-theme 'doom-vibrant t))

(defun me/doom-themes-config ()
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defun me/vertico-config ()
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  (me/vertico-keybinds))

(defun me/vertico-directory-config ()
  (me/vertico-directory-keybinds))

(defun me/consult-init ()
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
        xref-show-definitions-function #'consult-xref))

(defun me/consult-config ()
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
  (setq consult-narrow-key "<")
  (me/consult-keybinds))

(defun me/consult-project-extra-keybinds ()
  (bind-key "C-c p f" 'consult-project-extra-find)
  (bind-key "C-c p o" 'consult-project-extra-find-other-window))

(defun me/meow-init ()
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
(defun me/meow-config ()
  (me/meow-keybinds)
  (meow-global-mode 1)
  (global-set-key (kbd "C-h k") 'helpful-key))

(use-package exec-path-from-shell
  :config
  (me/exec-path-from-shell-config))

(use-package dash
  :commands (global-dash-fontify-mode)
  :init (global-dash-fontify-mode)
  :config (dash-register-info-lookup))

(use-package s)

(use-package persistent-scratch
  :after (no-littering org)
  :custom ((persistent-scratch-autosave-interval 180))
  :config
  (add-hook 'after-init-hook 'persistent-scratch-setup-default))

(use-package meow
  :init (me/meow-init)
  :config (me/meow-config))

(use-package key-chord
  :config
  (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
  (key-chord-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package avy
  :config (me/avy-config))

(use-package doom-themes
  :init (me/doom-themes-init)
  :config (me/doom-themes-config))

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
  (setq which-key-use-C-h-commands nil)
  (setq which-key-idle-delay 0.5))

(use-package vertico
  :init (vertico-mode)
  :config (me/vertico-config))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :config (me/vertico-directory-config)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :init (me/consult-init)
  :config (me/consult-config))

(use-package consult-project-extra
  :config (me/consult-project-extra-keybinds))

































































































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

  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico)


  )

;; Consult users will also want the embark-consult package.
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
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-at-point)
  :bind
  ("H-d" . helpful-at-point)
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable))

(use-package hydra
  :defer t)

(defhydra me/hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(global-set-key (kbd "C-c T f") 'me/hydra-text-scale/body)

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

(global-set-key (kbd "C-c b") 'me/hydra-buffers/body)

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

(use-package origami
  :bind (:map org-super-agenda-header-map
        ("<tab>" . origami-toggle-node))
  :config
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
  :hook ((org-agenda-mode . origami-mode)))

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

(defun me/org-keybinds-setup ()
  (define-key org-mode-map (kbd "C-c o t") 'me/insert-timestamp)
  (bind-key "C-c o c" 'org-clock-goto))

;; Directories
(defconst me/org-dir "~/Org/")
(defconst me/org-notes-dir "~/Org/notes/")
(defconst me/org-roam-dir "~/Org/roam/")
(defconst me/org-roam-notes-dir (concat me/org-roam-dir "notes/"))
(defconst me/org-roam-bookmarks-dir (concat me/org-roam-dir "bookmarks/"))
;; Files
(defconst me/org-todo-file (concat me/org-dir "todo.org"))
(defconst me/org-note-inbox-file (concat me/org-dir "notes/inbox.org"))
(defconst me/org-personal-note-file (concat me/org-dir "notes/personal.org"))
(defconst me/org-work-note-file (concat me/org-dir "notes/work.org"))
(defconst me/org-projects-file (concat me/org-dir "projects.org"))
(defconst me/org-snippet-file (concat me/org-dir "notes/snippets.org"))
(defconst me/org-mobile-file (concat me/org-dir "mobile.org"))
(defconst me/org-distractions-file (concat me/org-dir "distractions.org"))
(defconst me/org-journal-file (concat me/org-dir "journal.org"))
(defconst me/org-archive-file (concat me/org-dir "archive.org"))
(defconst me/org-emacs-note-file (concat me/org-notes-dir "emacs.org"))
(defconst me/org-emacs-config-file (concat user-emacs-directory "README.org"))

;; Archive
(defconst me/org-archive-location (concat me/org-archive-file "::* From %s"))

(defvar me/org-agenda-files (list me/org-todo-file me/org-projects-file me/org-mobile-file me/org-distractions-file me/org-journal-file me/org-emacs-config-file me/org-roam-notes-dir))
(defvar me/org-refile-files (list me/org-todo-file me/org-projects-file me/org-mobile-file me/org-distractions-file me/org-journal-file me/org-archive-file me/org-emacs-note-file))

(defun me/org-settings-setup ()
  (progn
    ;; Directories
    (setq org-directory me/org-dir)
    (setq org-archive-location me/org-archive-location)

    ;; Visuals
    (setq org-ellipsis " ▼ ")
    (setq org-pretty-entities t)

    ;; Behavior
    (setq org-cycle-emulate-tab 'white)
    (setq org-catch-invisible-edits 'smart)
    (setq org-link-search-must-match-exact-headline nil)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-log-state-notes-into-drawer t)
    (setq org-extend-today-until 4)
    (setq org-duration-format 'h:mm)
    (setq-default org-enforce-todo-dependencies t)

    ;; Source Editing
    (setq org-edit-src-turn-on-auto-save t)
    (setq org-src-window-setup 'current-window)

    ;; Time and Clock settings
    (org-clock-persistence-insinuate)
    (setq org-clock-out-when-done t)
                                        ;(setq org-clock-idle-time nil)

    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Use a function to decide what to change the state to.
    (defun me/switch-task-on-clock-start (task-state)
      (if (or (string= task-state "TODO")(string= task-state "NEXT"))
          "PROG"
        task-state))

    (defun me/switch-task-on-clock-out (task-state)
      (if (string= task-state "PROG")
          "NEXT"
        task-state))

    (setq org-clock-in-switch-to-state #'me/switch-task-on-clock-start)

    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    (setq org-clock-report-include-clocking-task t)

    ;; Refile

    (setq org-refile-use-outline-path nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-target-files me/org-refile-files)
    (setq org-refile-targets '((org-refile-target-files :maxlevel . 6)))))

(defun me/org-habit-setup ()
  (progn
    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-today-glyph ?◌)
    (setq org-habit-completed-glyph ?●)
    (setq org-habit-missed-glyph ?○)
    (setq org-habit-preceding-days 10)
    (setq org-habit-show-habits-only-for-today t
          org-habit-graph-column 65)))

(defun me/org-todo-tag-setup ()
  (progn
    (setq org-todo-keywords
          '((sequence "TODO(t)" "REVIEW(r)" "PROJ(P)" "NEXT(n)" "PROG(p!)" "INTR(i!)" "|" "DONE(d!)" "CANCELLED(c!)" "DEPR(D)")))

    (setq org-todo-keyword-faces
          '(
            ("TODO" . (:foreground "#ff39a3" :weight bold))
            ("NEXT" . (:foreground "DeepSkyBlue"
                                   :weight bold))
            ("PROG"  . (:foreground "orangered"
                                    :weight bold))
            ("INTR" . (:foreground "pink"
                                   :weight bold))
            ("DONE" . (:foreground "#008080"
                                   :weight bold))
            ("CANCELLED" . (:foreground "darkgrey"
                                        :weight bold))
            ("NOTE" . (:foreground "#9fc5e8"
                                   :weight bold))
            ("PROJ" . (:foreground "#B4A7D6"
                                   :weight bold))
            ("IDEA" . (:foreground "VioletRed4"
                                   :weight bold))
            ("DEPR" . (:foreground "darkgrey"
                                   :weight bold))))

    (setq org-tag-persistent-alist
          '((:startgroup)
            ("@errand" . ?e)
            ("@home" . ?h)
            ("@work" . ?w)
            (:endgroup)
            ("ARCHIVE" . ?A)
            ("appointment" . ?a)
            ("someday" . ?s)
            ("note" . ?n)
            ("idea" . ?i)
            ("personal" . ?p)
            ("bookmark" . ?b)
            ("health" . ?H)
            ("fun" . ?f)
            ("computer" . ?c)
            ("emacs" . ?E)
            ("goal" . ?g)
            ("routine" . ?r)))

    (setq org-tag-faces
          '(("@errand" . (:foreground "mediumPurple1" :weight bold))
            ("@home" . (:foreground "royalblue1" :weight bold))
            ("@work" . (:foreground "#1CC436" :weight bold))))))

(defun me/org-agenda-setup ()
  (progn
    (me/org-agenda-keybinds)
    (setq org-agenda-files me/org-agenda-files)
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
    (setq org-cycle-separator-lines 2)
    (setq org-agenda-category-icon-alist
          `(("work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
            ("personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
            ("computer" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
            ("calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)))

    (defun me/org-agenda-place-point ()
      (goto-char (point-min)))

    (add-hook 'org-agenda-finalize-hook #'me/org-agenda-place-point 90)))

(defun me/org-agenda-keybinds ()
  (progn
    ))

(defun me/org-capture-setup ()
  (progn
    (defun my-org-capture-place-template-dont-delete-windows (oldfun &rest args)
      (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
        (apply oldfun args)))

    (with-eval-after-load "org-capture"
      (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))

    (setq org-capture-templates
          '(("c" "Current" entry
             (file+headline me/org-todo-file "Personal Inbox")
             "* PROG %?\n%U\n" :prepend t :clock-in t :clock-keep t :clock-resume t)
            ;; Personal ;;
            ("d" "Distraction" entry
             (file+olp+datetree me/org-distractions-file)
             "* %U - %? :distraction:\n")

            ("n" "Note" entry
             (file me/org-note-inbox-file)
             "* NOTE %?\n%U\n" :prepend t)

            ("e" "Emacs Task" entry
             (file+headline me/org-todo-file "Emacs")
             "* TODO %?\n%U\n" :prepend t)

            ("E" "Emacs Note" entry
             (file+headline me/org-emacs-note-file "Inbox")
             "* NOTE %?\n%U\n" :prepend t)

            ("t" "Task" entry
             (file+headline me/org-todo-file "Personal Inbox")
             "* TODO %?\n%U\n" :prepend t)

            ("T" "Task (Scheduled)" entry
             (file+headline me/org-todo-file "Personal Inbox")
             "* TODO %?\nSCHEDULED: %^T\n" :prepend t)

            ("a" "Scheduled Appointment" entry
             (file+headline me/org-todo-file "Appointments")
             "* APT %?\n%^T\n" :prepend t)

            ("A" "Active Appointment Notes" entry
             (file+headline me/org-personal-note-file "Appointment Notes")
             "* NOTE %?\n%U\n" :prepend t :clock-in t :clock-resume t)

            ("s" "Snippet" entry
             (file+headline me/org-snippet-file "Snippet Inbox")
             "* %?\n%U\n%i\n" :prepend t)

            ("i" "Interuption" entry
             (file+headline me/org-todo-file "Interuptions")
             "* INTR [#A] %?\n%T\n" :prepend t :clock-in t :clock-resume t)

            ("j" "Journal" entry
             (file+olp+datetree "~/Org/journal.org")
             "* %U - %?\n")

            ;; Work ;;
            ("w" "Work Captures")

            ("wt" "Work Task" entry
             (file+headline me/org-todo-file "Work Inbox")
             "* TODO %?\n%U\n" :prepend t)

            ("wT" "Work Task (Scheduled)" entry
             (file+headline me/org-todo-file "Work Inbox")
             "* TODO %?\nSCHEDULED: %^T\n" :prepend t)

            ("wm" "Scheduled Meeting" entry
             (file+headline me/org-todo-file "Work Meetings")
             "* APT %?\n%^T\n" :prepend t)

            ("wM" "Active Meeting Notes" entry
             (file+headline me/org-work-note-file "Meeting Notes")
             "* NOTE %?\n%U\n" :prepend t :clock-in t :clock-resume t)

            ("wf" "Family Office Task" entry
             (file+headline me/org-projects-file "Family Office")
             "* TODO %?\n%U\n" :prepend t)

            ("ws" "Shareholder Task" entry
             (file+headline me/org-projects-file "Shareholder")
             "* TODO %?\n%U\n" :prepend t)

            ("wa" "Architecture Task" entry
             (file+headline me/org-projects-file "Architecture")
             "* TODO %?\n%U\n" :prepend t)
            ("wr" "Code Review" entry
             (file+headline me/org-projects-file "Admin")
             "* TODO %? :review:\nSCHEDULED: %^T\n" :prepend t)
            ))))

(defun me/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
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
  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . me/org-mode-initial-setup)
  :bind (("C-c c" . org-capture)
         ;("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c o s" . org-save-all-org-buffers)
         :map org-mode-map
         ("C-c ?" . nil)
         ("C-c T ?" . org-table-field-info)
         :map org-agenda-mode-map
         ("C-c o l" . org-agenda-log-mode))
  :config
  (me/org-settings-setup)
  (me/org-keybinds-setup)
  (me/org-habit-setup)
  (me/org-todo-tag-setup)
  (me/org-agenda-setup)
  (me/org-agenda-keybinds)
  (me/org-capture-setup)
  (me/org-font-setup))

(use-package org-contrib
  :after org)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config

  (cond (IS-MAC (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 200))
        (IS-WINDOWS (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 90)))

  (setq org-superstar-todo-bullet-alist
    '(("TODO" . ?λ)
      ("NEXT" . ?✰)
      ("PROG" . ?∞)
      ("INTR" . ?‼)
      ("DONE" . ?✔)
      ("CANCELLED" . ?✘)
      ("NOTE" . ?✎)
      ("PROJ" . ?⚙)
      ("IDEA" . ?⚛)
      ("DEPR" . ?✘)))

   (setq org-superstar-item-bullet-alist
    '((?* . ?•)
      (?+ . ?➤)
      (?- . ?•)))

   (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
   (setq org-superstar-special-todo-items t)
   (setq org-superstar-leading-bullet " ")
   (org-superstar-restart))

(defun me/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . me/org-mode-visual-fill))

(use-package org-super-agenda
  :after (org)
  :init (me/org-agenda-keybinds)
  :config
  (org-super-agenda-mode)
  (define-key org-super-agenda-header-map (kbd "z") nil)
  (define-key org-super-agenda-header-map (kbd "j") nil)
  (define-key org-super-agenda-header-map (kbd "k") nil)
  (define-key org-super-agenda-header-map (kbd "g") nil)

  (setq org-super-agenda-groups '((:habit t :order 99)
                                  (:name "In Progress"
                                         :todo "PROG")
                                  (:name "Next to do"
                                         :todo "NEXT")
                                  (:name "Interupts"
                                         :todo "INTR")
                                  (:name "Due Today"
                                         :deadline today)
                                  (:name "Due Soon"
                                         :deadline future)
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Today"
                                         :scheduled today)
                                  (:name "Maybe Today"
                                         :date today)
                                  (:name "Projects"
                                         :todo "PROJ")
                                  (:name "Future"
                                         :scheduled future)
                                  (:name "Work"
                                         :tag "@work"
                                         :order 97)
                                  (:name "Inbox"
                                         :tag ("inbox" "mobile"))

                                  (:name "Home" :tag "@home")
                                  (:name "Appointments"
                                         :tag "appointment")
                                  (:name "Grocery List"
                                         :tag "groceries")
                                  (:name "Emacs"
                                         :tag "emacs")
                                  (:name "Health"
                                         :tag "health")
                                  (:name "Errands"
                                         :tag "@errand")
                                  (:name "Computer"
                                         :category "computer")
                                  (:auto-category t :order 99)))

  (setq org-agenda-custom-commands '(("a" "POG AGENDA"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-log-mode t)
                                                   (org-super-agenda-groups
                                                    '((:order-multi (99
                                                                     (:name "Clocked Today" :log clock)
                                                                     (:name "Done Today" :and (:log t))))
                                                      (:name "Habits" :habit t :order 97)
                                                      (:name "Work" :tag "@work" :order 98)
                                                      (:name "Interupts" :todo "INTR")
                                                      (:name "In Progress" :todo "PROG")
                                                      (:name "Next" :todo "NEXT")
                                                      (:name "Overdue" :deadline past)
                                                      (:name "Due Today" :deadline today)
                                                      (:name "Scheduled Earlier" :scheduled past)
                                                      (:name "Today"
                                                             :scheduled today)
                                                      (:name "Maybe Today"
                                                             :date today
                                                             )))))
                                       (alltodo "" ((org-agenda-overriding-header "\nAll Tasks")
                                                    (org-super-agenda-groups
                                                     '((:discard (:habit t))
                                                       (:name "In Progress"
                                                              :todo "PROG")
                                                       (:name "Next to do"
                                                              :todo "NEXT")
                                                       (:name "Interupts"
                                                              :todo "INTR")
                                                       (:name "Due Today"
                                                              :deadline today)
                                                       (:name "Due Soon"
                                                              :deadline future)
                                                       (:name "Overdue"
                                                              :deadline past)
                                                       (:name "Projects"
                                                              :todo "PROJ")
                                                       (:name "Future"
                                                              :scheduled future)
                                                       (:name "Work"
                                                              :tag "@work"
                                                              :order 97)
                                                       (:name "Inbox"
                                                              :tag ("inbox" "mobile"))

                                                       (:name "Home" :tag "@home")
                                                       (:name "Appointments"
                                                              :tag "appointment")
                                                       (:name "Grocery List"
                                                              :tag "groceries")
                                                       (:name "Emacs"
                                                              :tag "emacs")
                                                       (:name "Health"
                                                              :tag "health")
                                                       (:name "Errands"
                                                              :tag "@errand")
                                                       (:name "Computer"
                                                              :category "computer")
                                                       (:auto-category t :order 99))))))))))

(use-package org-ql
  :config
  (bind-key "C-c a a" 'org-agenda)
  (bind-key "C-c a s" 'org-ql-search)
  (bind-key "C-c a v" 'org-ql-view)
  (bind-key "C-c a t" 'org-ql-view-sidebar)
  (bind-key "C-c a S" 'org-ql-sparse-tree)
  (bind-key "C-c a r" 'org-ql-view-recent-items)
  (bind-key "C-c a f" 'org-ql-find)
  (bind-key "C-c a p" 'org-ql-find-path)
  (bind-key "C-c a h" 'org-ql-find-heading)

  (setq org-ql-views nil)


  (add-to-list 'org-ql-views '("Inbox" :buffers-files org-agenda-files :query
                               (and
                                (not
                                 (done))
                                (tags "inbox")
                                (todo)
                                )
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
                               (and (done)
                                    (not (tags "ARCHIVE")))
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
                               (todo priority date))))

(use-package org-sidebar
  :config
  (bind-key "C-c a T" 'org-sidebar-tree-toggle))



(use-package org-roam
  :custom
  (org-roam-directory (file-truename me/org-roam-dir))
  :bind (("C-c n l" . org-roam-buffer-toggle)
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
  :config
  (require 'org-roam-dailies)

  (setq org-id-link-to-org-use-id t)

  (setq org-id-extra-files (org-roam--list-files me/org-roam-dir))

  (setq org-roam-completion-everywhere t)

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "\n%?"
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t)
          ("f" "Fleeting Note" plain
           "\n%?"
           :target (file+head "fleeting/%<%Y%m%d-%H%M%S>.org"
                              "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n")
           :unnarrowed t)
          ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :project:")
           :unnarrowed t)
          ("b" "bookmark" plain "%c%?"
           :if-new (file+head "bookmarks/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :bookmark:")
           :unnarrowed t)
          ))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-mode-sections '(org-roam-backlinks-section org-roam-reflinks-section org-roam-unlinked-references-section))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (org-roam-db-autosync-mode))

(setq org-confirm-babel-evaluate nil)

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; Automatically tangle our Emacs.org config file when we save it
(defun me/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'me/org-babel-tangle-config)))

(defun me/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . me/lsp-mode-setup)
  :init
  ;(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package json-mode)

(use-package magit
  :bind (("C-c g s" . magit))
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit
  :init (setq forge-add-default-bindings t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package format-all
  :hook (prog-mode . format-all-mode)
  :bind(("C-c F" . format-all-buffer)
        ("C-c x f b" . format-all-buffer)))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :bind (("C-c t" . vterm))
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "fish")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun me/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . me/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package fish-mode)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-dwim-target t))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles)

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
      erc-hide-list '("JOIN" "PART" "QUIT" "KICK" "NICK" "MODE" "333" "353"))
