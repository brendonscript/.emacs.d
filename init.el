;;; -*- lexical-binding: t -*-
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; Disable warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-minimum-level :error)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

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

;; You will most likely need to adjust this font size for your system!
(defvar me/default-font-size 160)
(defvar me/default-variable-font-size 160)

;; Make frame transparency overridable
(defvar me/frame-transparency '(95 . 95))

(defconst IS-MAC     (eq system-type 'darwin)
  "If the host is running MacOS return true")
(defconst IS-LINUX   (eq system-type 'gnu/linux)
  "If the host is running Linux return true")
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos))
  "If the host is running Windows return true")
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix))
  "If the host is running BSD return true")

(defun me/alternate-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun me/save-all-unsaved ()
  "Save all unsaved files. no ask."
  (interactive)
  (save-some-buffers t))

  (add-hook 'focus-out-hook 'me/save-all-unsaved)
  (setq after-focus-change-function 'me/save-all-unsaved)

(setq inhibit-startup-message t)

;; Have to wrap all of these due to them not working inside Termux on Android or inside terminals
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

(setq visible-bell nil)            ;; Set up the visible bell
(setq ring-bell-function #'ignore) ;; Disable the annoying bell sound

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha me/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,me/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq resize-mini-windows t)

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

(set-face-attribute 'default nil :font "Fira Code Retina" :height me/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height me/default-font-size)

;; (when '(IS-MAC)
;;   (set-face-attribute 'default nil :font "MonoLisa Custom" :height me/default-font-size)
;;   (set-face-attribute 'fixed-pitch nil :font "MonoLisa Custom" :height me/default-font-size))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height me/default-variable-font-size :weight 'regular)

(defalias 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq mac-command-modifier 'control
        mac-option-modifier 'meta
        mac-control-modifier 'super
        mac-right-command-modifier 'control
        mac-right-option-modifier 'meta
        ns-function-modifier 'hyper)

(defun me/open-config ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/README.org")))

(global-set-key (kbd "C-c e e") 'me/open-config)

(global-set-key (kbd "C-c e q") 'save-buffers-kill-emacs)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

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

(set-language-environment "UTF-8")

(when '(IS-MAC)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

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

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  :config

  (evil-mode 1)

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

  ;; Initial states
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
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
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package god-mode
   ;; :bind (("C-S-g" . god-mode))
   :defer t)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package avy
  :config

  (evil-global-set-key 'motion (kbd "C-:") 'avy-resume)
  (evil-global-set-key 'motion (kbd "C-f") 'avy-goto-char)
  (evil-global-set-key 'motion (kbd "C-'") 'avy-goto-char-2)
  (evil-global-set-key 'motion (kbd "M-g g") 'avy-goto-line)
  (evil-global-set-key 'motion (kbd "M-g w") 'avy-goto-word-1)
  (evil-global-set-key 'motion (kbd "M-g e") 'avy-goto-word-0)


  (global-set-key (kbd "C-:") 'avy-resume)
  (global-set-key (kbd "C-f") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0))

(use-package doom-themes
  :init (load-theme 'doom-vibrant t))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after all-the-icons
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :after all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
           (doom-modeline-bar-width 6)
           (doom-modeline-minor-modes t)
           (doom-modeline-buffer-file-name-style 'truncate-except-project)
           (doom-modeline-minor-modes nil)
           (doom-modeline-persp-name t)
           (doom-modeline-display-default-persp-name t)
           (doom-modeline-persp-icon t)
           (doom-modeline-modal-icon t))
  ;; This configuration to is fix a bug where certain windows would not display
  ;; their full content due to the overlapping modeline
  :config (advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t))))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-use-C-h-commands nil)
  (setq which-key-idle-delay 0.5))

(use-package savehist
  :init
  (savehist-mode))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))  ; pick your own prefix key here
  :init
  (setq persp-state-default-file (concat persp-save-dir "persp-state"))
  (setq persp-modestring-short t)
  :config
  (unless (equal persp-mode t)
    (persp-mode))
  (add-hook 'kill-emacs-hook #'persp-state-save))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-c s a" . consult-org-agenda)
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
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;;:hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key (kbd "M-."))
  (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (setq consult-narrow-key "<"))

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
                 (window-parameters (mode-line-format . none)))))

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
;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
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

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-J" . vertico-next-group)
              ("C-k" . vertico-previous)
              ("C-K" . vertico-previous-group)
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("M-TAB" . minibuffer-complete)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  ;;:custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("H-h" . helpful-at-point)
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

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

(defun me/org-mode-initial-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defvar me/org-dir "~/Org")

(defvar me/org-archive-location "~/Org/archive.org::* From %s")

(defvar me/org-agenda-files '("archive.org" "todo.org" "inbox.org" "projects.org" "distractions.org" "mobile.org" "journal.org" "routines.org"))

(defvar me/org-refile-files '("archive.org" "todo.org" "inbox.org" "projects.org" "someday.org" "distractions.org" "mobile.org" "journal.org" "routines.org"))

(defun me/org-settings-setup ()
  (progn
    ;; Directories
    (setq org-directory me/org-dir)
    (setq org-archive-location me/org-archive-location)

    ;; Visuals
    (setq org-ellipsis " ▾")
    (setq org-pretty-entities t)

    ;; Behavior
    (setq org-cycle-emulate-tab 'whitestart)
    (setq org-catch-invisible-edits 'smart)
    (setq org-link-search-must-match-exact-headline nil)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-extend-today-until 7)
    (setq org-duration-format 'h:mm)
    (setq-default org-enforce-todo-dependencies t)

    ;; Source Editing
    (setq org-edit-src-turn-on-auto-save t)

    ;; Time and Clock settings
    (setq org-clock-out-when-done t)
    (setq org-clock-idle-time nil)

    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Change tasks to whatever when clocking in
    (setq org-clock-in-switch-to-state "PROG")

    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)

    ;; Refile
    (setq org-refile-target-files me/org-refile-files)
    (setq org-refile-targets '((org-refile-target-files :maxlevel . 2)))))

(defun me/insert-timestamp ()
  (interactive)
  (let ((current-prefix-arg '(16))) (call-interactively 'org-time-stamp-inactive))) ; Universal Argument x2 - 4*4
(global-set-key (kbd "C-c o t") 'me/insert-timestamp)

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
          '((sequence "TODO(t)" "NEXT(n)" "PROG(p!)" "INTR(i!)" "|" "DONE(d!)" "CANCELLED(c!)")
            (sequence "|" "NOTE(N)" "PROJ(P)" "IDEA(I)" "DEPR(D)")))

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
            ("@errand" . ?E)
            ("@home" . ?h)
            ("@work" . ?w)
            ("@emacs" . ?e)
            (:endgroup)
            ("routine" . ?r)
            ("bookmark" . ?b)
            ("idea" . ?i)
            ("distraction" . ?d)))
    (setq org-tag-faces
          '(("@errand" . (:foreground "mediumPurple1" :weight bold))
            ("@home" . (:foreground "royalblue1" :weight bold))
            ("@work" . (:foreground "#1CC436" :weight bold))
            ("@emacs" . (:foreground "forest green" :weight bold))
            ("routine" . (:foreground "#CFE2F3" :weight regular))
            ("bookmark" . (:foreground "yellow1" :weight bold))
            ("idea" . (:foreground "pink" :weight bold))
            ("distraction" . (:foreground "red1" :weight bold))))))

(defun me/org-agenda-setup ()
  (progn
    (setq org-agenda-files me/org-agenda-files)
    (setq org-agenda-start-with-log-mode nil)
    (setq org-agenda-use-time-grid nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-start-day "-2d")
    (setq org-agenda-span 7)
    (setq org-agenda-todo-ignore-scheduled 'future)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-deadline-warning-days 0)
    (setq org-agenda-compact-blocks t)
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                        (todo priority-down category-keep)
                                        (tags priority-down category-keep)
                                        (search category-keep)))
    ;; (setq org-agenda-category-icon-alist
    ;;       `(("work" ,(list (all-the-icons-faicon "briefcase")) nil nil :ascent center)
    ;;         ("work projects" ,(list (all-the-icons-material "build")) nil nil :ascent center)))

    (setq org-agenda-prefix-format '((agenda  . " %i %-12:c%?-12t% s")
                                        ;(agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
                                     (timeline  . "  % s")
                                     (todo  . " %i %-12:c")
                                     (tags  . " %i %-12:c")
                                     (search . " %i %-12:c")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and all TODOs"
             ((agenda "")
              (alltodo "" ((org-agenda-tag-filter-preset '("-routine"))))))
            ("r" "Routines"
             ((agenda "" ((org-agenda-tag-filter-preset '("+routine"))))))
            ("w" "Work Agenda"
             ((agenda "" (
                          (org-agenda-span 7)
                          (org-agenda-tag-filter-preset '("+@work"))))
              (alltodo "" ((org-agenda-tag-filter-preset '("+@work"))))))))
    ))

(add-hook 'org-agenda-finalize-hook #'org-agenda-find-same-or-today-or-agenda 90)


;; (setq org-agenda-custom-commands
;;       '(("n" "Agenda and all TODOs"
;;          ((agenda "")
;;           (alltodo "")))
;; 	  ("s" "Super View"
;; 	   (
;; 	    (org-ql-block '(and (todo "TODO" "NEXT" "PROG" "INTR")
;; 				(planning 7)
;; 			  (not (tags "@work" "routine")))
;; 			  ((org-ql-block-header "Upcoming")))
;; 	    (org-ql-block '(and (todo "TODO" "NEXT" "PROG" "INTR")
;; 				(tags "@work")
;; 				(planning 1))
;; 			  ((org-ql-block-header "Work")))
;; 	    (agenda "" ((let org-agenda-category-filter-preset '("-routine"))))
;; 	    (org-ql-block '(and (todo "TODO" "NEXT" "PROG" "INTR")
;; 				(tags "routine")
;; 				(planning 0)))
;; 	  ))))

(defun me/org-capture-setup ()
  (progn
    (setq org-capture-templates
          '(
            ("d" "Distraction" entry
             (file+headline "~/Org/distractions.org" "Distractions")
             "* %?\n%U\n" :prepend t)
            ("n" "Note" entry
             (file+headline "~/Org/inbox.org" "Note Inbox")
             "* %?\n%U\n" :prepend t)
            ("t" "New Task" entry
             (file+headline "~/Org/inbox.org" "Task Inbox")
             "* TODO %?\n%U\n" :prepend t)
            ("T" "New Task Today" entry
             (file+headline "~/Org/inbox.org" "Task Inbox")
             "* TODO %?\nSCHEDULED: %t" :prepend t)
            ("w" "Work Captures")
            ("wt" "Work Task" entry
             (file+headline "~/Org/inbox.org" "Work Inbox")
             "* TODO %?\n%U\n" :prepend t)
            ("wT" "Work Task Today" entry
             (file+headline "~/Org/inbox.org" "Work Inbox")
             "* TODO %?\nSCHEDULED: %t" :prepend t)
            ("wf" "Family Office Task" entry
             (file+headline "~/Org/projects.org" "Family Office")
             "* TODO %?\n%U\n" :prepend t)
            ("wa" "Architecture Task" entry
             (file+headline "~/Org/projects.org" "Architecture")
             "* TODO %?\n%U\n" :prepend t)
            ("a" "Snippet" entry
             (file+headline "~/Org/inbox.org" "Snippet Notes")
             "* %?\n%U\n%i\n" :prepend t)
            ("i" "Interuptions" entry
             (file+headline "~/Org/todo.org" "Interuptions")
             "* INTR %?\nSCHEDULED: %t\n" :prepend t)
            ("j" "Journal" entry
             (file+olp+datetree "~/Org/journal.org")
             "* %U\n%?\n")
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
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

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
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c o s" . org-save-all-org-buffers)
         :map org-mode-map
         ("C-c ?" . nil)
         ("C-c T ?" . org-table-field-info)
         :map org-agenda-mode-map
         ("C-c o l" . org-agenda-log-mode))
  :config
  (me/org-settings-setup)
  (me/org-habit-setup)
  (me/org-todo-tag-setup)
  (me/org-agenda-setup)
  (me/org-capture-setup)
  (me/org-font-setup)
  (require 'org-protocol))

(use-package org-contrib
  :after org)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 180)
  :custom
  (org-superstar-todo-bullet-alist
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

 (org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))

  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (org-superstar-special-todo-items t)
  (org-superstar-leading-bullet ""))

(defun me/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . me/org-mode-visual-fill))

(use-package org-pretty-tags
  :commands (org-pretty-tags-global-mode)
  :init (org-pretty-tags-global-mode t)
  :config
  (setq org-pretty-tags-surrogate-strings '(("@errand" "🛒")
                                            ("@home" "🏡")
                                            ("@work" "💼")
                                            ("@emacs" "⌨️")
                                            ("routine" "🔁")
                                            ("bookmark" "🔖")
                                            ("idea" "💡")
                                            ("distraction" "❓")
                                            ("ARCHIVE" "🗄️")
                                            )))

(use-package org-ql)

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
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup)) ;; Automatically installs Node debug adapter if needed

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package json-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :bind (("C-c g s" . magit))
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

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
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000)
  (evil-set-initial-state 'vterm-mode 'emacs))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun me/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

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
  (setq dired-dwim-target t)
  (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer)
  )

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

(use-package dired-hide-dotfiles
  ;;:hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq erc-server "irc.libera.chat"
      erc-nick "geoffery"
      erc-user-full-name "Geoffery"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc-libera.chat" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
