
(setq inhibit-startup-message t)
(setq backup-inhibited t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room


(menu-bar-mode -1)            ; Disable the menu bar

(global-set-key (kbd "<escape>") 'keyboard-quit)

(setq scroll-up-aggressively nil)
(setq scroll-down-aggressively nil)
(setq scroll-conservatively 101)

(defvar myemacs-escape-hook nil 
  "for killing things")

(defun myemacs/escape (&optional interactive)
  "Run `myemacs-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'myemacs-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'myemacs/escape)
(add-hook 'myemacs-escape-hook (lambda ()
				 (when (evil-ex-hl-active-p 'evil-ex-search)
				   (evil-ex-nohighlight)
				   t)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package all-the-icons)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(use-package general
  ;:config
  ;(general-create-definer myemacs/leader
  ;  :keymaps '(normal insert visual emacs)
  ;  :prefix "SPC"
  ;  :global-prefix "M-SPC")
  )

(setq scroll-step 1)
(setq scroll-margin 8)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;:bind-keymap
  ;(myemacs/leader "p" . projectile-mode-map)
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package org)
(use-package org-superstar
  :after org)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-hide-leading-stars t)

(defvar myemacs-leader-map (make-sparse-keymap)
  "map for leader")
(setq leader "SPC")
(setq alt-leader "M-SPC")

(define-prefix-command 'myemacs/leader 'myemacs-leader-map)
(define-key myemacs-leader-map [override-state] 'all)


(evil-define-key* '(normal visual motion) general-override-mode-map (kbd leader) 'myemacs/leader)
(global-set-key (kbd alt-leader) 'myemacs/leader)
(general-override-mode +1)


(define-key myemacs-leader-map (kbd ".") '("find file" . counsel-find-file))
(define-key myemacs-leader-map (kbd "<") '("switch buffer" . counsel-switch-buffer))


(which-key-add-keymap-based-replacements myemacs-leader-map "b" "buffer")
(define-key myemacs-leader-map (kbd "bk") '("kill buffer" . kill-current-buffer))
(define-key myemacs-leader-map (kbd "bi") '("ibuffer" . ibuffer))
(define-key myemacs-leader-map (kbd "bn") '("next buffer" . evil-next-buffer))
(define-key myemacs-leader-map (kbd "bp") '("previous buffer" . evil-prev-buffer))

(which-key-add-keymap-based-replacements myemacs-leader-map "g" "git")
(define-key myemacs-leader-map (kbd "gg") '("Magit status" . magit-status))
 
(define-key myemacs-leader-map (kbd "h") '("help" . help-command))
(define-key myemacs-leader-map (kbd "w") '("window" . evil-window-map))
(define-key myemacs-leader-map (kbd "p") '("project" . projectile-command-map))
