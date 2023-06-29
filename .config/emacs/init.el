(setq inhibit-startup-message t)
(setq backup-inhibited t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq scroll-up-aggressively nil)
(setq scroll-down-aggressively nil)
(setq scroll-conservatively 101)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq scroll-up-aggressively nil)
(setq scroll-down-aggressively nil)
(setq scroll-conservatively 101)

(setq scroll-step 1)
(setq scroll-margin 8)

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

(setq straight-use-package-by-default t)
;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;; 			   ("org" . "https://orgmode.org/elpa/")
;; 			   ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

;; (use-package ivy
;;     :diminish
;;     :bind (("C-s" . swiper)
;; 	     :map ivy-minibuffer-map
;; 	     ("TAB" . ivy-alt-done)
;; 	     ("C-l" . ivy-alt-done)
;; 	     ("C-j" . ivy-next-line)
;; 	     ("C-k" . ivy-previous-line)
;; 	     :map ivy-switch-buffer-map
;; 	     ("C-k" . ivy-previous-line)
;; 	     ("C-l" . ivy-done)
;; 	     ("C-d" . ivy-switch-buffer-kill)
;; 	     :map ivy-reverse-i-search-map
;; 	     ("C-k" . ivy-previous-line)
;; 	     ("C-d" . ivy-reverse-i-search-kill))
;;     :config
;;     (ivy-mode 1))

;; (use-package ivy-rich
;;   :after ivy
;;   :init
;;   (ivy-rich-mode 1))

;; (use-package counsel
;;   :bind (:map minibuffer-local-map
;; 	   ("C-r" . 'counsel-minibuffer-history))
;;   :custom
;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;   :config
;;   (counsel-mode 1))

;; (use-package ivy-prescient
;;   :after counsel
;;   :custom
;;   (ivy-prescient-enable-filtering t)
;;   (ivy-prescient-retain-classic-highlighting t)
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;; 					  ;(prescient-persist-mode 1)
;;   (ivy-prescient-mode 1))

(use-package vertico
  :bind (:map vertico-map
	      ("C-n" . vertico-next)
	      ("C-p" . vertico-previous))
  :init
  (vertico-mode 1))
(use-package savehist
  :init
  (savehist-mode 1))
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))
(use-package consult)
(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package doom-modeline
  :init
  (setq doom-modeline-display-default-persp-name t)
  (doom-modeline-mode 1)
  :custom ((doom-modeline-height 35)))

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful)
  ;; :custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  ;; :bind
  ;; ([remap describe-function] . counsel-describe-function)
  ;; ([remap describe-command] . helpful-command)
  ;; ([remap describe-variable] . counsel-describe-variable)
  ;; ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-anzu
  :config
  (global-anzu-mode 1))

(use-package org
  :config
  (setq org-ellipsis " ▾"))
(use-package org-superstar
  :after org)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-hide-leading-stars t)
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package general
  ;:config
  ;(general-create-definer myemacs/leader
  ;  :keymaps '(normal insert visual emacs)
  ;  :prefix "SPC"
  ;  :global-prefix "M-SPC")
  )

(use-package smartparens
  :config
  (setq sp-highlight-pair-overlay nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (smartparens-global-mode 1))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package persp-mode
  :init
  (setq persp-nil-name "main")
  :config
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))
(load (expand-file-name "~/.config/emacs/workspace.el"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (c-mode . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-update-mode 'point)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-ignore-duplicate t))

(use-package lsp-haskell
  :hook
  (haskell-mode . lsp))

(use-package lsp-treemacs
  :after lsp)
(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :init (global-company-mode 1)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))
(use-package tree-sitter-langs)

(use-package highlight-quoted
  :ensure t
  :config
  (require 'highlight-quoted)
  (add-hook 'emacs-lisp-mode 'highlight-quoted-mode))

(use-package eros
  :config
  (eros-mode 1))

(use-package harpoon
  :straight '(:package "harpoon.el" :host github :type git :repo "NAHTAIV3L/harpoon.el"))

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

(global-set-key (kbd "<escape>") 'keyboard-quit)

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

(defvar myemacs-leader-map (make-sparse-keymap)
 "map for leader")
(setq leader "SPC")
(setq alt-leader "M-SPC")

(define-prefix-command 'myemacs/leader 'myemacs-leader-map)
(define-key myemacs-leader-map [override-state] 'all)

(evil-define-key* '(normal visual motion) general-override-mode-map (kbd leader) 'myemacs/leader)
(global-set-key (kbd alt-leader) 'myemacs/leader)
(general-override-mode +1)

(define-key myemacs-leader-map (kbd ".") '("find file" . find-file))
(define-key myemacs-leader-map (kbd "<") '("switch buffer" . switch-to-buffer))
(define-key myemacs-leader-map (kbd "s") '("search in file" . consult-line))

(evil-global-set-key 'normal "gc" 'evilnc-comment-operator)
(evil-global-set-key 'visual "gc" 'evilnc-comment-operator)

(which-key-add-keymap-based-replacements myemacs-leader-map "t" "toggle")
(define-key myemacs-leader-map (kbd "ts") '("text scaling" . hydra-text-scale/body))

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
(unbind-key (kbd "ESC") projectile-command-map)

(which-key-add-keymap-based-replacements myemacs-leader-map "TAB" "workspace")
(define-key myemacs-leader-map (kbd "TAB TAB") '("list workspaces" . +workspace/display))
(define-key myemacs-leader-map (kbd "TAB n") '("new workspace" . +workspace/new))
(define-key myemacs-leader-map (kbd "TAB d") '("delete workspace" . +workspace/delete))
(define-key myemacs-leader-map (kbd "TAB r") '("rename workspace" . +workspace/rename))
(define-key myemacs-leader-map (kbd "TAB .") '("switch workspace" . +workspace/switch-to))
(define-key myemacs-leader-map (kbd "TAB [") '("previous workspace" . +workspace/switch-left))
(define-key myemacs-leader-map (kbd "TAB ]") '("next workspace" . +workspace/switch-right))
(define-key myemacs-leader-map (kbd "TAB 1") '("switch to workspace 1" . +workspace/switch-to-1))
(define-key myemacs-leader-map (kbd "TAB 2") '("switch to workspace 2" . +workspace/switch-to-2))
(define-key myemacs-leader-map (kbd "TAB 3") '("switch to workspace 3" . +workspace/switch-to-3))
(define-key myemacs-leader-map (kbd "TAB 4") '("switch to workspace 4" . +workspace/switch-to-4))
(define-key myemacs-leader-map (kbd "TAB 5") '("switch to workspace 5" . +workspace/switch-to-5))
(define-key myemacs-leader-map (kbd "TAB 6") '("switch to workspace 6" . +workspace/switch-to-6))
(define-key myemacs-leader-map (kbd "TAB 7") '("switch to workspace 7" . +workspace/switch-to-7))
(define-key myemacs-leader-map (kbd "TAB 8") '("switch to workspace 8" . +workspace/switch-to-8))
(define-key myemacs-leader-map (kbd "TAB 9") '("switch to workspace 9" . +workspace/switch-to-9))
(define-key myemacs-leader-map (kbd "TAB 0") '("switch to final workspace" . +workspace/switch-to-final))
(define-key general-override-mode-map (kbd "M-1") '("switch to workspace 1" . +workspace/switch-to-1))
(define-key general-override-mode-map (kbd "M-2") '("switch to workspace 2" . +workspace/switch-to-2))
(define-key general-override-mode-map (kbd "M-3") '("switch to workspace 3" . +workspace/switch-to-3))
(define-key general-override-mode-map (kbd "M-4") '("switch to workspace 4" . +workspace/switch-to-4))
(define-key general-override-mode-map (kbd "M-5") '("switch to workspace 5" . +workspace/switch-to-5))
(define-key general-override-mode-map (kbd "M-6") '("switch to workspace 6" . +workspace/switch-to-6))
(define-key general-override-mode-map (kbd "M-7") '("switch to workspace 7" . +workspace/switch-to-7))
(define-key general-override-mode-map (kbd "M-8") '("switch to workspace 8" . +workspace/switch-to-8))
(define-key general-override-mode-map (kbd "M-9") '("switch to workspace 9" . +workspace/switch-to-9))
(define-key general-override-mode-map (kbd "M-0") '("switch to final workspace" . +workspace/switch-to-final))

(define-key myemacs-leader-map (kbd "1") '("harpoon go to 1" . harpoon-go-to-1))
(define-key myemacs-leader-map (kbd "2") '("harpoon go to 2" . harpoon-go-to-2))
(define-key myemacs-leader-map (kbd "3") '("harpoon go to 3" . harpoon-go-to-3))
(define-key myemacs-leader-map (kbd "4") '("harpoon go to 4" . harpoon-go-to-4))
(define-key myemacs-leader-map (kbd "5") '("harpoon go to 5" . harpoon-go-to-5))
(define-key myemacs-leader-map (kbd "6") '("harpoon go to 6" . harpoon-go-to-6))
(define-key myemacs-leader-map (kbd "7") '("harpoon go to 7" . harpoon-go-to-7))
(define-key myemacs-leader-map (kbd "8") '("harpoon go to 8" . harpoon-go-to-8))
(define-key myemacs-leader-map (kbd "9") '("harpoon go to 9" . harpoon-go-to-9))

(which-key-add-keymap-based-replacements myemacs-leader-map "d" "delete")
(define-key myemacs-leader-map (kbd "d1") '("harpoon delete 1" . harpoon-delete-1))
(define-key myemacs-leader-map (kbd "d2") '("harpoon delete 2" . harpoon-delete-2))
(define-key myemacs-leader-map (kbd "d3") '("harpoon delete 3" . harpoon-delete-3))
(define-key myemacs-leader-map (kbd "d4") '("harpoon delete 4" . harpoon-delete-4))
(define-key myemacs-leader-map (kbd "d5") '("harpoon delete 5" . harpoon-delete-5))
(define-key myemacs-leader-map (kbd "d6") '("harpoon delete 6" . harpoon-delete-6))
(define-key myemacs-leader-map (kbd "d7") '("harpoon delete 7" . harpoon-delete-7))
(define-key myemacs-leader-map (kbd "d8") '("harpoon delete 8" . harpoon-delete-8))
(define-key myemacs-leader-map (kbd "d9") '("harpoon delete 9" . harpoon-delete-9))

(which-key-add-keymap-based-replacements myemacs-leader-map "j" "harpoon")
(define-key myemacs-leader-map (kbd "ja") '("harpoon add file" . harpoon-add-file))
(define-key myemacs-leader-map (kbd "jD") '("harpoon delete item" . harpoon-delete-item))
(define-key myemacs-leader-map (kbd "jc") '("harpoon clear" . harpoon-clear))
(define-key myemacs-leader-map (kbd "jf") '("harpoon toggle file" . harpoon-toggle-file))
(define-key general-override-mode-map (kbd "C-SPC") '("harpoon toggle quick menu" . harpoon-toggle-quick-menu))
