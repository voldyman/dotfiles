;; activate all the packages (in particular autoloads)
(package-initialize)

;; No Splash Screen
(setq inhibit-splash-screen t)

;; Deactivate menu-bar, tool-bar and scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Indentation
(setq-default indent-tabs-mode nil)
(setq indent-tab-mode nil)
(setq-default tab-width 4)
(setq c-basic-indent 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Enable utf-8 in term mode
(set-terminal-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)

;; delete selected text
(delete-selection-mode t)

;; Fixme mode
(use-package fixme-mode
  :config
  (fixme-mode t))

;; Lines and columns
(global-linum-mode t)
(column-number-mode t)
(show-paren-mode t)
(size-indication-mode t)

;; User powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; mac specfic stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Move text up down
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

;; Automatically insert quotes and brackets pairs
(electric-pair-mode t)

;; Use C-ret for rectangular selection
(cua-selection-mode t)

;; Automatically reload file when changed on disk
(global-auto-revert-mode)
(global-discover-mode t)

;; Run zeal at point
(global-set-key (kbd "C-c a") 'zeal-at-point)

;; Code folding
(use-package vimish-fold
  :ensure t
  :defer t
  :init
  :bind
  (("C-c C-f" . vimish-fold)
   ("C-c C-u" . vimish-fold-unfold)
   ("C-c C-r" . vimish-fold-delete)
   ("C-c C-t" . vimish-fold-toggle)
   ("C-c C-n" . vimish-fold-next-fold)
   ("C-c C-p" . vimish-fold-previous-fold))
  )

;; Uniquify filenames
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  )

;; Enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; use ido vertical
(ido-vertical-mode t)

(defun ido-define-keys () 
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(use-package smex
  :config
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
  :bind             ; when Smex is auto-initialized on its first run.
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)) 

;; my theme
;;(load-theme 'tangotango t)
;;(load-theme 'sanityinc-tomorrow-eighties t)
;;(load-theme 'aurora t)
;;(color-theme-monokai)
;;(load-theme 'ample t)
;;(load-theme 'material t)
;;(load-theme 'badger t)
(load-theme 'spacemacs-dark t)

(setq linum-format "%4i\u2502")

;; Add GOPATH/bin to PATH
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

;; Add brew path to PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;; CC mode
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior


;; neotree keybinding
(use-package neotree
  :bind
  ("C-x t" . neotree-toggle))

;; y/n is easier than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; redo+ keybinding
(use-package redo+
  :bind
  ("C-?" . redo))

;; highlight current line
;;(global-hl-line-mode)
;;(set-face-background hl-line-face "#393939")
;;(powerline-default-theme)

;; Enabel recent files and disable backup and autosave file
(recentf-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Vala mode
(use-package cl ; workround for cl mode bug for vala
  :config
  (autoload 'vala-mode "vala-mode.el" "Major mode for editing Vala code." t)
  (add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
  (add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
  (add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
  (add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8)))

;; C++ Mode
(c-set-offset 'access-label '-2)
(c-set-offset 'inclass '4)
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil)            ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; Scroll window under mouse
(setq scroll-step 1)                                ;; Keyboard scroll one line at a time
(setq scroll-margin 4)                              ;; Always 4 lines above/below cursor


;; highlight TODO|FIXME|BUG in comments
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; better defaults (?)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Go fullscreen!
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (global-set-key (kbd "<f11>") 'toggle-fullscreen))))

;; Fix all indentation (I love this so much)
(defun fix-indentation ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!"))))

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active"))
    (insert (shell-command-to-string "xsel -o -b"))))

(defun clear-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (insert (shell-command-to-string "xsel -b  --clear"))))

;; copy/paste from clipboard in emacs -nw
(global-set-key [f7] 'clear-clipboard)
(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)

;; Some shortcuts
(global-set-key [f12] 'delete-trailing-whitespace)
(global-set-key [f10] 'fix-indentation)

;; Clear the eshell
(defun clear-eshell ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/clear ()
  (clear-eshell))

(define-minor-mode eshell-cust-mode
  "Get your foos in the right places."
  :lighter " cust"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l") 'clear-eshell)
            map))

(add-hook 'eshell-mode-hook (lambda ()
                              (eshell-cust-mode)))


;; Use C-tab and C-S-tab to switch buffers
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] (lambda() ; use C-S-iso-lefttab because thats what emacs wants.
                                    (interactive)
                                    (other-window -1)))

;; set font for all windows
;;(add-to-list 'default-frame-alist '(font . "Monaco-10"))
;;(add-to-list 'default-frame-alist '(font . "Meslo LG S-10"))
(set-default-font "Source Code Pro-14")

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-go :ensure t :defer t)
  (use-package company-irony :ensure t :defer t)
  (use-package company-irony-c-headers :ensure t :defer t)

  (setq company-idle-delay              0.3
        company-minimum-prefix-length   2
        company-begin-commands          '(self-insert-command)
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
        company-echo-delay              0
        company-backends                '((company-elisp
                                           company-gtags)))

  :bind ("C-;" . company-complete-common))

;; == irony-mode ==
(use-package irony
  :ensure t
  :ensure company-irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  :config
  (custom-set-variables
   '(irony-additional-clang-options
     '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1"
       "-std=c++11")))
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-to-list 'company-backends
               'company-irony 'company-irony-c-headers)
  (irony-eldoc)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;;  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package irony-eldoc
  :commands irony-eldoc
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package flycheck-irony
  :ensure t
  :commands flycheck-irony-setup
  :init
  (add-hook 'irony-mode-hook 'flycheck-irony-setup))

;; == ycmd ==
(use-package ycmd
  :ensure t
  :init
  (set-variable 'ycmd-global-config
                "~/.emacs.d/ycmd_extra_config.py")
  (set-variable 'ycmd-server-command
                '("python2.7" "/Users/voldyman/Code/ycmd/ycmd"))

  :config
  (with-eval-after-load 'company
    '(push 'company-ycmd company-backends))
  (add-hook 'after-init-hook #'global-ycmd-mode))

(use-package company-ycmd
  :init
  (company-ycmd-setup))

;; Go mode essentials
(use-package go-mode
  :ensure t
  :defer t
  :bind
  ("M-." . godef-jump)
  :config
  (defun go-mode-setup ()
    (set (make-local-variable 'company-backends) '(company-go))
    (setq gofmt-command "goimports")
    (go-eldoc-setup)
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'go-mode-setup))


;; Add ELPA package repository
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("770181eda0f652ef9293e8db103a7e5ca629c516ca33dfa4709e2c8a0e7120f3" "ea489f6710a3da0738e7dbdfc124df06a4e3ae82f191ce66c2af3e0a15e99b90" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(irony-additional-clang-options
   (quote
    ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1" "-std=c++11"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
