;; Config file by Akshay Shekher
;; most of the lines are borrowed from the internet

;; activate all the packages (in particular autoloads)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elpa/")

;; No Splash Screen
(setq inhibit-splash-screen t)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq indent-tab-mode nil)
(setq-default tab-width 4)
(setq c-basic-indent 2)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Enable Auto-Complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)
(auto-complete-mode t)

;; Enable utf-8 in term mode
(set-terminal-coding-system 'utf-8-unix)

;; Enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; delete selected text
(delete-selection-mode t)

;; use ido vertical
(ido-vertical-mode t)

;; neotree keybinding
(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; redo+ keybinding
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

;; y/n is easier than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; utf8 FTW!!
(prefer-coding-system 'utf-8)

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

;; Lines and columns
(global-linum-mode 1)
(column-number-mode 1)

;; Enabel recent files and disable backup and autosave file
(recentf-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Vala mode
(autoload 'vala-mode "vala-mode.el" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; PHP Mode
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;; C++ Mode
(c-set-offset 'access-label '-2)
(c-set-offset 'inclass '4)
(setq c-default-style "bsd"
      c-basic-offset 2)
;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil)            ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; Scroll window under mouse
(setq scroll-step 1)                                ;; Keyboard scroll one line at a time
(setq scroll-margin 4)                              ;; Always 4 lines above/below cursor

(column-number-mode 1)
(show-paren-mode 1)
(size-indication-mode 1)

;; Go fullscreen!
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (global-set-key (kbd "<f11>") 'toggle-fullscreen))))

;; Deactivate menu-bar, tool-bar and scroll-bar
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

;; copy/paste from clipboard in emacs -nw
(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)

;; Some shortcuts
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f10>") 'fix-indentation)

;; my theme
   ;(load-theme 'tango-dark t)
(load-theme 'sanityinc-tomorrow-eighties t)
(setq linum-format "%4d\u2502")

;; go mode additions
(add-hook 'go-mode-hook 'go-eldoc-setup)

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

;; Add ELPA package repository
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Set go path
(setenv "GOPATH" "/home/voldyman/code/go")

;; Auto enable paredit when in clojure-mode
(add-hook 'clojure-mode-hook (lambda ()
                               (paredit-mode)))

(add-hook 'eshell (lambda ()
                    (eshell-cust-mode)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "a31c86c0a9ba5d06480b02bb912ae58753e09f13edeb07af8927d67c3bb94d68" "4bfdf53bd55a41fbc2e48b8916d764f1e5bac03f74a264ca091cb79bd20080c3" "a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "a3d519ee30c0aa4b45a277ae41c4fa1ae80e52f04098a2654979b1ab859ab0bf" "d9639ebed5f3709f47b53e4bb8eea98a11455ab9336039cf06e9695a0233d5fb" default)))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Meslo LG S")))))

;; Use C-tab and C-S-tab to switch buffers
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] (lambda() ; use C-S-iso-lefttab because thats what emacs wants.
                                    (interactive)
                                    (other-window -1)))

;; set font for all windows
;;(add-to-list 'default-frame-alist '(font . "Monaco-10"))
(add-to-list 'default-frame-alist '(font . "Meslo LG S-10"))

(define-abbrev-table 'global-abbrev-table '(
    ("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("theta" "θ")
    ("inf" "∞")
    ("lambda" "λ")

    ("ar1" "→")
    ("ar2" "⇒")
    ))

(abbrev-mode t)
