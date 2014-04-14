;; Config file by Akshay Shekher
;; most of the lines are borrowed from the internet

;; activate all the packages (in particular autoloads)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elpa/")

;; No Splash Screen
(setq inhibit-splash-screen t)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tab-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; use ido vertical
(ido-vertical-mode t)

;; y/n is easier than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; utf8 FTW!!
(prefer-coding-system 'utf-8)

;; auto-complete using company mode
(company-mode t)

;; Lines and columns
(global-linum-mode 1)
(column-number-mode 1)

;; Enabel recent files and disable backup and autosave file
(recentf-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; better defaults (?)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))


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


;; Some shortcuts
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f10>") 'fix-indentation)

;; my theme
(load-theme 'tango-dark t)

;; ruby-mode
(require 'ruby-mode)

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(defun ruby-eval-buffer () (interactive)
    "Evaluate the buffer with ruby."
    (shell-command-on-region (point-min) (point-max) "ruby"))
 
(defun ruby-load-current-buffer ()
   "Load current buffer's Ruby file into the inferior Ruby process.
    Saving the current buffer first if needed."
   (interactive)
   (let ((buffer (current-buffer)))
     (or (eq major-mode 'ruby-mode)
       (error "Not ruby mode"))
     (save-buffer buffer)
     (comint-send-string
       (ruby-proc)
       (concat "(load '" (buffer-file-name buffer) "'\)\n")
     )
   )
)

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
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "a3d519ee30c0aa4b45a277ae41c4fa1ae80e52f04098a2654979b1ab859ab0bf" "d9639ebed5f3709f47b53e4bb8eea98a11455ab9336039cf06e9695a0233d5fb" default)))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Use C-tab and C-S-tab to switch buffers
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] (lambda() ; use C-S-iso-lefttab because thats what emacs wants.
                            (interactive)
                            (other-window -1)))

;; set font for all windows
(add-to-list 'default-frame-alist '(font . "Monaco-10"))
