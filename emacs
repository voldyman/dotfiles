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

;; Lines and columns
(global-linum-mode t)
(column-number-mode t)
(show-paren-mode t)
(size-indication-mode t)

;; Code folding
(require 'vimish-fold)
(global-set-key (kbd "C-c C-f") 'vimish-fold)
(global-set-key (kbd "C-c C-u") 'vimish-fold-unfold)
(global-set-key (kbd "C-c C-r") 'vimish-fold-delete)
(global-set-key (kbd "C-c C-t") 'vimish-fold-toggle)
(global-set-key (kbd "C-c C-n") 'vimish-fold-next-fold)
(global-set-key (kbd "C-c C-p") 'vimish-fold-previous-fold)

;; Uniquify filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; use ido vertical
(ido-vertical-mode t)

;; C-n/p is more intuitive in vertical layout
(defun ido-define-keys () 
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; my theme
;;(load-theme 'tangotango t)
;;(load-theme 'sanityinc-tomorrow-eighties t)
(load-theme 'aurora t)
(setq linum-format "%4i\u2502")

;; Enable Auto-Complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)
(auto-complete-mode t)

;; Go mode essentials
(require 'go-mode)
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

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
(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; y/n is easier than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; redo+ keybinding
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

;; highlight current line
(global-hl-line-mode)
(set-face-background hl-line-face "#393939")

;; Enabel recent files and disable backup and autosave file
(recentf-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Vala mode
(require 'cl) ; workround for cl mode bug for vala
(autoload 'vala-mode "vala-mode.el" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

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
(add-to-list 'default-frame-alist '(font . "Meslo LG S-10"))


;; Add ELPA package repository
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(custom-safe-themes
   (quote
    ("3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" "81a4b3d3751940b01617381397f31168420252e50cc9600cc0fc168ff4819ced" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(fci-rule-color "#515151")
 '(package-selected-packages
   (quote
    (zonokai-theme aurora-theme thrift railscasts-theme cmake-ide zenburn-theme vimish-fold ac-html ac-js2 ac-php ac-python auto-complete-c-headers auto-complete-clang darkmine-theme zen-and-art-theme idomenu ido-vertical-mode zeal-at-point cmake-mode web-mode auto-complete concurrent ctable deferred go-mode popup yasnippet vala-snippets vala-mode redo+ python-environment php-mode neotree markdown-mode go-eldoc go-autocomplete epc color-theme-sanityinc-tomorrow)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
