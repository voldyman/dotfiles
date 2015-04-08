;; Add ELPA package repository
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


(defvar required-packages
      '(auto-complete
        color-theme-solarized zenburn-theme
        go-mode go-eldoc
        clojure-mode js2-mode
        markdown-mode php-mode
        cider idomenu 
        neotree
        vala-mode
        web-mode
        browser-kill-ring
        anzu ;; show number of search matches
        ido-vertical-mode
        paredit  jedi
        undo-tree-mode redo+))

;; Installs missing packages
(defun install-missing-packages ()
  "Installs required packages that are missing"
  (interactive)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        required-packages)
  (message "Installed all missing packages!"))

