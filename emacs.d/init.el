;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(require 'package)

;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(unless (require 'use-package nil 'noerror)
  (package-install 'use-package))

(use-package web-mode
  :disabled
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; turn off annoying auto indent everything in web mode
  (setq web-mode-enable-auto-indentation nil))

(use-package flycheck
  :ensure t
  :config
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (global-flycheck-mode))


(use-package projectile
  ;; :ensure t
  ;; :disabled
  :config
  ;; (projectile-global-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package js2-mode
  :disabled
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package tide
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  )

(use-package indium
  :disabled
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode)
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package org
  :ensure t
  :config
  ;; (require 'org)
  (setq org-agenda-files '("~/Dropbox/org"))
  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-default-notes-file (concat org-directory "~/Dropbox/notes.org"))
  (define-key global-map "\C-cc" 'org-capture)
  ;; list stuff
  (setq org-list-allow-alphabetical t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  ;; (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  ;; (setq org-latex-listings 'minted)
  ;; (setq org-latex-listings-options '(("breaklines" "true")))
  ;; (setq org-latex-listings 'minted
  ;;       org-latex-packages-alist '(("" "minted"))
  ;;       org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle "~/.emacs.d/org-style.css" path)))
        (setq org-html-head-include-default-style t)
        (setq org-html-head (concat
                             "<style type=\"text/css\">\n"
                             "<!--/*--><![CDATA[/*><!--*/\n"
                             (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
                             "/*]]>*/-->\n"
                             "</style>\n")))))

  ;; uncomment to add style.css to html export
  ;; (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

  (add-hook 'org-mode-hook (lambda () (org-indent-mode t)))
  )

(use-package org-bullets
  :ensure t
  :config
  ;; use org-bullets-mode for utf8 symbols as org bullets
  ;; make available "org-bullet-face" such that I can control the font size individually
  (setq org-bullets-face-name (quote org-bullet-face))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
  ;; (setq org-bullets-bullet-list '("@" "*" "%" "-" "!"))
                                        ; org ellipsis options, other than the default Go to Node...
  ;; not supported in common font, but supported in Symbola (my fall-back font) ⬎, ⤷, ⤵
  ;; (setq org-ellipsis "↝")
  (put 'narrow-to-region 'disabled nil))

(use-package tuareg
  :disabled
  :config
  (push "<SHARE_DIR>/emacs/site-lisp" load-path) ; directory containing merlin.el
  ;; (setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
  (autoload 'merlin-mode "merlin" "Merlin mode" t)
  (add-hook 'tuareg-mode-hook 'merlin-mode))

;; ;; utop from opam
;; (setq utop-command "opam config exec -- utop -emacs")

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  ;; Set the banner
  (setq dashboard-startup-banner 3)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;;; "path/to/your/image.png" which displays whatever image you would prefer

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5) ;; need projectile
                          (agenda . 5) ;; need org mode
                          ;; (registers . 5)
                          ))
  (setq show-week-agenda-p t))

;;   htmlize            20180412.1944 installed             Convert buffer text and decorations to HTML.
;;   origami            20180101.1553 installed             Flexible text folding

(use-package reason-mode
  :disabled
  )

(use-package scala-mode
  ;; :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package nim-mode
  :ensure f
  :hook ((nim-mode . company-mode)
         (nim-mode . indent-guide-mode)))

(defun my-cc-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'access-label '-)
  (c-set-offset 'case-label '+)
  (c-set-offset 'topmost-intro '0)
  (c-set-offset 'inclass '++)
  (c-set-offset 'brace-list-entry '+))
(add-hook 'c-mode-common-hook 'my-cc-hook)
(add-hook 'c-mode-common-hook 'company-mode)

(use-package indent-guide
  :ensure f
  :config
  ;; (set-face-foreground 'indent-guide-face "dimgray")
  (setq indent-guide-delay 0)
  (setq indent-guide-char "|"))

(defun setup-company ()
  (interactive)
  (require 'color)
  
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package company
  :ensure t
  :hook (company-mode . setup-company)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward))

(use-package rust-mode
  ; https://github.com/rust-lang/rust-mode
  :ensure f
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
  ; enable format on save
  ;; (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))
  

;; no tool bar and no scroll bar
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)
;; no tabs
(setq-default indent-tabs-mode nil)


;; smarter-move-beginning-of-line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; cycle through windows commands
;; (global-set-key (kbd "C-<tab>") (lambda ()
;;                                 (interactive)
;;                                 (other-window 1)))
;; (global-set-key (kbd "C-S-<tab>") (lambda ()
;;                                 (interactive)
;;                                 (other-window -1)))
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; todo add to origami config
(global-set-key (kbd "<backtab>") 'origami-toggle-node)

;; Windmove keybindings
;; (global-set-key (kbd "C-<tab> <left>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-left)))
;; (global-set-key (kbd "C-<tab> <right>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-right)))
;; (global-set-key (kbd "C-<tab> <up>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-up)))
;; (global-set-key (kbd "C-<tab> <down>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-down)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" default))
 '(package-selected-packages
   '(clues-theme lsp-mode rust-mode ag scala-mode true indent-guide swiper company-mode nim-mode projectile org-bullets f dashboard magit yasnippet flycheck use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#36f948c24f39"))))
 '(company-scrollbar-fg ((t (:background "#2c7c3ae1401c"))))
 '(company-tooltip ((t (:inherit default :background "#2631328d370b"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
