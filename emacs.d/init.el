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


;; turn off line wrap
;; (set-default 'truncate-lines t)

(use-package web-mode
  :no-require t
  :ensure f
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
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-attr-indent-offset 4))

(use-package flycheck
  :ensure f
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
  :ensure t
  :config
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))

(use-package js2-mode
  :no-require t
  :ensure f
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js-switch-indent-offset 4)
  (setq js2-indent-switch-body t))

(use-package python-mode
  :ensure f
  :hook (python-mode . company-mode))

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

(use-package tide
  :ensure f
  ;; :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . company-mode))
  :config
  ;; (setq tide-tsserver-executable "/Users/ssu/.nvm/versions/node/v10.16.3/lib/node_modules/typescript/bin/tsserver")
  (setq tide-tsserver-executable "/Users/ssu/immuta/bodata/service/node_modules/typescript/lib/tsserver.js")
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  ;; (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  ;; (company-mode +1)
  )

(defun setup-company ()
  (interactive)
  (require 'color)
  
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(use-package company
  :ensure f
  :hook (company-mode . setup-company)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering))

(defun ivy-exit-and-do (func)
  '(lambda () (progn (func) (ivy-done)))
  )

(use-package swiper
  :ensure t
  :config
  ;; (global-set-key (kbd "C-S-s") 'swiper)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  (setq ivy-wrap t)
  ;; (substitute-key-definition 'ivy-forward-char (ivy-exit-and-do 'forward-char) ivy-minibuffer-map)
  )

(use-package ag
  :config
  (setq ag-reuse-buffers 't)
  (setq ag-arguments '("--smart-case" "--stats" "-p" "/Users/ssu/immuta/bodata/.ignore")))

(use-package indium
  :no-require t
  :ensure f
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
  (setq org-agenda-files '("~/Dropbox/org"))
  ;; (setq org-directory "~/org")
  (global-set-key (kbd "C-c a") 'org-agenda)
  ;; org caputre stuff
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-refile-targets
        '((("~/immuta/notes/tasks.org") . (:level . 1))
          (("~/immuta/immuta-hadoop/notes.org") . (:level . 2))
          (nil . (:level . 1))))
           
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
  (setq org-startup-indented t)
  )

(use-package org-bullets
  :ensure f
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
  :no-require t
  :ensure f
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
  ;; "path/to/your/image.png" which displays whatever image you would prefer

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
  :no-require t
  :ensure f)

(use-package scala-mode
  :ensure f
  :interpreter
  ("scala" . scala-mode))

(use-package nim-mode
  :ensure f
  :hook (nim-mode . company-mode))

(use-package indent-guide
  :ensure f
  :hook ((nim-mode . indent-guide-mode)
         (python-mode . indent-guide-mode))
  :config
  ;; (set-face-foreground 'indent-guide-face "dimgray")
  (setq indent-guide-delay 0)
  (setq indent-guide-char "|"))

(use-package terraform-mode
  :no-require t)

(use-package cc-mode
  :config
  (setq c-basic-offset 4))

;; c-mode-common stuff
(defun my-cc-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'access-label '-)
  (c-set-offset 'case-label '+)
  (c-set-offset 'topmost-intro '0)
  (c-set-offset 'inclass '++)
  (c-set-offset 'brace-list-entry '+))
(add-hook 'c-mode-common-hook 'my-cc-hook)
(add-hook 'c-mode-common-hook 'company-mode)
 

(use-package rust-mode
  ;; https://github.com/rust-lang/rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
  ;; enable format on save
  ;; (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))


;; try to get language server working
;; (use-package lsp-mode
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;; (rust-mode . lsp)
;;          (scala-mode . lsp)
;;          ;; if you want which-key integration
;;          ;; (lsp-mode . lsp-enable-which-key-integration)
;;          )
;;   :commands lsp
;;   :config
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l", "s-l")
;;   (setq lsp-keymap-prefix "C-c l"))

;; scala lsp backend
;; (use-package lsp-metals
;;   :config (setq lsp-metals-treeview-show-when-views-received nil))

;; no tool bar and no scroll bar
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)
;; no tabs
(setq-default indent-tabs-mode nil)


;; company mode in minibuffer from: https://gist.github.com/Bad-ptr/7787596
;; TODO: need to fix issue where no matches throws error
;; TODO: need to make it so <enter> runs command right away
;; TODO: need to filter out everything thats not a function
(with-eval-after-load "company-autoloads"
  (global-company-mode 1)

  (setq company-tooltip-limit 20
        company-minimum-prefix-length 1
        company-echo-delay 0
        company-begin-commands '(self-insert-command
                                 c-electric-lt-gt c-electric-colon
                                 completion-separator-self-insert-command)
        company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-align-annotations t)

  (defvar-local company-col-offset 0 "Horisontal tooltip offset.")
  (defvar-local company-row-offset 0 "Vertical tooltip offset.")

  (defun company--posn-col-row (posn)
    (let ((col (car (posn-col-row posn)))
          ;; `posn-col-row' doesn't work well with lines of different height.
          ;; `posn-actual-col-row' doesn't handle multiple-width characters.
          (row (cdr (posn-actual-col-row posn))))
      (when (and header-line-format (version< emacs-version "24.3.93.3"))
        ;; http://debbugs.gnu.org/18384
        (cl-decf row))
      (cons (+ col (window-hscroll) company-col-offset) (+ row company-row-offset))))

  (defun company-elisp-minibuffer (command &optional arg &rest ignored)
    "`company-mode' completion back-end for Emacs Lisp in the minibuffer."
    (interactive (list 'interactive))
    (case command
      ('prefix (and (minibufferp)
                    (case company-minibuffer-mode
                      ('execute-extended-command (company-grab-symbol))
                      (t (company-capf `prefix)))))
      ('candidates
       (case company-minibuffer-mode
         ('execute-extended-command (all-completions arg obarray 'commandp))
         (t nil)))
      ('meta "poop")
      ))

  (defun minibuffer-company ()
    (unless company-mode
      (when (and global-company-mode (or (eq this-command #'execute-extended-command)
                                         (eq this-command #'eval-expression)))

        (setq-local company-minibuffer-mode this-command)

        (setq-local completion-at-point-functions
                    (list (if (fboundp 'elisp-completion-at-point)
                              #'elisp-completion-at-point
                            #'lisp-completion-at-point) t))

        (setq-local company-show-numbers nil)
        (setq-local company-backends '((company-elisp-minibuffer company-capf)))
        (setq-local company-tooltip-limit 8)
        (setq-local company-col-offset 1)
        (setq-local company-row-offset 0)
        (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                        company-preview-if-just-one-frontend))

        (company-mode 1)
        (when (eq this-command #'execute-extended-command)
          (company-complete)
          ))
      ))

  ;; (add-hook 'minibuffer-setup-hook #'minibuffer-company)
  ;;(remove-hook 'minibuffer-setup-hook #'minibuffer-company)
  ;;(add-hook 'eval-expression-minibuffer-setup-hook #'minibuffer-company)
  ;; (with-eval-after-load "company-flx-autoloads"
  ;; (company-flx-mode))
  )


;; work commands
(defun bomocha ()
  (interactive)
  (async-shell-command (format "cd ~/immuta/bodata/service; npm run mocha -- %s" (shell-quote-argument buffer-file-name)) "*bomocha output*")
  ;; (start-process "bomocha" "*bomocha output*" "~/immuta/bodata/service/node_modules/mocha/bin/mocha" "--timeout" "10000" (shell-quote-argument buffer-file-name))
  ;; (start-process-shell-command "bomocha" "*bomocha output*" (format "~/immuta/bodata/service/node_modules/mocha/bin/mocha --timeout 10000 %s" (shell-quote-argument buffer-file-name)))
  ;; (switch-to-buffer "*bomocha output*")
  )

(defun bomochadb ()
  (interactive)
  (async-shell-command (format "cd ~/immuta/bodata/service; ~/immuta/bodata/service/node_modules/mocha/bin/mocha --timeout 10000 --inspect %s" (shell-quote-argument buffer-file-name)) "*bomocha output*")
  (indium-maybe-quit)
  (unless (indium-client-process-live-p)
    (let ((dir (expand-file-name default-directory)))
      (indium-client-start
       (lambda ()
	 (indium-client-list-configurations
	  dir
	  (lambda (configurations)
	    (indium-client-connect dir "bodata")))))))
  )


;; TODO: make more flexible commands using project-project-root
;; (defun projectile-run-shell-command-in-root ()
;;   "Invoke `shell-command' in the project's root."
;;   (interactive)
;;   (projectile-with-default-dir (projectile-project-root)
;;     (call-interactively 'shell-command)))

;; (defun projectile-run-async-shell-command-in-root ()
;;   "Invoke `async-shell-command' in the project's root."
;;   (interactive)
;;   (projectile-with-default-dir (projectile-project-root)
;;     (call-interactively 'async-shell-command)))


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

(use-package origami
  :ensure f
  :config
  (global-set-key (kbd "<backtab>") 'origami-toggle-node)
  (setq global-origami-mode t))

;; ;; Windmove keybindings
;; (global-set-key (kbd "S-s-<left>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-left)))
;; (global-set-key (kbd "S-s-<right>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-right)))
;; (global-set-key (kbd "S-s-<up>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-up)))
;; (global-set-key (kbd "S-s-<down>") (lambda ()
;;                                 (interactive)
;;                                 (windmove-down)))


;; replace-string binding
(global-set-key (kbd "C-c r") 'replace-string)

;; split window ish
(setq split-width-threshold 200)
(setq split-height-threshold nil)

;; save session
;; (desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#333" "#ff5f87" "#3affa3" "#f6df92" "#b2baf6" "#c350ff" "#5af2ee" "#ccc"])
 '(custom-enabled-themes '(grandshell))
 '(custom-safe-themes
   '("e9740103f6ae2cbc97fef889b85b1c51b4d4a2d95c2b398b57a1842d14d96304" "2593436c53c59d650c8e3b5337a45f0e1542b1ba46ce8956861316e860b145a0" "38143778a2b0b81fb7c7d0e286e5b0e27cd6b2ba1c3b0aa4efbc33e6ac2ed482" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ed91d4e59412defda16b551eb705213773531f30eb95b69319ecd142fab118ca" "08141ce5483bc173c3503d9e3517fca2fb3229293c87dc05d49c4f3f5625e1df" default))
 '(fringe-mode 10 nil (fringe))
 '(linum-format " %6d ")
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(package-selected-packages
   '(lsp-metals sbt-mode dash ## rust-mode yaml-mode zweilight-theme terraform-mode indent-guide nim-mode ag rainbow-delimiters swiper dakrone-theme grandshell-theme f company emojify tide haskell-mode ample-theme restclient restclient-test scala-mode reason-mode indium web-mode use-package htmlize dashboard utop tuareg monokai-theme magit org-bullets org-link-minor-mode origami projectile flycheck yasnippet js2-mode))
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(window-min-width 10))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#ffffffffffff"))))
 '(company-scrollbar-fg ((t (:background "#ffffffffffff"))))
 '(company-tooltip ((t (:inherit default :background "#ffffffffffff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
