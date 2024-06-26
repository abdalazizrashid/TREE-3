
;; Garbage collect after startup
(add-hook 'after-init-hook #'garbage-collect t)

;; Load use packages
(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (setq package-enable-at-startup nil
        load-path
        (append (list (emacs-path "use-package"))
                (delete-dups load-path)
                (list (emacs-path "lisp")))))

(require 'use-package)
;; Set use-package to always ensure packages are installed
(setq use-package-always-ensure t)

;; (setq use-package-verbose init-file-debug
;;       use-package-expand-minimally (not init-file-debug)
;;       use-package-compute-statistics t
;;       debug-on-error init-file-debug)

;; Define the “data environment” for this instance of Emacs

(defconst emacs-environment (or (getenv "NIX_MYENV_NAME") "default"))

(defconst emacs-data-suffix
  (cond ((string= "emacsERC" emacs-environment) "alt")
        ((string-match "emacs2[6789]\\(.+\\)$" emacs-environment)
         (match-string 1 emacs-environment))))

(defconst alternate-emacs (string= emacs-data-suffix "alt"))

(defconst user-data-directory
  (emacs-path (if emacs-data-suffix
                  (format "data-%s" emacs-data-suffix)
                "data")))

;; (defun user-data (
;;   (expand-file-name dir user-data-directory)))

;; Font setup
;; https://slumpy.org/blog/2016-01-11-proper-way-to-setup-fonts-in-emacs/
(defun my/setup-fonts ()
  (interactive)
  (set-face-font 'default "Berkeley Mono-15")
  (set-fontset-font t 'hebrew (font-spec :name "Berkeley Mono-15"))
)
;; (add-to-list 'default-frame-alist '(font . "IBM Plex Mono" ))
;; (set-face-attribute 'default t :font "IBM Plex Mono")
(use-package modus-themes
  :ensure t
  :custom
  ;; Load the theme of your choice:
  (load-theme 'modus-operandi-tinted :no-confirm)
  (modus-themes-select 'modus-operandi-tinted))

;; Using `use-package` to configure emacs, here emacs is a pseudo
;; package
(use-package emacs
  :bind* (
          ("M-o" . ace-window)
          ("C-x <C-m>" . execute-extended-command)
          ("C-c <C-m>" . execute-extended-command) ;; Sloppy version
	  ("C-h h" . nil) ;; Disable the hello page
          )
  :custom
  (user-full-name "A.R.M")


  ;; Settings for the Cocoa port
  (ns-alternate-modifier 'super)
  (ns-command-modifier 'meta)
  (ns-function-modifier 'hyper)
  (ns-right-alternate-modifier 'super)

  ;; Settings for the Emacs Mac-port
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super)
  (mac-pass-command-to-system nil)

  (frame-title-format
   '(:eval
     (concat
      (if buffer-file-name default-directory "%b")
      "    "
      (number-to-string
       (cdr
        (assq 'width
              (frame-parameters))))
      "x"
      (number-to-string
       (cdr
        (assq 'height
              (frame-parameters)))))))

  (completion-cycle-threshold 7)
  (completion-ignored-extensions
   '(".a"
     ".aux"
     ".bbl"
     ".bin"
     ".elc"
     ".git/"
     ".o"
     ".pyc"
     ".pyo"
     ".so"
     ".toc"
     "~"))

  ;; startup.el
  (auto-save-list-file-prefix (user-data "auto-save-list/.saves-"))
  (inhibit-startup-echo-area-message "aziz")
  (inhibit-startup-screen t)
  (initial-buffer-choice t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (user-mail-address "abdalaziz.rashid@outlook.com")

  ;; files.el
  (auto-save-file-name-transforms '(("\\`/[^/]*:.*" "~/.emacs.d/auto-saves/" t)))
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
  (delete-old-versions t)
  (directory-abbrev-alist
   '(("\\`/org" . "~/org")
     ("\\`/home-manager" . "~/.config/home-manager")))
  (directory-free-space-args "-kh")
  (large-file-warning-threshold nil)
  (save-abbrevs 'silently)
  (trash-directory "~/.Trash")
  (version-control t)

  ;; bytecomp.el
  (byte-compile-verbose nil)

  ;; scroll-bar.el
  (scroll-bar-mode nil)

  ;; paren.el
  (show-paren-delay 0)

  ;; window.el
  (same-window-buffer-names
   '("*eshell*"
     "*shell*"
     "*mail*"
     "*inferior-lisp*"
     "*ielm*"
     "*scheme*"))
  (switch-to-buffer-preserve-window-point t)

  ;; warnings.el
  (warning-minimum-log-level :error)

  ;; frame.el
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))


  ;; mwheel.el
  ;; TODO: disable keybindings to  mouse-wheel-global-text-scale
  ;; and mouse-wheel-text-scale
  (global-set-key (kbd "<C-wheel-up>") nil)
  (global-set-key (kbd "<C-wheel-down>") nil)

  
  :init
  (setq disabled-command-function nil) ;; enable all commands
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-region '(bg-only)
	modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
  
  :bind ("<f5>" . modus-themes-toggle)

  :config  
  ;; Setup font
  (advice-add 'server-create-window-system-frame
              :after 'my/setup-fonts)
  (advice-add 'server-create-tty-frame
              :after 'my/setup-fonts)
  (unless (daemonp) (my/setup-fonts))

  ;; Enable line numbers only with programing modes
  ;; (add-hook 'prog-mode-hook (lambda () (
  ;;           display-line-numbers-mode
  ;;           (setq-default display-line-numbers-type 'relative))))

  ;; This is an ugly hack the fix is upstream but hasn't been merge yet
  ;; https://github.com/doomemacs/doomemacs/issues/7532
  (add-hook 'after-init-hook (lambda () 
                             (tool-bar-mode 1) 
                             (tool-bar-mode 0)))
  (defun my-toggle-toolbar (frame)
    "Toggle tool-bar-mode on then off when a new frame is created."
    (with-selected-frame frame
      (tool-bar-mode 1)
      (tool-bar-mode 0)))

  (add-hook 'after-make-frame-functions 'my-toggle-toolbar)
  )


(use-package package
  :custom
  (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

  (defun validate-package-urls (urls)
    "Validate the given package repository URLs."
    (dolist (url urls)
      (unless (string-match "\\`https?:" (cdr url))
	(error "Invalid URL: %s" (cdr url)))))

  (validate-package-urls package-archives)

  (package-initialize))



(use-package imenu
  :config
  (setq imenu-auto-rescan t)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^\\s-*(use-package\\s-+\\(\\_<.+?\\_>\\)" 1))))))

;;;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-display-maybe-show-predicates t)
  (ibuffer-expert t)
  (ibuffer-formats
   '((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-maybe-show-regexps nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "\\*magit")
        (name . "magit-")
        (name . "git-monitor")))
      ("Coq"
       (or
        (mode . coq-mode)
        (name . "\\<coq\\>")
        (name . "_CoqProject")))
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Haskell"
       (or
        (mode . haskell-mode)
        (mode . haskell-cabal-mode)
        (mode . haskell-literate-mode)))
      ("Rust"
       (or
        (mode . rust-mode)
        (mode . cargo-mode)
        (name . "\\*Cargo")
        (name . "^\\*rls\\(::stderr\\)?\\*")
        (name . "eglot")))
      ("Nix"
       (mode . nix-mode))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")
        (name . "^\\*\\(sent\\|unsent\\|fetch\\)")
        (name . "^ \\*\\(nnimap\\|nntp\\|nnmail\\|gnus\\|server\\|mm\\*\\)")
        (name . "\\(Original Article\\|canonical address\\|extract address\\)")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (name . "^diary$")
        (mode . org-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)"))))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-shrink-to-minimum-size t t)
  (ibuffer-use-other-window t)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package dired
  :ensure nil
  :config
  ;; (when (string= system-type "darwin")
  ;;   (setq dired-use-ls-dired t
  ;;         insert-directory-program "/usr/local/bin/gls"
  ;;         dired-listing-switches "-aBhl --group-directories-first")))
)
(use-package dired-x
  :ensure nil
  :after dired
  :config
  (add-hook 'dired-mode-hook #'dired-omit-mode))

  

;; Project management
;;;; projectile
(use-package projectile
  :init
  (projectile-mode t)
  (require 'tramp)
  :bind
  ((:map projectile-mode-map
         ("s-p" . projectile-command-map))
         ("s-p v" . 'magit))
 
  :config
  
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-sort-order 'recentf)
  (setq projectile-git-use-fd t)
  (setq projectile-enable-caching t)
  (add-to-list 'projectile-other-file-alist '("ex" . ("html.heex" "html.leex")))
  (add-to-list 'projectile-other-file-alist '("html.heex" . ("ex")))
  (add-to-list 'projectile-other-file-alist '("html.leex" . ("ex")))
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action #'projectile-dired))


 

;; Terminal emulator
;;;; vterm
(use-package vterm
  :defer t)

(use-package elixir-mode)

;; (use-package treesit
;;   :mode (("\\.tsx\\'" . tsx-ts-mode))
;;   :preface
;;   ;; Treesitter remapping
;;   (dolist (mapping
;;          '((python-mode . python-ts-mode)
;; 	   (elixir-mode . elixir-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (typescript-mode . typescript-ts-mode)
;;            (js2-mode . js-ts-mode)
;;            (bash-mode . bash-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (json-mode . json-ts-mode)
;;            (js-json-mode . json-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))

;;   :config
;;   (use-package combobulate
;;     :preface
;;     (setq combobulate-key-prefix "C-c o")
;;     :hook
;;       ((python-ts-mode . combobulate-mode)
;;        (js-ts-mode . combobulate-mode)
;;        (html-ts-mode . combobulate-mode)
;;        (css-ts-mode . combobulate-mode)
;;        (yaml-ts-mode . combobulate-mode)
;;        (typescript-ts-mode . combobulate-mode)
;;        (json-ts-mode . combobulate-mode)
;;        (tsx-ts-mode . combobulate-mode))
;;       :init
;;       (
;; 	      (package-vc-install
;; 	       '(combobulate :url "https://github.com/mickeynp/combobulate")))))


;;;; eglot
(use-package eglot
  :commands eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc))
;;              ("C-c C-e" . eglot-rename)
;;              ("C-c C-o" . python-sort-imports)
;;              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . flyspell-prog-mode)
         (python-ts-mode . superword-mode)
         (python-ts-mode . hs-minor-mode)
         (python-ts-mode . (lambda () (set-fill-column 88)))
         (nix-ts-mode . eglot-ensure)
	 (tex-mode . eglot-ensure)
         ;; (prog-mode . (lambda ()
         ;;               (add-hook 'before-save-hook 'eglot-format nil t)))
	 )
 
  :config
  (add-to-list
   'eglot-server-programs
   '(nix-ts-mode
     . ("nix-shell" "-p" "nixd" "--run" "nixd")))
  (add-to-list
   'eglot-server-programs
   '((elixir-ts-mode heex-ts-mode)
     ;; TODO remove elixir package from runtime shell
     . ("nix-shell" "-p" "elixir-ls" "elixir" "--run" "elixir-ls")))
  
  (setq read-process-output-max (* 1024 1024))
  ;; (with-eval-after-load 'eglot
  ;;   (dolist (mode '((nix-mode . ("nixd"))))
  ;;     (add-to-list 'eglot-server-programs mode)))

  
  (add-hook 'eglot-managed-mode-hook
          #'(lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function
                                  eldoc-documentation-functions)))))
  (setq-default eglot-workspace-configuration '(
      (:pylsp . (:configurationSources ["flake8"] :plugins (
                 :pycodestyle (:enabled t) :mccabe (:enabled t)
                 :pyflakes (:enabled t) :flake8 (:enabled t
                 :maxLineLength 88) :ruff (:enabled t :lineLength 88)
                 :pydocstyle (:enabled t :convention "numpy") :yapf
                 (:enabled t) :autopep8 (:enabled :json-false) :black
                 (:enabled t :line_length 88 :cache_config t))))
  
      (:nixd
       (:nixpkgs
	(:expr "import <nixpkgs> { }"))))))
      

;; Manuals and Docs
;;;; info TODO: read and refactor
(use-package info
  :bind ("C-h C-i" . info-hlookup-symbol)
  :custom
  ;; (Info-default-directory-list (list (emacs-path "lisp/org-mode/doc")))
  (Info-fit-frame-flag nil)
  :autoload Info-goto-node
  :preface
  (eval-when-compile
    (defvar buffer-face-mode-face))

  (defun nix-read-environment (name)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents-literally
         (with-temp-buffer
           (insert-file-contents-literally
            (executable-find (concat "load-env-" name)))
           (and (re-search-forward "^source \\(.+\\)$" nil t)
                (match-string 1))))
        (and (or (re-search-forward "^  nativeBuildInputs=\"\\(.+?\\)\"" nil t)
                 (re-search-forward "^  buildInputs=\"\\(.+?\\)\"" nil t))
             (split-string (match-string 1))))))
  :init
  (defvar Info-directory-list
    (mapcar 'expand-file-name
            (append
             (mapcar (apply-partially #'expand-file-name "share/info")
                     (nix-read-environment emacs-environment))
             '("~/.local/share/info"
               "~/.nix-profile/share/info"
               "/etc/profiles/per-user/aziz/share/info/"
               "/run/current-system/sw/share/info/"
               ))))
  :config
  ;; (add-hook 'Info-mode-hook
  ;;           #'(lambda ()
  ;;                (setq buffer-face-mode-face '(:family "Arial"))
  ;;                (buffer-face-mode)
  ;;                (text-scale-adjust 1)
  ;; 		))
)


(use-package info-look :autoload info-lookup-add-help)

;; (use-package info-lookmore
;;   :enable nil
;;   :after info-look
;;   :config
;;   (info-lookmore-elisp-cl)
;;   (info-lookmore-elisp-userlast)
;;   (info-lookmore-elisp-gnus)
;;   (info-lookmore-apropos-elisp))

;; Completion

;;;; Orderless
;; (use-package orderless
;;   :disabled t
;;   :demand t
;;   :config
;;   (defun prefixes-for-separators (pattern _index _total)
;;     (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
;;       (cons 'orderless-prefixes pattern)))
;;   (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
;;   :custom
;;   (orderless-style-dispatchers
;;    '(orderless-affix-dispatch prefixes-for-separators)))

;;;; Helm

(use-package helm-projectile)
(use-package helm
  :demand t
  :config
  (require 'helm-source)
 
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x m") #'helm-M-x)
  (global-set-key (kbd "C-c m") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-c i n") #'helm-complete-file-name-at-point)
  (global-set-key (kbd "C-x i") #'helm-imenu)
  (global-set-key (kbd "C-x b") 'helm-mini)

  
  (setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t)

  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	(overlay-put ov 'window (selected-window))
	(overlay-put ov 'face
		     (let ((bg-color (face-background 'default nil)))
		       `(:background ,bg-color :foreground ,bg-color)))
	(setq-local cursor-type nil))))

  ;; (when (executable-find "ack-grep")
  ;; (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
  ;;       helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  (add-hook 'helm-minibuffer-set-up-hook
	    'spacemacs//helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)


  
  (setq helm-completion-style 'helm)
  (setq helm-M-x-fuzzy-match t)
  (helm-mode t)
  )

;; ;;;; ido
;; (use-package ido
;;   :demand t
;;   :config (setq ido-enable-flex-matching t)
;;   (ido-mode t)
;;   ;;(setq ido-everywhere t)
;;   )


;; Information management
;;;; Org-mode
;;;; org babel support for nix
(use-package ob-nix)
(use-package org
      :bind
      ("C-c l" . 'org-store-link)
      ("C-c a" . 'org-agenda)
      ("C-c c" . 'org-capture)
      :hook (org-mode . auto-revert-mode)
      :config
      (setq auto-revert-verbose nil)
      (setq org-directory
	    "~/Documents/org/")
      (setq org-agenda-files (list "/tmp/"))
      (setq org-log-done 'time)
      (setq org-agenda-files (list org-directory))
      (setq org-refile-use-outline-path 'file)
      (setq org-refile-targets '(
	      (nil :maxlevel . 5)
	      (org-agenda-files :maxlevel . 5)))
      (setq org-outline-path-complete-in-steps nil)
      (setq org-default-notes-file (concat org-directory "/notes.org"))
      (setq org-capture-templates
            `(("i" "inbox" entry (file ,(concat org-directory "/inbox.org"))
               "* TODO %?")
              ("l" "link" entry (file ,(concat org-directory "/inbox.org"))
               "* TODO %(org-cliplink-capture)" :immediate-finish t)
              ("c" "org-protocol-capture" entry (file ,(concat org-directory "/inbox.org"))
               "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
              ("u" "URL capture from Safari" entry (file+olp+datetree ,(concat
 org-directory "/links.org"))
               "* %i    :safari:url:\n%U\n\n")))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((nix . t)
	 (shell .t))))

(use-package org-protocol
  :ensure nil
  :after org)

;;;; Hyperbole

(use-package hyperbole
	    :demand t
	    :config
	    (add-to-list 'Info-directory-list (concat hyperb:dir "man/"))
	    (add-to-list 'hyrolo-file-list (concat org-directory "people.org"))
	    (hyperbole-mode t)
	    (add-hook 'hyperbole-init-hook
		      (lambda ()
			(require 'org)
			(setq hyrolo-file-list (append (hyrolo-initialize-file-list)
						       (list org-directory))))))

;; Nix
;; https://github.com/NixOS/nix-mode?tab=readme-ov-filelsp
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))


;; Tramp
;;;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Ssh-setup.html
(use-package tramp
  :config
  (setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlMaster=auto "
       "-o ControlPath=~/tmp/.ssh-control-%%r-%%h-%%p"))
  (tramp-set-completion-function
   "ssh" (append (tramp-get-completion-function "ssh")
               (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                       (directory-files
                        "~/.ssh/conf.d/"
                        'full directory-files-no-dot-files-regexp))))
  
  (setq tramp-default-method "ssh")
  :custom
  (custom-set-variables  '(tramp-remote-path
                           (quote
                            (tramp-own-remote-path))))
  (debug-ignored-errors
        (cons 'remote-file-error debug-ignored-errors))
  (tramp-lock-file-name-transforms
      '(("\\`\\(.+\\)\\'" "\\1~"))))

(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("C-c C-j" . avy-resume)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-define-global-key-bindings 'recommended))

(use-package pdf-tools)

;; Auctex
(use-package tex
  :ensure auctex
  :defer t
 ;; :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode)
	 (LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . abbrev-mode))
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-auto-untabify t)
  (TeX-electric-escape t)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-engine 'xetex)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; Eglot keybindings interferes with auctex
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c C-e") nil)
  :config
  ;; Set Skim as the PDF viewer
  (setq TeX-view-program-list
        '(("Skim" "open -a Skim.app %o")))
  (setq TeX-view-program-selection
        '((output-pdf "Skim")
	  ((output-dvi style-pstricks)
	   "dvips and gv")
	  (output-dvi "xdvi")
	  (output-html "open")))
  ;; Configure Skim to auto-reload PDF files
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  ;; Sync TeX source with Skim
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)))


;;   :defines
;;   (latex-help-cmd-alist
;;    latex-help-file)
;;   :preface
;;   (defvar latex-prettify-symbols-alist
;;     '(("\N{THIN SPACE}" . ?\⟷)))
;;   :config
;;   (require 'preview)

  ;; (defun latex-help-get-cmd-alist ()    ;corrected version:
  ;;   "Scoop up the commands in the index of the latex info manual.
  ;;  The values are saved in `latex-help-cmd-alist' for speed."
  ;;   ;; mm, does it contain any cached entries
  ;;   (if (not (assoc "\\begin" latex-help-cmd-alist))
  ;;       (save-window-excursion
  ;;         (setq latex-help-cmd-alist nil)
  ;;         (Info-goto-node (concat latex-help-file "Command Index"))
  ;;         (goto-char (point-max))
  ;;         (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
  ;;           (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
  ;;                 (value (buffer-substring (match-beginning 2)
  ;;                                          (match-end 2))))
  ;;             (add-to-list 'latex-help-cmd-alist (cons key value))))))
  ;;   latex-help-cmd-alist)

  ;; (info-lookup-add-help :mode 'LaTeX-mode
  ;;                       :regexp ".*"
  ;;                       :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
  ;;                       :doc-spec '(("(latex2e)Concept Index")
  ;;                                   ("(latex2e)Command Index")))

  ;; (add-hook 'LaTeX-mode-hook
  ;;           #'(lambda
  ;;               ()
  ;;               (setq-local prettify-symbols-alist latex-prettify-symbols-alist)
  ;;               (prettify-symbols-mode 1)))

  ;; (add-hook 'TeX-after-compilation-finished-functions
  ;;           #'TeX-revert-document-buffer))

;; My packages
(use-package capture-frame
  :load-path "./capture-frame.el"
  :commands (my/make-capture-frame)) 

(use-package terraform-mode
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))


(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-copy-env "PATH"))


(use-package helm-org-rifle)
(use-package org-transclusion)
(use-package org-ref
  :config
  (require 'org-ref-helm)
  :bind
  (:map org-mode-map
        ("C-c ]" . org-ref-insert-link))
  :custom
  (bibtex-completion-bibliography '("~/Documents/bibliography.bib"))
  (bibtex-completion-library-path '("~/My library/"))
  (bibtex-completion-notes-path "~/notes/")
  (bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-display-formats '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
                                       (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
                                       (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                       (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                       (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
  (bibtex-completion-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil fpath))))

(use-package bibtex
  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5)
  :bind
  (:map bibtex-mode-map
        ("H-b" . org-ref-bibtex-hydra/body)))

;; this issue pr fix the issue with tramp
(use-package dumb-jump
  ;; :hook
  ;; (('xref-backend-functions #'dumb-jump-xref-activate))
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-buffer)
  )


(defun my/projectile-remove-selected-projects ()
  "Select and remove multiple projects from the known projects list."
  (interactive)
  (let* ((projects (projectile-relevant-known-projects))
         (selected (completing-read-multiple "Select projects to remove: " projects)))
    (dolist (project selected)
      (projectile-remove-known-project project))
    (message "Removed projects: %s" (string-join selected ", "))))

(use-package ebib
  :after
  (use-package org-ebib)
  :custom
  (global-set-key (kbd "C-c e") 'ebib)
  (setq ebib-bibtex-dialect 'biblatex) 
  (setq ebib-preload-bib-files '("../research/bibliography.bib" "~/Documents/bibliography.bib")))




(use-package djvu)

(use-package org-noter
  :custom
  (setq org-noter-auto-save-last-location t))

(use-package s)
(use-package slurm-mode
  :config
  (use-package slurm-script-mode))

; References
;;;; Disclaimars
;; the current version borrows heavily from John Wiegley excellent
;; dot-emacs repo (https://github.com/jwiegley/dot-emacs)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("36b57dcbe8262c52d3123ed30fa34e5ef6b355881674142162a8ca8e26124da9" "6b912e025527ffae0feb76217f1a3e494b0699e5219ab59ea4b3a36c319cea17" "52632b69c2813771327a2c22f51ccaca466ba3cc1aa8f3bf2d613573ea934993" default))
 '(nil nil t)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(ivy-mode helm-config helm-projectile projectile-helm slurm-script-mode s combobulate slurm-mode org-noter djvu org-ebib ebib dumb-jump org-ref org-transclusion helm-org-rifle exec-path-from-shell terraform-mode pdf-tools avy org-protocol treesit elixir-modeg dired-x dired use-package auctex vterm treesit-auto projectile ob-nix nix-mode modus-themes magit hyperbole helm elixir-mode))
 '(package-vc-selected-packages
   '((combobulate :url "https://github.com/mickeynp/combobulate")))
 '(safe-local-variable-values
   '((projectile-project-test-cmd . "nix flake check")
     (projectile-project-run-cmd . "darwin-rebuild test --flake . --fast")
     (projectile-project-compilation-cmd . "darwin-rebuild switch --flake .#m1 --impure")
     (projectile-project-configure-cmd . "nix flake update")))
 '(tramp-remote-path '(tramp-own-remote-path)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
