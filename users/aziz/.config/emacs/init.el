
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

(setq use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      use-package-compute-statistics t
      debug-on-error init-file-debug)

;; Define the “data environment” for this instance of Emacs

(defconst emacs-environment (or (getenv "NIX_MYENV_NAME") "default"))

(defconst emacs-data-suffix
  (cond ((string= "emacsERC" emacs-environment) "alt")
        ((string-match "emacs2[6789]\\(.+\\)$" emacs-environment)
         (match-string 1 emacs-environment))))

(defconst alternate-emacs (string= emacs-data-suffix "alt"))

(defconst user-data-directory
  (emacs-path (if emacs-data-suffix
                  (format "data-%s" emacs-data-suffix)
                "data")))

(defun user-data (dir)
  (expand-file-name dir user-data-directory))

;; Font setup
;; https://slumpy.org/blog/2016-01-11-proper-way-to-setup-fonts-in-emacs/
(defun my/setup-fonts ()
  (interactive)
  (set-face-font 'default "Berkeley Mono-15")
  (set-fontset-font t 'hebrew (font-spec :name "Berkeley Mono-15"))
)


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
  (auto-save-file-name-transforms '(("\\`/[^/]*:.*" "/tmp" t)))
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

  :custom-face


  :init
  (setq disabled-command-function nil) ;; enable all commands
  

  :config
  ;; Setup theme
  (load-theme 'modus-operandi)
  
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

  (add-hook 'after-make-frame-functions 'my-toggle-toolbar))



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
  :config
  ;; (when (string= system-type "darwin")
  ;;   (setq dired-use-ls-dired t
  ;;         insert-directory-program "/usr/local/bin/gls"
  ;;         dired-listing-switches "-aBhl --group-directories-first")))
)  
(use-package dired-x
  :after dired
  :config
  (add-hook 'dired-mode-hook #'dired-omit-mode))

  

;; Project management
;;;; projectile
(use-package projectile
  :init
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-sort-order 'recentf)
  (setq projectile-git-use-fd t)
  (setq projectile-enable-caching t))



;; Terminal emulator
;;;; vterm
(use-package vterm
  :defer t)



;; Treesitter
;;;; Treesitter remapping
(add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))

;;;; Structural editing
(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  (use-package combobulate
    :preface
    (setq combobulate-key-prefix "C-c o")
    :hook
      ((python-ts-mode . combobulate-mode)
       (js-ts-mode . combobulate-mode)
       (html-ts-mode . combobulate-mode)
       (css-ts-mode . combobulate-mode)
       (yaml-ts-mode . combobulate-mode)
       (typescript-ts-mode . combobulate-mode)
       (json-ts-mode . combobulate-mode)
       (tsx-ts-mode . combobulate-mode))
    :load-path ("combobulate")))



;;;; eglot
(use-package eglot
  :commands eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . flyspell-prog-mode)
         (python-ts-mode . superword-mode)
         (python-ts-mode . hs-minor-mode)
         (python-ts-mode . (lambda () (set-fill-column 88)))
         (nix-ts-mode . eglot-ensure)
         (prog-mode . (lambda ()
                        (add-hook 'before-save-hook 'eglot-format nil t))))
 
  :config
  (setq read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd")))
  (with-eval-after-load 'eglot
    (dolist (mode '((nix-mode . ("nixd"))))
      (add-to-list 'eglot-server-programs mode)))

  (add-to-list
   'eglot-server-programs `(elixir-ts-mode
       . ,(eglot-alternatives
      ;; look for flakes first
	   '(("nix-shell" "-p" "elixir-ls" "elixir" "--run" "elixir-ls")))))
  
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
      (:nixd . (
		(nixpkgs :expr "import <nixpkgs> {}")
 	        (formatting 
		 command ["nixpkgs-fmt"])
                (options (nixos :expr "(builtins.getFlake \"/Users/aziz/.config/nix-darwin/\").darwinConfigurations.simple.options")
                         ( home-manager :expr "import <home-manager> {}")
			 ))))))

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

(use-package info-lookmore
  :after info-look
  :config
  (info-lookmore-elisp-cl)
  (info-lookmore-elisp-userlast)
  (info-lookmore-elisp-gnus)
  (info-lookmore-apropos-elisp))

;; Completion

;;;; Orderless
(use-package orderless
  :disabled t
  :demand t
  :config
  (defun prefixes-for-separators (pattern _index _total)
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  :custom
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators)))

;;;; Helm
(use-package helm
  :demand t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x m") #'helm-M-x)
  (global-set-key (kbd "C-c m") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-c i n") #'helm-complete-file-name-at-point)
  (global-set-key (kbd "C-x i") #'helm-imenu)
  (global-set-key (kbd "M-g i") #'helm-imenu)
  
  (setq helm-completion-style 'helm))

;;;; ido
(use-package ido
  :demand t
  :config (setq ido-enable-flex-matching t)
  (ido-mode t)
  ;;(setq ido-everywhere t)
  )


;; Information management
;;;; Org-mode
 (use-package org
      :bind
      ("C-c l" . 'org-store-link)
      ("C-c a" . 'org-agenda)
      ("C-c c" . 'org-capture)
      :after
      (use-package org-protocol)
      :config
      (setq org-directory
	    "/Users/aziz/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
      (setq org-log-done 'time
            org-agenda-files (list org-directory)
            org-refile-targets '((org-agenda-files :maxlevel . 5))
            org-refile-use-outline-path 'file)
      (setq org-default-notes-file (concat org-directory "/notes.org"))
      (setq org-capture-templates nil)
      (setq org-capture-templates
            `(("i" "inbox" entry (file ,(concat org-directory "inbox.org"))
               "* TODO %?")
              ("l" "link" entry (file ,(concat org-directory "inbox.org"))
               "* TODO %(org-cliplink-capture)" :immediate-finish t)
              ("c" "org-protocol-capture" entry (file ,(concat org-directory "inbox.org"))
               "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
              ("u" "URL capture from Safari" entry (file+olp+datetree ,(concat org-directory "links.org"))
               "* %i    :safari:url:\n%U\n\n"))))


;;;; Hyperbole

(use-package hyperbole
	    :demand t
	    :config
	    (add-to-list 'Info-directory-list (concat hyperb:dir "man/"))
	    (hyperbole-mode t))
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
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (custom-set-variables  '(tramp-remote-path
                           (quote
                            (tramp-own-remote-path)))))

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
  :custom
  (magit-define-global-key-bindings 'recommended))

;; My packages
(use-package capture-frame
  :load-path "capture-frame.el"
  :commands (my/make-capture-frame)) 

  
;; References
;;;; Disclaimars
;; the current version borrows heavily from John Wiegley excellent
;; dot-emacs repo (https://github.com/jwiegley/dot-emacs)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(framemove with-simulated-input el-mock vterm projectile pdf-tools nix-ts-mode magit languagetool hyperbole helm elixir-ts-mode ag ace-window nix-mode))
 '(safe-local-variable-values
   '((projectile-project-test-cmd . "")
     (projectile-project-run-cmd . "")
     (projectile-project-compilation-cmd . "")
     (projectile-project-test-cmd . "nix flake check")
     (projectile-project-run-cmd . "darwin-rebuild test --flake . --fast")
     (projectile-project-compilation-cmd . "darwin-rebuild switch --flake .#m1 --impure")
     (projectile-project-configure-cmd . "nix flake update")))
 '(send-mail-function 'mailclient-send-it)
 '(tramp-remote-path '(tramp-own-remote-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
