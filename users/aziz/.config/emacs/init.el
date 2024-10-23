;; Garbage collect after startup
(add-hook 'after-init-hook #'garbage-collect t)
(defvar bootstrap-version)
(let ((bootstrap-file
	 (expand-file-name
	  "straight/repos/straight.el/bootstrap.el"
	  (or (bound-and-true-p straight-base-dir)
	      user-emacs-directory)))
	(bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; :config
;; ;; Setup a personal keymap. I'll bind various things to this later on:
(bind-keys :prefix "<f1>"
	     :prefix-map my/map)
(setq use-package-enable-imenu-support t
	use-package-compute-statistics t)
(straight-use-package 'org)

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-region '(bg-only)
	modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
  (setq modus-themes-custom-auto-reload nil
	modus-themes-to-toggle '(modus-operandi modus-vivendi)
	;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
	;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
	;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui t
	modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-org-blocks nil
	modus-themes-completions '((t . (extrabold)))
	modus-themes-prompts '(extrabold)
  ;; 	modus-themes-headings
  ;; 	'((agenda-structure . (variable-pitch light 2.2))
  ;;         (agenda-date . (variable-pitch regular 1.3))
  ;;         (t . (regular 1.15))))

  ;; (setq 
   modus-themes-common-palette-overrides nil
  ;; '((bg-mode-line-active bg-cyan-subtle)
  ;;   (keybind yellow-warmer))
	 )


  :bind ("<f5>" . modus-themes-toggle)

  :config
  ;; Load the theme of your choice:
  (load-theme 'modus-operandi-tinted :no-confirm)
  (modus-themes-select 'modus-operandi-tinted)
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

  (vc-follow-symlinks nil)
  :init
  (setq disabled-command-function nil) ;; enable all commands


  :config  
  ;; Setup font
  ;; (advice-add 'server-create-window-system-frame
  ;;              :after 'my/setup-fonts)
  ;; (advice-add 'server-create-tty-frame
  ;;              :after 'my/setup-fonts)
  ;; (unless (daemonp) (my/setup-fonts))

  ;; ;; Enable line numbers only with programing modes
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

(use-package s)

(use-package async)

(use-package vterm
  :straight t
  :defer t)

(use-package fontaine
  :disabled
  :config
  (setq fontaine-presets
        '((small
           :default-family "Berkeley Mono"
           :default-height 80
           :variable-pitch-family "Berkeley Mono")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (presentation
           :inherit medium
           :default-weight light
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Berkeley Mono"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Berkeley Mono"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; Persist font configurations while switching themes.  The
  ;; `enable-theme-functions' is from Emacs 29.
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)

)

;;;; eglot
(use-package project
  :ensure t)
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

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;;;; Orderless
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (defun prefixes-for-separators (pattern _index _total)
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  :custom
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators))
  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(defun my/consult-ripgrep-nixpkgs ()
  "Search Nixpkgs repository using `rg` via `consult-ripgrep`."
  (interactive)
  (let ((default-directory "~/Sources/nixpkgs/"))  ;; Change this to your nixpkgs path
    (consult-ripgrep default-directory)))

(defun my/consult-ripgrep-symbol-at-point-nixpkgs ()
  "Search the symbol at point in the Nixpkgs repository using `rg` via `consult-ripgrep`."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)) ;; Get the symbol at point
        (default-directory "~/Sources/nixpkgs/")) ;; Replace with your nixpkgs path
    (if symbol
        (consult-ripgrep default-directory symbol) ;; Use the symbol for search
      (message "No symbol at point")))) ;; Error message if no symbol is found

;; Configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
	 ;; Custom bindings
	 ("C-c n" . my/consult-ripgrep-nixpkgs)
         ("C-c s" . my/consult-ripgrep-symbol-at-point-nixpkgs)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :commands (my/consult-ripgrep-nixpkgs
              my/consult-ripgrep-symbol-at-point-nixpkgs)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
    ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  (add-to-list 'consult-fd-args "--hidden" t)
  (add-to-list 'consult-fd-args "--exclude .git" t)
  )

(use-package embark-consult)

(use-package avy
  :straight t
  :config
  (avy-setup-default)
  :bind
  (("C-:" . avy-goto-char-timer)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("C-c C-j" . avy-resume)))

;;;; Hyperbole
(use-package hyperbole
  :config
  (add-to-list 'Info-directory-list (concat hyperb:dir "man/"))
  (add-to-list 'hyrolo-file-list (concat org-directory "people.org"))
  (hyperbole-mode t)
  (add-hook 'hyperbole-init-hook
            (lambda ()
              (require 'org)
              (setq hyrolo-file-list (append (hyrolo-initialize-file-list)
                                             (list org-directory))))))

;;;; Org-mode
(setq org-drill-template
   "* %^{Topic|Noun|Verb|Adjective|Phrase}                                    :drill:
    :PROPERTIES:
    :DRILL_CARD_TYPE: %^{Card Type|twosided|hide1cloze|multisided}
    :END:

     **** Russian
     %^{Russian word}
     **** English
     %^{English translation}

     **** Example sentence (optional)
     %^{Russian example sentence}
     %^{English example sentence}
     ")

(use-package org
  :init
  ;;;; org babel support for nix
  (use-package ob-nix)
  :bind
  ("C-c l" . 'org-store-link)
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture)
  :hook (org-mode . auto-revert-mode)
  :config
  (require 'org-protocol)
  (setq auto-revert-verbose nil)
  (setq org-directory
        "~/Documents/org/")
  (setq org-log-done 'time)
  (setq org-agenda-files (list org-directory))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '(
                             (nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %?")
          ("l" "link" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          ("u" "URL capture from Safari" entry (
                                                file+olp+datetree ,(concat org-directory "/links.org"))
           "* %i    :safari:url:\n%U\n\n")
	  ("d" "Drill Entry" entry
           (file+headline ,(concat org-directory "inbox.org") "Drill cards")
           ,org-drill-template)
	  ))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((nix . t)
     (shell . t)
     (python . t)))
  (setq org-preview-latex-default-process 'dvisvgm))

(use-package org-transclusion)

(use-package org-noter
  :config
  (setq org-noter-auto-save-last-location t))

(use-package org-ref
  :bind
  (:map org-mode-map
        ("C-c ]" . org-ref-insert-link))
  :config
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos)
  :custom
  ;;  (bibtex-completion-bibliography '("~/Documents/bibliography.bib"))
  (bibtex-completion-bibliography '("~/Documents/library/inbox.bib"))
  (bibtex-completion-library-path '("~/Documents/library/"))
  (bibtex-completion-notes-path "~/Documents/library/notes/")
  (bibtex-completion-notes-template-multiple-files "*${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}:
 \n\nSee [[cite:&${=key=}]]\n")
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-display-formats '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}
 ${journal:40}")
                                       (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}
 Chapter ${chapter:32}")
                                       (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}
 ${booktitle:40}")
                                       (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}
 ${booktitle:40}")
                                       (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
  (bibtex-completion-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil fpath))))

;; (load "other_importers.el")

(use-package bibtex
  :straight t
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

(use-package ebib
 :after
 (use-package org-ebib)
 :custom
 (global-set-key (kbd "C-c e") 'ebib)
 (setq ebib-bibtex-dialect 'biblatex) 
 (setq ebib-preload-bib-files '("../research/bibliography.bib" "~/Documents/bibliography.bib")))

(defun retrieve-crossref-metadata (&optional doi)
  "Retrieve metadata from CrossRef using DOI and populate rec database."
  (interactive)
  (unless doi
    (setq doi (read-string "Enter DOI: ")))
  (let ((url (format "https://api.crossref.org/works/%s" doi)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((json-object (json-read))
             (message (cdr (assoc 'message json-object)))
             (title-list (cdr (assoc 'title message)))
             (title (if (and (sequencep title-list) (> (length title-list) 0))
                        (elt title-list 0)
                      ""))
             (authors-list (cdr (assoc 'author message)))
             (authors (if authors-list
                          (mapconcat (lambda (author)
                                       (concat (cdr (assoc 'given author)) " " (cdr (assoc 'family author))))
                                     authors-list
                                     "; ")
                        ""))
             (journal-list (cdr (assoc 'container-title message)))
             (journal (if (and journal-list (sequencep journal-list) (> (length journal-list) 0))
                          (elt journal-list 0)
                        ""))
             (year (or
                    (let ((published-print (cdr (assoc 'published-print message))))
                      (if published-print
                          (let ((date-parts (cdr (assoc 'date-parts published-print))))
                            (if date-parts
                                (aref (aref date-parts 0) 0)
                              nil))
                        nil))
                    (let ((published-online (cdr (assoc 'published-online message))))
                      (if published-online
                          (let ((date-parts (cdr (assoc 'date-parts published-online))))
                            (if date-parts
                                (aref (aref date-parts 0) 0)
                              nil))
                        nil))))
             (volume (or (cdr (assoc 'volume message)) ""))
             (number (or (cdr (assoc 'issue message)) ""))
             (pages (or (cdr (assoc 'page message)) ""))
             (doi (cdr (assoc 'DOI message)))
             (url (cdr (assoc 'URL message)))
             (abstract (or (cdr (assoc 'abstract message)) ""))
             (tags-list (cdr (assoc 'subject message)))
             (tags (if tags-list
                       (mapconcat 'identity tags-list "; ")
                     ""))
             )
        ;; Now, create the rec record
        (let ((rec-entry (format "ID: %d\nTitle: %s\nAuthors: %s\nJournal: %s\nYear: %s\nVolume: %s\nNumber: %s\nPages: %s\nDOI: %s\nTags: %s\nURL: %s\nAbstract: %s\nTopics: \n\n"
                                 id
                                 title
                                 authors
                                 journal
                                 (if year (number-to-string year) "")
                                 volume
                                 number
                                 pages
                                 doi
                                 tags
                                 url
                                 (replace-regexp-in-string "<[^>]*>" "" abstract))))
          ;; Append to the rec to clipboard
	  (kill-new rec-entry)
	  (message "Rec record copied to clipboard")
	  )))))


(defun retrieve-crossref-metadata-by-isbn (&optional isbn)
  "Retrieve metadata from CrossRef using ISBN and populate rec database."
  (interactive)
  (unless isbn
    (setq isbn (read-string "Enter ISBN: ")))
  (let ((url (format "https://api.crossref.org/works?filter=isbn:%s" isbn)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((json-object (json-read))
             (message (cdr (assoc 'message json-object)))
             (items (cdr (assoc 'items message)))
             ;; We take the first item in the list as the matching work
             (work (if (and (sequencep items) (> (length items) 0))
                       (elt items 0)
                     nil)))
        (if work
            (let* ((title-list (cdr (assoc 'title work)))
                   (title (if (and (sequencep title-list) (> (length title-list) 0))
                              (elt title-list 0)
                            ""))
                   (authors-list (cdr (assoc 'author work)))
                   (authors (if authors-list
                                (mapconcat (lambda (author)
                                             (concat (cdr (assoc 'given author)) " " (cdr (assoc 'family author))))
                                           authors-list
                                           "; ")
                              ""))
                   (journal-list (cdr (assoc 'container-title work)))
                   (journal (if (and journal-list (sequencep journal-list) (> (length journal-list) 0))
                                (elt journal-list 0)
                              ""))
                   (year (or
                          (let ((published-print (cdr (assoc 'published-print work))))
                            (if published-print
                                (let ((date-parts (cdr (assoc 'date-parts published-print))))
                                  (if date-parts
                                      (aref (aref date-parts 0) 0)
                                    nil))
                              nil))
                          (let ((published-online (cdr (assoc 'published-online work))))
                            (if published-online
                                (let ((date-parts (cdr (assoc 'date-parts published-online))))
                                  (if date-parts
                                      (aref (aref date-parts 0) 0)
                                    nil))
                              nil))))
                   (volume (or (cdr (assoc 'volume work)) ""))
                   (number (or (cdr (assoc 'issue work)) ""))
                   (pages (or (cdr (assoc 'page work)) ""))
                   (doi (cdr (assoc 'DOI work)))
                   (url (cdr (assoc 'URL work)))
                   (abstract (or (cdr (assoc 'abstract work)) ""))
                   (tags-list (cdr (assoc 'subject work)))
                   (tags (if tags-list
                             (mapconcat 'identity tags-list "; ")
                           ""))
                   (id (retrieve-rec-next-id "database.rec")))
              ;; Create the rec record
              (let ((rec-entry (format "ID: %d\nTitle: %s\nAuthors: %s\nJournal: %s\nYear: %s\nVolume: %s\nNumber: %s\nPages: %s\nDOI: %s\nTags: %s\nURL: %s\nAbstract: %s\nTopics: \n\n"
                                       id
                                       title
                                       authors
                                       journal
                                       (if year (number-to-string year) "")
                                       volume
                                       number
                                       pages
                                       doi
                                       tags
                                       url
                                       (replace-regexp-in-string "<[^>]*>" "" abstract))))
                ;; Append to the rec database file
                (kill-new rec-entry)
		(message "Rec record copied to clipboard")))
          (message "No metadata found for ISBN: %s" isbn))))))


(defun retrieve-openlibrary-metadata-by-isbn (&optional isbn)
  "Retrieve metadata from Open Library using ISBN and populate rec database."
  (interactive)
  (unless isbn
    (setq isbn (read-string "Enter ISBN: ")))
  (let ((url (format "https://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data" isbn)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((json-object (json-read))
             (book-data (cdr (assoc (concat "ISBN:" isbn) json-object))))
        (if book-data
            (let* ((title (or (cdr (assoc 'title book-data)) ""))
                   (authors-list (cdr (assoc 'authors book-data)))
                   (authors (if authors-list
                                (mapconcat (lambda (author)
                                             (cdr (assoc 'name author)))
                                           authors-list
                                           "; ")
                              ""))
                   (publish-date (or (cdr (assoc 'publish_date book-data)) ""))
                   (publish-year (if (string-match "\\([0-9]\\{4\\}\\)" publish-date)
                                     (match-string 1 publish-date)
                                   ""))
                   (pages (or (cdr (assoc 'number_of_pages book-data)) ""))
                   (tags ""))
              ;; Create the rec record
              (let ((rec-entry (format "ID: %d\nTitle: %s\nAuthors: %s\nJournal: \nYear: %s\nVolume: \nNumber: \nPages: %s\nDOI: \nTags: %s\nURL: \nAbstract: \nTopics: \n\n"
                                       (retrieve-rec-next-id "database.rec")
                                       title
                                       authors
                                       publish-year
                                       (if (numberp pages) (number-to-string pages) "")
                                       tags)))
                ;; Append to the rec database file
                (with-temp-buffer
                  (insert rec-entry)
                  (write-region (point-min) (point-max) "database.rec" t))))
          (message "No metadata found for ISBN: %s" isbn))))))

(use-package rec-mode)

(use-package pdf-tools)

(use-package djvu)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package man
  :config
  ;; Color the man page
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; (dolist (mapping
;;          '((python-mode . python-ts-mode)
;; 	   (elixir-mode . elixir-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (typescript-mode . typescript-ts-mode)
;;            (js2-mode . js-ts-mode)
;;            (bash-mode . bash-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (json-mode . json-ts-mode)
;;            (js-json-mode . json-ts-mode))))

;; (add-to-list 'major-mode-remap-alist mapping)

;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))


(use-package combobulate
  :disabled
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
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
   (tsx-ts-mode . combobulate-mode)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-define-global-key-bindings 'recommended))

(use-package dumb-jump
  :straight t
  ;; :hook
  ;; (('xref-backend-functions #'dumb-jump-xref-activate))
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-buffer)
  )

;; projectile
(use-package project)
(use-package projectile
:init
(projectile-mode t)
(require 'tramp)
:bind
((:map projectile-mode-map
       ("s-p" . projectile-command-map))
       ("s-p v" . 'magit))

:config
(setq projectile-sort-order 'recentf)
(setq projectile-git-use-fd t)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(advice-add 'projectile-project-root :before-while
            (lambda (&optional dir)
              (not (file-remote-p (or dir default-directory)))))
(add-to-list 'projectile-other-file-alist '("ex" . ("html.heex" "html.leex")))
(add-to-list 'projectile-other-file-alist '("html.heex" . ("ex")))
(add-to-list 'projectile-other-file-alist '("html.leex" . ("ex")))
)

(use-package elixir-mode)

;; Nix
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :config
  ;; (use-package nix-drv-mode
  ;;   :ensure nix-mode
  ;;   :mode "\\.drv\\'")

  ;; (use-package nix-shell
  ;;   :ensure nix-mode
  ;;   :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

  ;; (use-package nix-repl
  ;;   :ensure nix-mode
  ;;   :commands (nix-repl))
  )

(use-package terraform-mode
 :custom (terraform-indent-level 4)
 :config
 (defun my-terraform-mode-init ()
   ;; if you want to use outline-minor-mode
   ;; (outline-minor-mode 1)
   )
 (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(add-to-list 'auto-mode-alist '("\\.ts\\(x\\)?\\'" . tsx-ts-mode))

(use-package rust-mode
      :straight t
      :defer t)

(use-package geiser-mit)

(use-package yasnippet
  :config
  ;; get more snippets from here
  ;; https://github.com/AndreaCrotti/yasnippet-snippets/tree/master/snippets
  (setq yas-snippet-dirs
      '("./snippets"))
  (yas-global-mode 1))

;; Auctex
(use-package auctex)
(use-package cdlatex)
(use-package tex
  :straight nil
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

(use-package slurm-mode
  :config
  (require 'slurm-script-mode))

(use-package writeroom-mode)

(use-package ement)
