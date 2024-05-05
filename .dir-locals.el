;;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")
:
((nil . ((sentence-end-double-space . t)
         (emacs-lisp-docstring-fill-column . 65)
         (fill-column . 70)
         (projectile-project-configure-cmd . "nix flake update")
         (projectile-project-compilation-cmd . "darwin-rebuild switch --flake .")
         (projectile-project-run-cmd . "darwin-rebuild test --flake . --fast")
         (projectile-project-test-cmd . "nix flake check")
         (compile-command
	  . "darwin-rebuild test --flake" (expand-file-name "." default-directory))))
 (nix-mode
  . ((mode . nixpkgs-fmt-on-save)
     ((eglot-workspace-configuration
       . (
          (options
           (nixos
            :expr
            (format "(builtins.getFlake \"%s\").darwinConfigurations.m1.options"
                    (expand-file-name "." default-directory))
            (Home-manager
             :expr
	     (format "(builtins.getFlake \"%s\").darwinConfigurations.m1.options.home-manager"
                    (expand-file-name "." default-directory)))))))))))


