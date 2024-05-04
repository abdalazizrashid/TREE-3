;; Set it to big number(100mb) like most of the popular starter kits like Spacemacs/Doom/Prelude, etc do:
;; Follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: "My suggestion is to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch. If even a 100-fold increase doesn't help, there's some deeper problem with the Lisp code which produces so much garbage, or maybe GC is not the reason for slowdown." Source: <https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/>

(setq gc-cons-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process#
;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; plists provide better performance in deserialization and also put less presure than hash-tables
(setenv "LSP_USE_PLISTS" "true")

;; Disable the tool bar
(tool-bar-mode -1)
(menu-bar-mode 1)
(toggle-scroll-bar -1) 
(scroll-bar-mode -1)


