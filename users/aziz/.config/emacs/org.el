(defun call-raise-frame ()
   (raise-frame))
(defun end-server-edit ()
   (shell-command "osascript -e \"tell application \\\"System Events\\\" to keystroke tab using command down\""))
(add-hook 'server-visit-hook 'call-raise-frame)
(add-hook 'server-done-hook 'end-server-edit)
(server-start)


(defun my/make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (let ((capture-frame (make-frame '((name . "capture")
                             ;;        (undecorated . t)
                                     (z-group . above)
                                     (top . 300)
                                     (left . 700)
                                     (width . 80)
                                     (height . 25)))))
    (select-frame-set-input-focus capture-frame)
    (delete-other-windows)
    ;; Redefine switch-to-buffer-other-window temporarily
    (let ((original-func (symbol-function 'switch-to-buffer-other-window)))
      (unwind-protect
          (progn
            (fset 'switch-to-buffer-other-window
                  (lambda (buf) (switch-to-buffer buf)))
            (condition-case err
               (org-capture)
             (error nil))) ; Ignore all errors gracefully
        ;; Restore the original function
        (fset 'switch-to-buffer-other-window original-func)))))



(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-select-template (around delete-capture-frame activate)
  "Advise org-capture-select-template to close the frame on abort"
  (unless (ignore-errors ad-do-it t)
    (setq ad-return-value "q"))
  (if (and
       (equal "q" ad-return-value)
       (equal "capture" (frame-parameter nil 'name)))
      (delete-frame)))
