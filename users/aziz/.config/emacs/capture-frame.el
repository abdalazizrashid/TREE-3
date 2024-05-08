;;; capture-frame.el --- Create a global dedicated frames for org-capture

;; Author: Your Name <abdalaziz.rashid@outlook.com>
;; Version: 0.1.0
;; Keywords: convenience, frames, org-capture
;; URL: http://github.com/abdalazizrashid/tree-3

;;; Commentary:

;; This package provides utilities to create a dedicated frame for org-capture.
;; It includes functions to manage frame focus and automatically close the frame
;; when org-capture is finished.

;;; Code:


(defun my/call-raise-frame ()
  "Raise the current frame to the top of the window stack."
  (raise-frame))

(add-hook 'server-visit-hook 'my/call-raise-frame)

(defun my/make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (let ((capture-frame (make-frame '((name . "capture")
                                     (undecorated . t)
                                     (z-group . above)
                                     (top . 300)
                                     (left . 700)
                                     (width . 80)
                                     (height . 25)))))
    (select-frame-set-input-focus capture-frame)
    (delete-other-windows)
    (let ((original-func (symbol-function 'switch-to-buffer-other-window)))
      (unwind-protect
          (progn
            (fset 'switch-to-buffer-other-window
                  (lambda (buf) (switch-to-buffer buf)))
            (condition-case nil
                (org-capture)
              (error nil))) ; Ignore errors
        (fset 'switch-to-buffer-other-window original-func)))))

(defadvice org-capture-finalize (after my/delete-capture-frame activate)
  "Close the capture frame after finalizing org-capture."
  (when (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(defadvice org-capture-destroy (after my/delete-capture-frame activate)
  "Close the capture frame when org-capture is aborted."
  (when (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(defadvice org-capture-select-template (around my/delete-capture-frame activate)
  "Close the capture frame if org-capture template selection is aborted."
  (unless (ignore-errors ad-do-it t)
    (setq ad-return-value "q"))
  (when (and (equal "q" ad-return-value)
             (equal "capture" (frame-parameter nil 'name)))
    (delete-frame)))

;;; End:

(provide 'capture-frame)
;;; capture-frame.el ends here
