(defvar md-slide-preview-dir (file-name-directory load-file-name))
(defvar-local md-slide-preview nil "non-nil to activate markdown preview mode for slide editting")
(defvar md-slide-preview-template
        (expand-file-name "md-slide-preview.html" md-slide-preview-dir)
        )
(defvar md-slide-preview-command "emacs-md-slide-preview")
(defvar md-slide-preview--buffers nil)

(defun activate-md-slide-preview ()
  (when md-slide-preview
    (add-to-list 'md-slide-preview--buffers (buffer-name))
    ))
(defun deactivate-md-slide-preview ()
  (setq md-slide-preview--buffers (remove (buffer-name) md-slide-preview--buffers))
  )

(add-hook 'markdown-preview-mode-hook 'activate-md-slide-preview)
(add-hook 'kill-buffer-hook 'deactivate-md-slide-preview)

(defun md-slide-preview-wrapper (f &rest args)
  (if (not md-slide-preview) (apply f args)
    (let ((markdown-preview--preview-template md-slide-preview-template)
          )
      (apply f args)
      )))

(dolist (f '(markdown-preview--start))
  (advice-add f :around 'md-slide-preview-wrapper)
  )

(advice-add 'markdown-preview--send-preview-to :around 'advice--markdown-preview--send-preview-to)
(defun advice--markdown-preview--send-preview-to (f websocket preview-uuid)
  (let ((md-buffer (gethash preview-uuid markdown-preview--preview-buffers))
        (page-number 0)
        cur
        )
    (if (not (member md-buffer md-slide-preview--buffers)) (apply f (list websocket preview-uuid))
      (setq cur (save-mark-and-excursion (end-of-line) (point)))
      (when (not (region-active-p))
        (save-mark-and-excursion
          (goto-char (point-min))
          (while (integerp (search-forward-regexp "^##?[ \t]" cur t))
            (setq page-number (+ page-number 1))
            )))
      (if (eq page-number 0) (setq page-number 1))

      (when md-buffer
        (with-current-buffer md-buffer
          (let ((markdown-command md-slide-preview-command))
            (markdown markdown-output-buffer-name)
            )))

      (with-current-buffer markdown-output-buffer-name ;; get-buffer
        (websocket-send-text websocket
                             (concat
                              (number-to-string page-number)
                              ";"
                              (buffer-substring-no-properties (point-min) (point-max))
                              ))))))

(defun ad-before--markdown-preview--stop ()
  (remove-hook 'after-save-hook
               (lambda ()
                 (when markdown-preview--uuid
                   (markdown-preview--send-preview markdown-preview--uuid)
                   ))
               t
               ))
(advice-add 'markdown-preview--stop :before 'ad-before--markdown-preview--stop)
