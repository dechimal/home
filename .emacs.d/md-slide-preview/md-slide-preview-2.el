(defvar markdown-slide-preview-dir (file-name-directory load-file-name))
(defvar-local markdown-slide-preview nil "non-nil to activate markdown preview mode for slide editting")
(defvar markdown-slide-preview-template
        (expand-file-name "md-slide-preview.html" markdown-slide-preview-dir)
        )
(defvar markdown-slide-preview-command "emacs-md-slide-preview")

(defvar markdown-slide-preview--buffer-list nil)

(defvar markdown-slide-preview--server nil)
(defvar markdown-slide-preview--server-port nil)

(define-minor-mode markdown-slide-preview-mode
  "Markdown slide preview mode."
  :group 'markdown-slide-preview
  :init-value nil
  (if markdown-slide-preview-mode
      (markdown-slide-preview--activate)
    (markdown-slide-preview--deactivate)
    )
  )

(defun markdown-slide-preview-mode--activate ()
  (add-to-list 'markdown-slide-preview--buffer-list (buffer-name))
  (when (not markdown-slide-preview--server)
    (setq markdown-slide-preview--server
          (websocket-server )
    )
  )

(defun markdown-slide-preview-mode--deactivate ()
  (remove (buffer-name) 'markdown-slide-preview--buffer-list)
  )

(provide 'markdown-slide-preview-mode)
