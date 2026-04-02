;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(require 'tramp)

(global-set-key "\C-h" 'backward-delete-char)

;; tool bar, menu bar
(if window-system
 (progn (tool-bar-mode 0)
        (set-scroll-bar-mode nil)))
(menu-bar-mode 0)

(setq truncate-lines 1)
(setq-default truncate-lines 1)
(setq inhibit-startup-message t)
(column-number-mode t)

(add-hook 'comint-output-filter-functions
  'comint-watch-for-password-prompt)

(defun find-file-around (f &rest args)
  (let ((filespec (car args)) filename lineno buffer)
    (if (equal (string-match "^\\([^:]*\\):\\(.*\\)$" filespec) ())
        (apply f args)
      (setq filename (match-string 1 filespec))
      (if (not (eq filename "/ssh"))
          (setq lineno (string-to-number (match-string 2 filespec)))
        (if (not (equal (string-match "^\\([^:]*:[^:]*\\):\\(.*\\)$" filename) ()))
            (apply f args)
          (setq filename (concat "/ssh:" (match-string 2 filename)))
          (setq lineno (string-to-number (match-string 2 filename)))
          ))
      (setq buffer (apply f (cons filename (cdr args))))
      (goto-line lineno buffer)
      )
    )
  )

(advice-add 'find-file :around 'find-file-around)
(advice-add 'find-altenative-file :around 'find-file-around)

(if window-system
  (progn
    (set-face-background 'region "Blue")
    (set-face-foreground 'font-lock-comment-face "green1")
    (set-face-foreground 'font-lock-variable-name-face nil)
    (set-face-foreground 'font-lock-constant-face "#44ffbb")
    (set-face-foreground 'font-lock-string-face "chocolate1")
    (set-face-foreground 'font-lock-keyword-face "yellow2")
    (set-face-foreground 'font-lock-function-name-face nil)
    (set-face-foreground 'font-lock-builtin-face "yellow2")
    (set-face-foreground 'font-lock-preprocessor-face "#ff66ff")
    (set-face-foreground 'font-lock-type-face "#ffdd44")
    (set-cursor-color "#ffccdd")

    (set-fontset-font (frame-parameter nil 'font) '(#x80 . #x7ffff) "Migu 2M-10" nil)

    (global-unset-key "\C-z")
    (global-unset-key "\C-x\C-z")
    ))

; 行間設定
(setq-default line-spacing 0)

;; スマートインデントを殺す
(setq c-syntactic-indentation nil)
(setq c-electric-flag nil)

(global-set-key "\C-h" 'delete-backward-char)

;; c mode hook
(defun my-c-mode-hook ()
  (setq-local c-set-style "stroustrup")
  (setq-local c-basic-offset 4)
  (setq-local c-auto-newline nil)
  (c-set-offset 'innamespace 0)
  (c-toggle-electric-state t))

(defun my-java-mode-hook ()
  (setq-local c-basic-offset 4)
  (setq-local c-auto-newline nil)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  )

(with-eval-after-load 'cc-mode
  (dolist (mode-map (list c++-mode-map awk-mode-map c-mode-map java-mode-map))
    (dolist (key '(";" "{" "}" "<" ">" "," "(" ")" "\C-c\C-c" "\C-c\C-a" "\C-c\C-s"))
      (define-key mode-map key 'self-insert-command)
      )
    (define-key mode-map [(control :)] 'c-scope-operator)
    (define-key mode-map "\C-i" 'deepen-line-or-region)
    (define-key mode-map (kbd "<backtab>") 'shallow-line-or-region)
    (define-key mode-map "\C-m" 'newline-and-indent)
    )
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun offset-unit ()
  (let ((e (assoc major-mode '((c++-mode . c-basic-offset)
                               (cc-mode . c-basic-offset)
                               (c-mode . c-basic-offset)
                               (java-mode . c-basic-offset)
                               (php-mode . c-basic-offset)
                               (rust-mode . rust-indent-offset)
                               (js-mode . js-indent-level)
                               (html-mode . sgml-basic-offset)))))
    (setq e (if e (eval (cdr e)) 4))
    ))

(defun deepen-line-or-region (&optional arg region)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (use-region-p)))
  (let ((init nil)
        (beg nil)
        (end nil)
        (indent nil)
        (blank-line nil)
        (line-end nil)
        (diff 0)
        (depth (* (offset-unit) arg)))
    (if region
        (progn
          (indent-code-rigidly (region-beginning) (region-end) depth)
          (setq deactivate-mark nil)
          )
      (save-excursion
        (setq init (point))
        (beginning-of-line)
        (search-forward-regexp "[ 	]*")
        (setq beg (match-beginning 0))
        (setq end (match-end 0))
        (when (or (not (eq beg end)) (> depth 0))
          (setq blank-line
                (and (eq beg (line-beginning-position)) (eq end (line-end-position))))
          (setq indent (< init end))
          (end-of-line)
          (setq line-end (point))
          (insert-char ?a)
          (indent-code-rigidly (line-beginning-position) (line-end-position) depth)
          (delete-char -1)
          (setq diff (- (point) line-end))
          )
        )
      (cond
       (blank-line
        (end-of-line))
       (indent
        (goto-char init))
       (t
        (goto-char (+ init diff)))
       )
      )))

(defun shallow-line-or-region (&optional arg region)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (use-region-p)))
  (deepen-line-or-region (- arg) region))

(defun newline-witless (&optional arg)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)))
  (let ((loop 'init)
        (indent-pos 0)
        (indent-str ""))
    (save-excursion
      (while loop
        (beginning-of-line)
        (search-forward-regexp "^[ 	]*")
        (if (eq loop 'init)
            (setq indent-pos (match-end 0)))
        (setq loop (eq (line-end-position) (match-end 0)))
        (cond
         (loop
          (let ((p (point)))
            (forward-line -1)
            (setq loop (not (eq (point) p)))
            ))
         (t
          (setq indent-str (buffer-substring-no-properties (line-beginning-position) (match-end 0)))
          )))
      )
    (let* ((leading
            (buffer-substring-no-properties
             (line-beginning-position)
             (point)))
           (match-result
            (string-match-p "^[ 	]*$" leading)))
      (if (numberp match-result)
          (delete-region (line-beginning-position) indent-pos)))
    (insert-char 10 arg)
    (insert-before-markers indent-str)
    ))

;; kill smart indentation
(setq c-syntactic-indentation nil)

;; ruby-mode hook
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map "\C-i" nil))

(setq auto-mode-alist
      (append
        '(("\\.py$" . python-mode)
          ("\\.hpp$" . c++-mode)
          ("\\.h$" . c++-mode)
          ("\\.cs$" . c++-mode)
          ("\\.hs$" . haskell-mode)
          ("\\.ipp$" . c++-mode)
          ("PKGBUILD$" . sh-mode))
        auto-mode-alist))

; インデントでタブ文字を使わない
(setq-default indent-tabs-mode nil)

;; 行折り返し
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))

(global-set-key "\C-x\C-l" 'toggle-truncate-lines)

; C-tab とか C-S-tab でバッファ切り替え
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-tab] 'previous-buffer)
(global-set-key [C-iso-lefttab] 'previous-buffer)

;; クリップボード
(global-set-key "\C-c\C-c" 'clipboard-kill-ring-save)
(global-set-key "\C-c\C-y" 'clipboard-yank)

; C-←とかでWindow切り替え
(setq windmove-wrap-around t)
(global-set-key [C-right] 'windmove-right)
(global-set-key [C-left] 'windmove-left)
(global-set-key [C-up] 'windmove-up)
(global-set-key [C-down] 'windmove-down)

;; 全体を選択
(defun copy-whole-buffer ()
  (interactive)
  (save-mark-and-excursion
    (kill-ring-save (point-min) (point-max)))
  )
(global-set-key "\C-x\C-a" 'copy-whole-buffer)

; ウィンドウ切り替えを変更
(global-set-key "\C-o" 'other-window)
(global-set-key [(control shift o)] '(lambda ()
                                       (interactive)
                                       (other-window -1)))

;; C-cC-aで全範囲選択して、クリップボードにコピーする
(defun clipboard-save-whole-buffer ()
  (interactive)
  (save-mark-and-excursion
    (clipboard-kill-ring-save (point-min-marker) (point-max-marker))))
(global-set-key "\C-c\C-a" 'clipboard-save-whole-buffer)

;; emacsclientを使えるようにする
(if window-system (server-start))

;; バックアップを作らないようにする
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 左端に行番号を表示する
(setq-default global-display-line-numbers-mode t)
(global-display-line-numbers-mode t)

;; 文字コード
(set-default-coding-systems 'utf-8-unix)

;; kill emacs
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c\C-c" 'save-buffers-kill-terminal)

;; make
(global-unset-key "\C-xm")
(global-set-key "\C-xmb" '(lambda ()
                           (interactive)
                           (compile "
CFLAGS=-fdiagnostics-color=never CXXFLAGS=-fdiagnostics-color=never LDFLAGS=-fdiagnostics-color=never kati")))
(global-set-key "\C-xmt" '(lambda ()
                           (interactive)
                           (compile "make -B test")))

;; gdb
(setq-default gdb-many-window t)
(setq gdb-many-window t)

;; query-replace-regexpしようとしてasync shell commandになるのを防ぐ
(global-set-key "\M-&" 'query-replace)

;; haskell-mode-hook
(require 'haskell-mode)
(with-eval-after-load 'haskell-mode
  (turn-on-haskell-indent))

;; C-/のundoでredoしないようにする
(require 'undo-tree)
(global-set-key (if window-system [(control /)] "\C-_") 'undo-tree-undo)
(global-set-key [(control ?\\)] 'undo-tree-redo)
(global-set-key "\C-xu" 'undo-tree-visualize)

;; jaunte
(require 'ace-jump-mode)
(global-set-key (kbd "\C-z") 'ace-jump-word-mode)

;; wandbox
(require 'wandbox)
(setq wandbox-lang-alist
      '((c++-mode . "C++")
        (rust-mode . "Rust")
        (c-mode . "C")
        (java-mode . "Java")
        (php-mode . "PHP")
        (haskell-mode . "Haskell")))

(defun wandbox-compile-buffer-or-region-with-detected-lang ()
  (interactive)
  (let* ((lang (cdr (assoc major-mode wandbox-lang-alist)))
         (code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (args (append (wandbox--buffer-profile) (list :lang lang :code code))))
    (apply 'wandbox-compile args)))

;; php-mode
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "<tab>") 'deepen-line-or-region)
  (define-key php-mode-map "\C-i" 'deepen-line-or-region)
  (define-key php-mode-map "\C-m" 'newline-witless)
  (define-key php-mode-map (kbd "<backtab>") 'shallow-line-or-region)
  )

;; rust-mode
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map "\C-xw" 'wandbox-compile-buffer-or-region-with-detected-lang)
  (define-key rust-mode-map (kbd "<tab>") 'deepen-line-or-region)
  (define-key rust-mode-map "\C-i" 'deepen-line-or-region)
  (define-key rust-mode-map (kbd "<backtab>") 'shallow-line-or-region)
  (define-key rust-mode-map "\C-m" 'newline-witless)
  )

;; html-mode
(with-eval-after-load 'sgml-mode
  (define-key html-mode-map (kbd "<tab>") 'deepen-line-or-region)
  (define-key html-mode-map "\C-i" 'deepen-line-or-region)
  (define-key html-mode-map (kbd "<backtab>") 'shallow-line-or-region)
  (define-key html-mode-map "\C-m" 'newline-witless)
  )

;; js-mode
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "<tab>") 'deepen-line-or-region)
  (define-key js-mode-map "\C-i" 'deepen-line-or-region)
  (define-key js-mode-map (kbd "<backtab>") 'shallow-line-or-region)
  (define-key js-mode-map "\C-m" 'newline-witless)
  )

;; lisp-mode
(with-eval-after-load 'lisp-mode
  (electric-indent-local-mode t)
  )

;; lisp以外勝手にインデントするな
(electric-indent-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(haskell-mode-hook '(haskell-indentation-mode))
 '(markdown-command "emacs-markdown-preview")
 '(markdown-preview-delay-time 0.5)
 '(org-agenda-files '("/home/d/repos/consh/note.org"))
 '(package-selected-packages
   '(cmake-mode graphviz-dot-mode haskell-mode jaunte less lua-mode magit
                markdown-mode markdown-preview-eww
                markdown-preview-mode multi-term php-mode rust-mode
                sr-speedbar undo-tree wandbox yaml-mode))
 '(safe-local-variable-values '((c-indent-level . 4))))

(global-undo-tree-mode)

;; uim
;; (require 'uim-leim)
(require 'uim)
(when window-system
  ;; (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
  ;; (set-input-method "japanese-skk-uim")
  (global-set-key (kbd "<zenkaku-hankaku>") 'uim-mode)
  (uim-mode-on)
  )

(defun auto-uim-mode-advice (f &rest args)
  (let ((r (apply f args)))
    (if (and
         (not uim-mode)
         (not buffer-read-only)
         (not (eq major-mode 'diff-mode))
         )
        ;; (set-input-method "japanese-skk-uim")
        (uim-mode-on)
      )
    r
  ))

(advice-add 'switch-to-buffer :around 'auto-uim-mode-advice)
(advice-add 'find-file :around 'auto-uim-mode-advice)
(advice-add 'find-file-read-only :around 'auto-uim-mode-advice)

(require 'magit)

(setq markdown-preview-stylesheets nil)

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map "\C-c\C-c\C-p" 'markdown-preview-mode)
  )

;; Xmodmapで keycode 97 = yen underscore が必要
(global-set-key [(?¥)] 'self-insert-command)

;; 起動時にnote.orgを開いておく
(find-file "~/note.org")

(load-file (expand-file-name "md-slide-preview/md-slide-preview.el" (file-name-directory load-file-name)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 113 :width normal :foundry "GOOG" :family "monospace")))))

(when window-system
  (set-face-attribute 'default nil :foreground "#99ffff")
  (set-face-attribute 'default nil :background "#113311")
  )
