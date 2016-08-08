

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

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

    (global-unset-key "\C-z")))

; 行間設定
(setq-default line-spacing 0)

;; スマートインデントを殺す
(setq c-syntactic-indentation nil)

(global-set-key "\C-h" 'delete-backward-char)

;; c mode hook
(load "font-lock")
(defun my-c++-mode-hook ()
  (setq c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (setq c-auto-newline nil)
  (c-set-offset 'innamespace 0)
  (dolist (key '(";" "{" "<" ">" "," "(" ")" "\C-c\C-c" "\C-c\C-a" "\C-c\C-s"))
    (define-key c++-mode-map key nil))
  (define-key c++-mode-map [(control :)] 'c-scope-operator)
  (define-key c++-mode-map "\C-i" 'deepen-line-or-region)
  (define-key c++-mode-map (kbd "<backtab>") 'shallow-line-or-region)
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (define-key c++-mode-map "\C-xw" 'wandbox-compile-buffer-or-region-with-detected-lang)
  ;; add C++11's keywords to keyword list, and mute typename followed by eol, ::type
  (font-lock-add-keywords nil
    '(("\\<\\(typename\\)$\\|\\(\\:\\:type\\)\\>" . 'font-lock-silent-face)
      ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|export\\|final\\|noexcept\\|override\\|static_assert\\|thread_local\\)\\>" . font-lock-keyword-face)
      ("\\<\\(nullptr\\|\\(-?[[:digit:]]+\\(\\.[[:digit:]]+\\(e[+-]?[[:digit:]]+\\)?\\)?\\|0x[[:xdigit:]]+\\)\\([[:alpha:]_][[:alnum:]_]*\\)?\\)\\>" . font-lock-constant-face)
      ("\\<\\(char\\(8\\|16\\|32\\)_t\\)\\>" . font-lock-builtin-face)))
  (make-local-variable 'font-lock-maximum-decoration)
  (setcdr (assoc 'c++-mode font-lock-maximum-decoration) 2)
  (font-lock-fontify-buffer)
  (c-toggle-electric-state t))

(add-hook 'c++-mode-hook
  'my-c++-mode-hook)

(defface font-lock-silent-face
  '((t :foreground "#448844"))
  "A face for more silent words.")

(defun offset-unit ()
  (let ((e (assoc major-mode '(('c++-mode . 'c-basic-offset)
                               ('cc-mode . 'c-basic-offset)
                               ('c-mode . 'c-basic-offset)
                               ('java-mode . 'c-basic-offset)
                               ('php-mode . 'c-basic-offset)
                               ('rust-mode . 'rust-indent-offset)))))
    (if e (car e) 4)
    ))

(defun deepen-line-or-region (&optional arg region)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (use-region-p)))
  (let ((init nil)
        (beg nil)
        (indent nil)
        (blank-line nil)
        (line-end nil)
        (diff nil)
        (depth (* (offset-unit) arg)))
    (if (not region)
        (progn
          (save-excursion
            (setq init (point))
            (beginning-of-line)
            (search-forward-regexp "[[:space:]]*")
            (setq beg (match-beginning 0))
            (setq end (match-end 0))
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
          (cond
           (blank-line
            (end-of-line))
           (indent
            (goto-char init))
           (t
            (goto-char (+ init diff)))
           ))
      (indent-code-rigidly (region-beginning) (region-end) depth)
      (setq deactivate-mark nil)
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
        (search-forward-regexp "^[[:space:]]*")
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
            (string-match-p "^[[:space:]]*$" leading)))
      (if (numberp match-result)
          (delete-region (line-beginning-position) indent-pos)))
    (insert-char 10 arg)
    (insert-string indent-str)
    ))

(setq font-lock-maximum-decoration
      '((c++-mode . nil) (t . t)))

;; kill smart indentation
(setq c-syntactic-indentation nil)

(add-hook 'java-mode-hook
  '(lambda ()
     (setq c-basic-offset 4)
     (setq c-auto-newline nil)
     (setq indent-tabs-mode t)
     (setq tab-width 4)
     (define-key java-mode-map "\C-i" 'deepen-line-or-region)
     (define-key java-mode-map (kbd "<backtab>") 'shallow-line-or-region)))

; ruby-mode hook
(add-hook 'ruby-mode-hook
  '(lambda ()
     (define-key ruby-mode-map "\C-i" nil)))

(setq auto-mode-alist
      (append
        '(("\\.py$" . python-mode)
          ("\\.hpp$" . c++-mode)
          ("\\.h$" . c++-mode)
          ("\\.cs$" . c++-mode)
          ("\\.hs$" . haskell-mode)
          ("\\.ipp$" . c++-mode))
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

;; クリップボード
(global-set-key "\C-c\C-c" 'clipboard-kill-ring-save)
(global-set-key "\C-c\C-y" 'clipboard-yank)

; C-←とかでWindow切り替え
(setq windmove-wrap-around t)
(global-set-key [C-right] 'windmove-right)
(global-set-key [C-left] 'windmove-left)
(global-set-key [C-up] 'windmove-up)
(global-set-key [C-down] 'windmove-down)

; キーボードマクロ連続再生
(require 'easy-mmode)
(easy-mmode-define-minor-mode repeat-kbd-macro-mode
                              "keyboard macro repetition mode."
                              nil
                              nil
                              '(("\C-r" . call-last-kbd-macro)
                                ("\C-g" . repeat-kbd-macro-mode)))

(global-set-key "\C-c\C-r" (lambda ()
                             (interactive)
                             (repeat-kbd-macro-mode)
                             (call-last-kbd-macro)))

; ウィンドウ切り替えを変更
(global-set-key "\C-o" 'other-window)
(global-set-key [(control shift o)] '(lambda ()
                                       (interactive)
                                       (other-window -1)))

;; term-modeでterminalのkill-ringを変更する際emacs側のも変更する

(defun backward-kill-word-with-term ()
  (interactive)
  (save-excursion
    (let ((begin (point)))
      (backward-word)
      (kill-ring-save begin (point))))
  (term-send-backward-kill-word))

(defun kill-word-with-term ()
  (interactive)
  (save-excursion
    (let ((begin (point)))
      (forward-word)
      (kill-ring-save begin (point))))
  (term-send-forward-kill-word))

(defun kill-line-with-term ()
  (interactive)
  (save-excursion
    (let ((begin (point)))
      (move-end-of-line 1)
      (kill-ring-save begin (point))))
  (term-send-raw-string "\C-k"))

(defun undo-with-term ()
  (interactive)
  (term-send-raw-string "\C-_"))

(global-set-key "\C-xt" 'multi-term)

(add-hook 'term-mode-hook
  '(lambda ()
     (define-key term-mode-map "\C-c\C-l" 'term-mode-switch)
     (define-key term-raw-map "\C-c\C-l" 'term-mode-switch)
     (define-key term-raw-map (kbd "M-DEL") 'backward-kill-word-with-term)
     (define-key term-raw-map [(control backspace)] 'backward-kill-word-with-term)
     (define-key term-raw-map "\M-d" 'kill-word-with-term)
     (define-key term-raw-map "\C-k" 'kill-line-with-term)
     (define-key term-raw-map [(control /)] 'undo-with-term)
     (define-key term-raw-map "\C-y" 'term-paste)
     (define-key term-raw-map "\C-o" 'other-window)
     (define-key term-raw-map "\C-q" 'quoted-insert)
     (define-key term-raw-map "\C-g" 'keyboard-quit)))

;; 起動時にnoteを開いておく
(find-file "~/note")

;; C-cC-aで全範囲選択して、クリップボードにコピーする
(global-set-key "\C-c\C-a"
  '(lambda ()
     (interactive)
     (save-excursion
       (clipboard-kill-ring-save (point-min-marker) (point-max-marker)))))

;; emacsclientを使えるようにする
(if window-system
    (server-start))

;; バックアップを作らないようにする
(setq make-backup-files nil)

;; 左端に行番号を表示する
(setq-default global-linum-mode t)
(global-linum-mode t)

;; 文字コード
(set-default-coding-systems 'utf-8-unix)

;; make
(global-set-key "\C-xm" '(lambda () (interactive) (compile "make -B")))

;; gdb
(setq-default gdb-many-window t)
(setq gdb-many-window t)

;; query-replace-regexpしようとしてasync shell commandになるのを防ぐ
(global-set-key "\M-&" 'query-replace)

;; terminal
(require 'multi-term)

(defun term-mode-switch ()
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (if (term-in-line-mode)
        (term-char-mode))))

(setq multi-term-program "/bin/zsh")
(setq multi-term-buffer-name "term")
(add-to-list 'term-unbind-key-list "M-x")

(setq system-uses-terminfo nil)

;; haskell-mode-hook
(require 'haskell-mode)
(add-hook 'haskell-mode-hook
          '(lambda ()
             (turn-on-haskell-indent)))

;; C-/のundoでredoしないようにする
(require 'undo-tree)
(global-set-key (if window-system [(control /)] "\C-_") 'undo-tree-undo)
(global-set-key "\C-\\" 'undo-tree-redo)
(global-set-key "\C-xu" 'undo-tree-visualize)

;; auto-complete
(require 'auto-complete-config)

(ac-config-default)
(define-key ac-mode-map [(control .)] 'auto-complete)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-menu-map "\C-n" 'ac-next)
(ac-set-trigger-key "TAB")

(setq ac-auto-start nil)
(setq ac-trigger-commands (append (list 'delete-char 'backward-char) ac-trigger-commands))

(add-hook 'c++-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (make-local-variable 'ac-trigger-commands)
            (setq ac-trigger-commands (append (list 'c-scope-operator)) ac-trigger-key-commands)
            ))

;; jaunte
(require 'jaunte)
(global-set-key "\C-c\C-s" 'jaunte)

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
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (define-key php-mode-map (kbd "<tab>") 'deepen-line-or-region)
            (define-key php-mode-map "\C-i" 'deepen-line-or-region)
            (define-key php-mode-map (kbd "<backtab>") 'shallow-line-or-region)))

;; sr-speedbar
(require 'sr-speedbar)

;; rust-mode
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (make-local-variable 'electric-indent-mode)
            (make-local-variable 'electric-indent-chars)
            (setq electric-indent-mode nil)
            (setq electric-indent-chars nil)
            (define-key rust-mode-map "\C-xw" 'wandbox-compile-buffer-or-region-with-detected-lang)
            (define-key rust-mode-map (kbd "<tab>") 'deepen-line-or-region)
            (define-key rust-mode-map "\C-i" 'deepen-line-or-region)
            (define-key rust-mode-map (kbd "<backtab>") 'shallow-line-or-region)
            (define-key rust-mode-map "\C-m" 'newline-witless)
            ))
