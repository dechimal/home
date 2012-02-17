
(setq load-path
  (append '("~d/.emacs.d/auto-install"
            "~d/.emacs.d"
            "/usr/share/emacs/site-lisp/yas") load-path))
(require 'redo+)
(require 'tramp)

; auto-install
(require 'auto-install)
(setq auto-install-directory "~d/.emacs.d/auto-install")

; tool bar, menu bar
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
    (set-face-foreground 'font-lock-string-face "chocolate1")
    (set-face-foreground 'font-lock-keyword-face "yellow2")
    (set-face-foreground 'font-lock-function-name-face nil)
    (set-face-foreground 'font-lock-builtin-face "yellow2")
    (set-face-foreground 'font-lock-preprocessor-face "#ff66ff")
    (set-face-foreground 'font-lock-type-face "gold")
    (set-cursor-color "#ffccdd")

    (set-fontset-font (frame-parameter nil 'font) '(#x80 . #x7ffff) "Migu 2M-10" nil)

    (global-unset-key "\C-z")))

; 行間設定
(setq-default line-spacing 0)

;; c mode hook
(add-hook 'c++-mode-hook
  '(lambda ()
     (setq c-set-style "stroustrup")
     (setq c-basic-offset 4)
     (setq c-auto-newline nil)
     (c-set-offset 'innamespace 0)
     (dolist (key '(";" "{" "<" ">" "," "(" ")"))
       (define-key c++-mode-map key nil))
     (define-key c++-mode-map [(control :)] 'c-scope-operator)
     (define-key c++-mode-map "\C-m" 'newline-and-indent)
     (define-key c++-mode-map "\C-c\C-c" nil)
     (define-key c++-mode-map "\C-c\C-a" nil)
     (c-toggle-electric-state t)))

(add-hook 'java-mode-hook
  '(lambda ()
     (setq c-basic-offset 4)
     (setq c-auto-newline nil)))

; ruby-mode hook
(add-hook 'ruby-mode-hook
  '(lambda ()
     (define-key ruby-mode-map "\C-i" nil)))

(setq auto-mode-alist
      (append
        '(("\\.py$" . python-mode)
	  ("\\.hpp$" . c++-mode)
          ("\\.h$" . c++-mode)
          ("\\.hs$" . haskell-mode))
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

;; C-/のundoでredoしないようにする
(global-set-key [(control /)] 'undo-only)
(global-set-key "\C-\\" 'redo)

; C-tab とか C-S-tab でバッファ切り替え

(setq enable-other-buffer-select nil)
(defun other-buffer-select
  (interactive)
  (setq enable-other-buffer-select (not enable-other-buffer-select)))

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
                             (start-kbd-macro)
                             (repeat-kbd-macro-mode)
                             (call-last-kbd-macro)))

; いらんし打ち間違えたらうざいので削除
(global-unset-key "\C-o")

; 範囲をコメント化/非コメント化
(global-set-key [(control c)(control /)] 'comment-or-uncomment-region)

; terminal
(require 'multi-term)
(setq system-uses-terminfo nil)

(defun term-mode-switch ()
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (if (term-in-line-mode)
        (term-char-mode))))

(setq multi-term-program "/bin/zsh")
(setq multi-term-buffer-name "term")
(add-to-list 'term-unbind-key-list "M-x")

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
  '(lambda term-mode-hook-fun ()
     (define-key term-mode-map "\C-c\C-l" 'term-mode-switch)
     (define-key term-raw-map "\C-c\C-l" 'term-mode-switch)
     (define-key term-raw-map [(meta backspace)] 'backward-kill-word-with-term)
     (define-key term-raw-map [(control backspace)] 'backward-kill-word-with-term)
     (define-key term-raw-map "\M-d" 'kill-word-with-term)
     (define-key term-raw-map "\C-k" 'kill-line-with-term)
     (define-key term-raw-map [(control /)] 'undo-with-term)
     (define-key term-raw-map "\C-y" 'term-paste)))

; haskell-mode-hook
(require 'haskell-mode)
(add-hook 'haskell-mode-hook
  '(lambda ()
     (turn-on-haskell-indent)))

; 起動時にnoteを開いておく
(find-file "~d/note")

; C-cC-aで全範囲選択して、クリップボードにコピーする
(global-set-key "\C-c\C-a"
  '(lambda ()
     (interactive)
     (save-excursion
       (clipboard-kill-ring-save (point-min-marker) (point-max-marker)))))

; emacsclientを使えるようにする
(if window-system
    (server-start))

; バックアップを作らないようにする
(setq make-backup-files nil)

; 左端に行番号を表示する
(setq-default global-linum-mode t)
(global-linum-mode t)

; anything
; (require 'anything-startup)

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory '("~d/.emacs.d/yas-snippets"))
(setq yas/trigger-key nil)
(mapc 'yas/load-directory yas/root-directory)

; auto-complete
; auto-complete-clangをyaourtからインストールしたために
; clangとauto-completeもpacmanでインストールされたけど気にするな
(require 'auto-complete-config)

(ac-config-default)
(define-key ac-mode-map [(control .)] 'auto-complete)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-menu-map "\C-n" 'ac-next)
(ac-set-trigger-key "TAB")

(setq ac-clang-executable "~d/progs/bin/clang++")
(setq ac-clang-flags '("-I." "-std=c++0x"))

(add-hook 'c++-mode-hook
  (lambda ()
    (make-local-variable 'ac-sources)
    (make-local-variable 'clang-completion-flags)
    (make-local-variable 'ac-trigger-commands)
    (setq ac-trigger-commands (append (list 'c-scope-operator)) ac-trigger-key-commands)
    ; (setq clang-completion-flags "-std=c++0x -I. -I$HOME/repos/boost")
    ; (define-key ac-mode-map [(control .)] 'ac-complete-clang)
    ))

(setq ac-auto-start nil)
(setq ac-trigger-commands (append (list 'delete-char 'backward-char) ac-trigger-commands))
; (setq clang-executable "/usr/bin/clang++")
