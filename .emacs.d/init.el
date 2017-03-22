;--------------------------------------------------------------------------
; EMACS configuration 
;--------------------------------------------------------------------------


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-screen t)
(setq line-number-mode t)
(setq column-number-mode t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

(fset 'yes-or-no-p 'y-or-n-p)

(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")) 

(require 'helm-config)

(setq
 ; delay time for show suggested list. (default 0.5)
 helm-idle-delay 0.01
 ; delay time from typing to re-painting. (default 0.1)
 helm-input-idle-delay 0.1
 ; max suggestion (default 50)
 helm-candidate-number-limit 30
 ; boost when many suggestion exist.
 helm-quick-update t
 ; use alphabet for shortcut of suggestion
 ; helm-enable-shortcuts 'alphabet
 )

(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(setq kill-ring-max 20)
(setq helm-kill-ring-threshold 5)
(define-key global-map (kbd "C-x b") 'helm-mini)
(define-key global-map (kbd "M-x") 'helm-M-x)

;; install package undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "C-'") 'undo-tree-redo)

;; (install-elisp-from-emacswiki "color-moccur.el")
;; (install-elisp-from-emacswiki "moccur-edit.el")
(when (require 'color-moccur nil t)
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  (setq moccur-split-word t)
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (require 'moccur-edit nil t))

(require 'wdired)
(define-key dired-mode-map "r"
  'wdired-change-to-wdired-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; install package recentf-ext
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; install package key-chord
(when (require 'key-chord nil t)
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-mode 1))

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/public_repos/yasnippet/snippets"))
(yas-global-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet undo-tree sequential-command redo+ recentf-ext paredit open-junk-file moccur-edit key-chord helm-descbinds auto-complete auto-async-byte-compile)))
 '(user-mail-address "atakeuti2@gmail.com")
 '(yas-prompt-functions (quote (my-yas/prompt)))
 '(yas-trigger-key "TAB"))
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(eval-after-load "helm-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (helm-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*helm yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key helm-command-map (kbd "y") 'yas/insert-snippet)))

;; snippet-mode for *.yasnippet files
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; (auto-install-batch "auto-complete development version")
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (setq ac-auto-start 2)
  (ac-config-default)
  (setq ac-delay 0.1)
  (setq ac-auto-show-menu 0.2)
  (define-key ac-completing-map (kbd "<C-tab>") 'ac-stop))

;; (install-elisp-from-emacswiki "multi-term.el")
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/bash")
  (global-set-key (kbd "M-s") 'multi-term)
  (add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-t") 'other-window))))

;; (auto-install-batch "sequential-command")
(when (require 'sequential-command-config nil t)
  (sequential-command-setup-keys))

;(require 'egg)

;;; colorize files 
(require 'font-lock)
;; turn on font-lock mode
(global-font-lock-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)

(setq visible-bell nil)

(add-to-list 'default-frame-alist '(font . "mono:size=14"))

(setq-default indent-tabs-mode nil)

(when (require 'mozc nil t)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)

  (setq mozc-cursor-color "lightblue")
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color mozc-cursor-color)))
  (add-hook 'input-method-inactivate-hook
            (lambda () (set-cursor-color default-cursor-color))))

(setq default-cursor-color "white")
;(set-cursor-color default-cursor-color)
(set-face-background 'cursor default-cursor-color)

(cua-mode t)
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "<C-kp-enter>") 'cua-set-rectangle-mark)

(global-set-key [delete] 'delete-char)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-c\C-w" 'font-lock-fontify-buffer)
(global-set-key "\M-\C-u" 'query-replace-regexp)
(global-set-key (kbd "C-c \\") 'toggle-input-method)
;[f1] is help for help.
(global-set-key [f2] 'call-last-kbd-macro)
(global-set-key [f12] 'new-frame)
(global-set-key [pause] 'advertised-undo)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x C-a") 'beginning-of-buffer)
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(define-key key-translation-map [?\C-h] [?\C-?])



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "white" :background "black" :size "12pt")))))

(autoload 'qt-pro-mode "qt-pro" "Qt project file editing mode" t)
(setq auto-mode-alist
      (append (list (cons "\\.\\(pro\\|pri\\)$" 'qt-pro-mode))
              auto-mode-alist))

(delete-selection-mode 1)

(autoload 'cperl-mode
  "cperl-mode"
  "alternate mode for editing Perl programs" t)
(autoload 'tcl-mode "tcl" "tcl editing mode" t)
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (set-face-foreground 'cperl-hash-face "green")
               (set-face-bold-p 'cperl-hash-face nil)
               (set-face-italic-p 'cperl-hash-face nil)
               (set-face-background 'cperl-hash-face "black")
               )))
(setq auto-mode-alist
      (append (list (cons "\\.\\(pl\\|pm\\)$" 'cperl-mode))
              auto-mode-alist))
(setq auto-mode-alist
      (append (list (cons "\\.\\(tcl\\|tvf\\)$" 'tcl-mode))
              auto-mode-alist))
(setq auto-mode-alist
      (append (list (cons "\\.\\(xml\\|xsd\\)$" 'nxml-mode))
              auto-mode-alist))

(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

(setq auto-mode-alist
      (append (list (cons "\\.\\(cpp\\|h\\)$" 'c++-mode))
              auto-mode-alist))
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (progn
	       (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
					      "\\|protected slot\\|private\\|private slot"
					      "\\)\\>"))
	       (setq c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
					      "\\|public slots\\|protected slots\\|private slots"
					      "\\)\\>[ \t]*:"))
	       (make-face 'qt-keywords-face)
	       (set-face-foreground 'qt-keywords-face "LightGreen")
	       ;; qt keywords
	       (font-lock-add-keywords 'c++-mode
				       '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
	       (font-lock-add-keywords 'c++-mode
				       '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
	       (font-lock-add-keywords 'c++-mode
				       '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
	       (font-lock-add-keywords 'c++-mode
				       '(("\\<Qt[A-Za-z]*" . 'qt-keywords-face)))
	       (c-set-offset 'innamespace 0)
	       ;(c-set-offset 'access-label -1)
	       ;(c-set-offset 'case-label +1)
	       ;(c-set-offset 'statement-case-intro +1)
	       ;(c-set-offset 'statement-case-open +1)
	       (setq c-basic-offset 4)
               (setq semantic-mode 1)
               (add-to-list 'ac-sources 'ac-source-semantic)
               ;; (setq semantic-default-submodes 
               ;;       '(
               ;;         global-semantic-idle-scheduler-mode 
               ;;         global-semantic-idle-completions-mode
               ;;         global-semanticdb-minor-mode
               ;;         global-semantic-decoration-mode
               ;;         global-semantic-highlight-func-mode
               ;;         global-semantic-stickyfunc-mode
               ;;         global-semantic-mru-bookmark-mode
               ;;         ))
               (setq qt4-base-dir (getenv "QTDIR"))
               (semantic-add-system-include qt4-base-dir 'c++-mode)
               (semantic-add-system-include (concat qt4-base-dir "QtCore") 'c++-mode)
               (semantic-add-system-include (concat qt4-base-dir "QtGui") 'c++-mode)
               (semantic-add-system-include (concat qt4-base-dir "QtTest") 'c++-mode)
               (semantic-add-system-include (concat qt4-base-dir "QtXml") 'c++-mode)
               (semantic-add-system-include (concat qt4-base-dir "QtXmlPatterns") 'c++-mode)
               (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
               (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
               (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
               (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
	       )))

(add-hook 'c-mode-hook
	  '(lambda()
	     (progn
	       (setq tab-width 3)
	       (setq c-basic-offset 3))))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq auto-fill-mode -1)
            (setq nxml-slash-auto-complete-flag t)
	    (setq nxml-child-indent 2)
            (define-key nxml-mode-map (kbd "M-h") 'backward-kill-word)))
(when (require 'w3m-load nil t)
  (global-set-key (kbd "C-c w") 'w3m-search)
  (setq w3m-home-page "http://www.google.co.jp")
  (add-hook 'w3m-mode-hook
            (lambda ()
              (define-key w3m-mode-map (kbd "C-t") 'other-window)
              (define-key w3m-mode-map (kbd "<down>") 'next-line)
              (define-key w3m-mode-map (kbd "<up>") 'previous-line)
              (define-key w3m-mode-map (kbd "<left>") 'backward-char)
              (define-key w3m-mode-map (kbd "<right>") 'forward-char)
              (define-key w3m-mode-map (kbd "<C-down>") 'w3m-next-anchor)
              (define-key w3m-mode-map (kbd "<C-up>") 'w3m-previous-anchor)
              (define-key w3m-mode-map (kbd "<C-left>") 'w3m-view-previous-page)
              (define-key w3m-mode-map (kbd "<C-right>") 'w3m-view-this-url))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; From emacs mail magazine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; install package open-junk-file

;; (require 'open-junk-file)
;; (setq open-junk-file-format "~/emacsjunk/%Y/%m/%Y-%m-%d-%H%M%S.")
;; (global-set-key (kbd "C-x C-z") 'open-junk-file) 

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")
(find-function-setup-keys)
