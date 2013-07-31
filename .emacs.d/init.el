;--------------------------------------------------------------------------
; EMACS configuration 
;--------------------------------------------------------------------------

(setq inhibit-startup-screen t)
(setq line-number-mode t)
(setq column-number-mode t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

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

(when (= emacs-major-version 23)
  (when (require 'package nil t)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
    (package-initialize)))

(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; (install-elisp-from-emacswiki "redo+.el")
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-'") 'redo))

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

;; (auto-install-batch "auto-complete development version")
(auto-install-update-emacswiki-package-name t)
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "C-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-delay 0.1))

;; (install-elisp-from-emacswiki "multi-shell.el")
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/bash")
  (global-set-key (kbd "M-s") 'multi-term))

(load-file "~/.emacs.d/public_repos/cedet-1.1/common/cedet.el")
(global-ede-mode 1)
(semantic-load-enable-code-helpers)
(global-srecode-minor-mode 1)

(if (= emacs-major-version 23)
    (when (require 'anything nil t)
      (setq
       anything-idle-delay 0.3
       anything-input-idle-delay 0.2
       anything-quick-update t
       anything-enable-shortcuts 'alphabet)
      (when (require 'anything-config nil t)
        (setq
         anything-su-or-sudo "sudo"))
      (require 'anything-match-plugin nil t)
      (when (require 'anything-complete nil t)
        (anything-lisp-complete-symbol-set-timer 150))
      (require 'anything-show-completion nil t)
      (when (require 'auto-install nil t)
        (require 'anything-auto-install nil t))
      (when (require 'descbinds-anything nil t)
        (descbinds-anything-install))
      (require 'anything-c-moccur)
      (setq moccur-split-word t)
      (global-set-key (kbd "M-s") 'anything-c-moccur-occur-by-moccur)
      (define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-from-isearch)
      (define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)
      )
  
  (when (require 'helm-config nil t)
    (global-set-key (kbd "C-c h") 'helm-mini)
    (helm-mode 1)
    (setq helm-idle-delay 0.2
          helm-input-idle-delay 0.2
          helm-quick-update t)))

(require 'egg)

;;; colorize files 
(require 'font-lock)
;; turn on font-lock mode
(global-font-lock-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)

(setq visible-bell nil)

(add-to-list 'default-frame-alist '(font . "mono:size=15"))

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

(global-set-key [delete] 'delete-char)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-c\C-w" 'font-lock-fontify-buffer)
(global-set-key "\M-\C-u" 'query-replace-regexp)
(global-set-key "\C-\\" 'toggle-input-method)
;[f1] is help for help.
(global-set-key [f2] 'call-last-kbd-macro)
(global-set-key [f3] 'find-file)
(global-set-key [f4] 'grep)
(global-set-key [f5] 'toggle-viper-mode)
(global-set-key [f11] 'kill-this-buffer)
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

(custom-set-variables
 '(user-mail-address "atakeuti2@gmail.com" t))

(custom-set-faces
  '(default ((t (:foreground "white" :background "black" :size "12pt"))) t)
)

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
(show-paren-mode t)
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
;(require 'w3m-load)


;------------------------
; Setup for python mode
;------------------------
;; (setenv "PYMACS_PYTHON" "python2.7")
;; (setq load-path
;;       (append (list nil
;; 		    "~/.emacs.d/"
;; 		    "~/.emacs.d/python-mode.el-6.1.0"
;; 		    "~/.emacs.d/Pymacs-0.25"
;; 		    "~/.emacs.d/ropemacs-0.7"
;; 		    "~/.emacs.d/ipython-0.13.1"
;; 		    )
;; 	      load-path))
;; (require 'ipython)
;; (setq py-python-command-args '( "--colors" "Linux"))
;; (require 'python-mode)
;; (require 'pymacs)
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

