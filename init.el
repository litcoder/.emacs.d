;;; **************************************************************
;;; Unicode Encoding
;;; **************************************************************
(setenv "LC_CTYPE" "UTF-8")

;;; **************************************************************
;;; PROXY settings. - When you're behind a firewall.
;;; **************************************************************
;;; (setq url-proxy-services
;;;      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;;       ("http" . "<http_proxy_server>:<port>")
;;;       ("https" . "<https_proxy_server>:<port>")))


;;; **************************************************************
;;; Set column-number-mode by default.
;;; **************************************************************
(setq column-number-mode t)

;;; **************************************************************
;;; Code style
;;;  From: https://www.emacswiki.org/emacs/IndentingC
;;;    A partial list of the better known C styles:
;;;
;;;    “gnu”: The default style for GNU projects
;;;    “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;;;    “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;;;    “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;;;    “stroustrup”: What Stroustrup, the author of C++ used in his book
;;;    “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;;;    “linux”: What the Linux developers use for kernel development
;;;    “python”: What Python developers use for extension modules
;;;    “java”: The default style for java-mode (see below)
;;;    “user”: When you want to define your own style 
;;; **************************************************************
(setq c-default-style "linux")


;;; **************************************************************
;;; Environment variables.
;;; **************************************************************
(setenv "PATH"
		(concat
		"/usr/local/bin" ":"
		(getenv "PATH")))


;;; **************************************************************
;;; Package managers.
;;; **************************************************************
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;; **************************************************************
;;; Set UTF-8 as a default language.
;;; **************************************************************
(set-language-environment "utf-8")


;;; **************************************************************
;;; Set line number mode on.
;;; **************************************************************
(global-linum-mode t)


;;; **************************************************************
;;; Indentation (No tab mode)
;;; **************************************************************
;(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)


;;; **************************************************************
;;; Splitted window resizing.
;;; **************************************************************
(global-set-key (kbd "S-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-M-<down>") 'shrink-window)
(global-set-key (kbd "S-M-<up>") 'enlarge-window)


;;; **************************************************************
;;; Autofill mode
;;; **************************************************************
(add-hook 'text-mode-hook 'auto-fill-mode)


;;; **************************************************************
;;; Recently open files.
;;; **************************************************************
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;;; **************************************************************
;;; Exuberant CTAGS.
;;; **************************************************************
;;(setq path-to-ctags "/usr/local/bin/ctags")
(setq path-to-ctags "ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )


;;; **************************************************************
;;; TRAMP for remote ssh access.
;;; **************************************************************
(require 'tramp)
(setq tramp-default-method "ssh")



;;; **************************************************************
;;; Git
;;; **************************************************************
;; (require 'git)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages (quote (markdown-mode elpy magit git))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; ;; Bind magit status
;; (global-set-key (kbd "C-x g") 'magit-status)



;;; **************************************************************
;;; Elpy
;;; **************************************************************
;;(elpy-enable)
;; CPython
;;(setq python-shell-interpreter "python"
;;      python-shell-interpreter-args "-i")
;; IPython
;;(setq python-shell-interpreter "ipython"
;;        python-shell-interpreter-args "--simple-prompt -i")

;;; **************************************************************
;;; CTags - https://www.emacswiki.org/emacs/BuildTags
;;; **************************************************************
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (plantuml-mode git go-mode tramp-theme gited))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; **************************************************************
;;; Font - Courier
;;; **************************************************************
(set-frame-font "Courier 12" nil t)

;;; **************************************************************
;;; Spelling checker. e.g) M-x flyspell-mode
;;; You may need to install spelling checker in your system
;;; before describe path to it.
;;; **************************************************************
(setq ispell-program-name "/usr/local/bin/aspell")


;;; **************************************************************
;;; Plantuml mode
;;; https://github.com/skuro/plantuml-mode
;;; **************************************************************
(setq plantuml-jar-path "~/.emacs.d/bin/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

