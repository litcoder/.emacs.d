;;; **************************************************************
;;; Environment variables.
;;; **************************************************************
(setenv "LC_CTYPE" "UTF-8")
(setenv "PATH"
        (concat
         "/usr/local/bin" ":"
         (getenv "PATH")))


;;; **************************************************************
;;; PROXY settings. - When you're behind a firewall.
;;; **************************************************************
;;; (setq url-proxy-services
;;;      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;;       ("http" . "<http_proxy_server>:<port>")
;;;       ("https" . "<https_proxy_server>:<port>")))


;;; **************************************************************
;;; Packages.
;;; **************************************************************
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;;install packages
(setq package-list
      '(
	use-package
        pyvenv
        py-autopep8
        tramp
        highlight-indentation
        find-file-in-project
        exec-path-from-shell
        auto-complete
        magit
	lsp-mode
	lsp-ui
        rust-mode))


;; install the missing packages
(setq package-install-upgrade-built-in t)
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;; **************************************************************
;;; Set UTF-8 as a default language.
;;; **************************************************************
(set-language-environment "utf-8")


;;; **************************************************************
;;; Set line number mode on.
;;; **************************************************************
(global-display-line-numbers-mode 1)


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
;;; Set column-number-mode by default.
;;; **************************************************************
(setq column-number-mode t)


;;; **************************************************************
;;; TRAMP for remote ssh access.
;;; **************************************************************
(require 'tramp)
(setq tramp-default-method "ssh")


;;; **************************************************************
;;; CTags - https://www.emacswiki.org/emacs/BuildTags
;;; **************************************************************
;; (setq path-to-ctags "/usr/bin/ctags")
(setq path-to-ctags "ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )


;;; **************************************************************
;;; Clang
;;; **************************************************************
(require 'clang-format)
(setq clang-format-style "file")

;;; **************************************************************
;;; rust
;;; **************************************************************
(require 'rust-mode)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;; **************************************************************
;;; LSP settings
;;; **************************************************************
(use-package lsp-mode
	 :commands (lsp lsp-defferred)
	 :init
	 (setq lsp-keymap-prefix "C-c l"))
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-diagnostics-provider :none)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-signature-auto-activate t)
(setq lsp-signature-render-documentation nil)
(setq lsp-completion-provider :none)
(setq lsp-completion-show-detail nil)
(setq lsp-completion-show-kind nil)


;;; **************************************************************
;;; Spelling checker. e.g) M-x flyspell-mode
;;; You may need to install spelling checker in your system
;;; before describe path to it.
;;; **************************************************************
(setq ispell-program-name "/usr/local/bin/aspell")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package lsp-ui rust-mode python-black py-autopep8 magit lsp-mode find-file-in-project exec-path-from-shell elpy clang-format auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
