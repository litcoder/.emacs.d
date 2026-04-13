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
        pyvenv
        blacken
        tramp
        highlight-indentation
        find-file-in-project
        exec-path-from-shell
        auto-complete
        lsp-mode
        lsp-ui
        rust-mode
        log4j-mode
	treemacs
        inkpot-theme
        s
        dash
        editorconfig
        company))


;; install the missing packages
(setq package-install-upgrade-built-in t)
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))


;;; **************************************************************
;;; Set UTF-8 as a default language.
;;; **************************************************************
(set-language-environment "utf-8")


;;; **************************************************************
;;; Set modes.
;;; - line numbers
;;; - company mode
;;; **************************************************************
(global-display-line-numbers-mode 1)
(add-hook 'after-init-hook 'global-company-mode)


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
;; Respect remote path.
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; **************************************************************
;;; Clang
;;; **************************************************************
(require 'clang-format)
(setq clang-format-style "file")

;;; **************************************************************
;;; rust
;;; **************************************************************
(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-?" . lsp-find-references)
              ("C-c C-c C-c" . rustic-cargo-build)
              ("C-c C-c C-k" . rustic-cargo-check)
              ("C-c C-c C-t" . rustic-cargo-test)
              ("C-c C-c C-f" . rustic-format-buffer))
  :config
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-client 'lsp-mode))

;;; **************************************************************
;;; LSP settings
;;; **************************************************************
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-symbol-highlighting t
        lsp-ui-doc-enable t
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-ui-sideline-enable t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-completion-show-detail t
        lsp-completion-show-kind t)
  (setq lsp-rust-analyzer-display-combine-imports t
        lsp-rust-analyzer-display-chaining-hints t   ; 메서드 체이닝 시 타입 표시
        lsp-rust-analyzer-display-parameter-hints t  ; 매개변수 이름 표시
        lsp-rust-analyzer-proc-macro-enable t))      ; 매크로 지원 강화

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point))



;;; **************************************************************
;;; copilot
;;; **************************************************************
(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
  ;; don't show in mode line
  :diminish)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)


;;; **************************************************************
;;; Log4j
;;; **************************************************************
(use-package log4j-mode
  :ensure t
  :disabled t
  :init
  (add-hook #'log4j-mode-hook #'view-mode)
  (add-hook #'log4j-mode-hook #'read-only-mode)
  (add-hook #'log4j-mode-hook 'eos/turn-on-hl-line))
(use-package view
  :config
  (defun View-goto-line-last (&optional line)
    "goto last line"
    (interactive "P")
    (goto-line (line-number-at-pos (point-max))))

  (define-key view-mode-map (kbd "e") 'View-scroll-half-page-forward)
  (define-key view-mode-map (kbd "u") 'View-scroll-half-page-backward)

  ;; less like
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
  (define-key view-mode-map (kbd "g") 'View-goto-line)
  (define-key view-mode-map (kbd "G") 'View-goto-line-last)
  ;; vi/w3m like
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char))


;;; **************************************************************
;;; Treemacs
;;; **************************************************************
(use-package treemacs
  :demand t
  :config
    (setq treemacs-follow-after-init t
          treemacs-is-never-other-window t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-fringe-indicator-mode t))
(global-set-key (kbd "C-M-f") 'treemacs-find-file)
(global-set-key (kbd "C-M-0") 'treemacs-select-window)



;;; **************************************************************
;;; Python formatter
;;; **************************************************************
(use-package blacken
  :ensure t
  :after python
  :commands (blacken-buffer)
  :config
  (setq blacken-line-length 88))
(global-set-key (kbd "C-c b") #'blacken-buffer)


;;; **************************************************************
;;; Theme
;;; **************************************************************
;;(load-theme 'inkpot t)



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
   '(auto-complete blacken clang-format copilot editorconfig elpy
		   exec-path-from-shell find-file-in-project
		   inkpot-theme jsonrpc kkp log4j-mode lsp-ui
		   plantuml-mode py-autopep8 python-black rust-mode
		   tramp treemacs use-package viewer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
