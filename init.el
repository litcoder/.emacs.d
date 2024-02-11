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
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;;install packages
(setq package-list
      '(
        pyvenv
        tramp
        highlight-indentation
        find-file-in-project
        exec-path-from-shell
        auto-complete
        magit))

;; activate
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

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
;;; Indentation (No tab mode)
;;; **************************************************************
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
;;; Spelling checker. e.g) M-x flyspell-mode
;;; You may need to install spelling checker in your system
;;; before describe path to it.
;;; **************************************************************
(setq ispell-program-name "/usr/local/bin/aspell")
