;;; init.el --- Settings for emacs
(require 'package)

;;; Commentary:
;;; none

;;; Code:
(setq user-emacs-directory (file-truename "~/.emacs.d/")) ;Unfolds the link to user emacs directory

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Adding directories to load path
(add-to-list 'load-path "~/.emacs.d/init/")
(let ((default-directory "~/.emacs.d/other/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq visible-bell 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("732ccca2e9170bcfd4ee5070159923f0c811e52b019106b1fc5eaa043dff4030" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" default)))
 '(package-selected-packages
   (quote
    (prettier-js flx-ido helm-flx which-key powerline yaml-mode slime ensime espresso auctex yasnippet-snippets yasnippet company company-mode helm-projectile hel-projectile haskell-mode projectile tide web-mode rjsx-mode flycheck js2-mode js2-jsx-mode ag evil-collection magit helm-ag helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium-11"))
(set-face-attribute 'default t :font "Source Code Pro Medium-11")

;; show-paren-mode settings
(setq show-paren-delay 0)
(show-paren-mode 1)

(defun bind-leader-commands ()
  "Binds leader commands."
  (evil-leader/set-key "x" #'helm-M-x)
  (evil-leader/set-key "gt" 'ggtags-find-tag-dwim))

(use-package evil
  :ensure t
  :init
  (setq-default evil-shift-width 2)
  (setq-default evil-search-module 'evil-search)
  :config
  (evil-mode t)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)

    (bind-leader-commands))

  (use-package nlinum-relative
    :ensure t
    :config
    (nlinum-relative-setup-evil)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
    (add-hook 'text-mode-hook 'nlinum-relative-mode)
    (setq nlinum-relative-redisplay-delay 0)
    (setq nlinum-relative-current-symbol ""))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-gruvbox-dark-medium t))

(use-package ag
  :ensure t)

(use-package org
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-switchb)
  (evil-define-key 'motion 'org-mode-map (kbd "M-l") 'org-metaright)
  (evil-define-key 'motion 'org-mode-map (kbd "M-h") 'org-metaleft)
  (evil-define-key 'motion 'org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key 'motion 'org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key 'motion 'org-mode-map (kbd "M-u") 'org-toggle-heading)
  (add-hook 'org-mode-hook (lambda ()
                             (setq evil-auto-indent nil))))

(use-package helm
  :ensure t
  :init
  :config
  (require 'helm-config)

  ;; key maps
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (evil-define-key 'motion 'global (kbd "M-f f") #'helm-find-files)
  (evil-define-key 'motion 'global (kbd "M-f g") #'helm-find)
  (evil-define-key 'motion 'global (kbd "M-f b") #'helm-mini)
  (evil-define-key 'motion 'global (kbd "M-f r") #'helm-resume)

  (use-package helm-ag
    :ensure t
    :config
    (evil-define-key 'motion 'global (kbd "M-f a f") #'helm-do-ag)
    (evil-define-key 'motion 'global (kbd "M-f a p") #'helm-do-ag-project-root))

  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on))
    
  (helm-mode 1))

(use-package flx-ido
  :ensure t
  :config
  
  (use-package helm-flx
    :ensure t
    :after helm
    :config
    (helm-flx-mode +1)))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/sellpy/" "~/code/" "~/.emacs.configs"))
  :config
  (evil-define-key 'motion 'global (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package ggtags
  :ensure t)

(require 'fixme-mode)
(setq fixme-background-color nil)
(setq fixme-foreground-color "Yellow")
;; (add-hook 'prog-mode-hook 'fixme-mode t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (add-to-list 'safe-local-variable-values
               '(flycheck-javascript-standard-executable . "~/sellpy/sellpy/node_modules/.bin/standard"))
  (add-to-list 'safe-local-variable-values
               '(flycheck-javascript-standard-executable . "~/sellpy/admin/node_modules/.bin/eslint"))
  (add-to-list 'safe-local-variable-values
               '(flycheck-javascript-standard-executable . "~/sellpy/parsoku/node_modules/.bin/standard"))
  (add-to-list 'safe-local-variable-values
               '(flycheck-javascript-standard-executable . "~/sellpy/partner-store/node_modules/.bin/standard"))
  (setq flycheck-ghc-args '("-dynamic"))
  (evil-leader/set-key "f c n" 'flycheck-next-error)
  (evil-leader/set-key "f c p" 'flycheck-previous-error))

(use-package web-mode
  :after flycheck
  :ensure t
  :init
  (setq web-mode-enable-comment-interpolation t)
  (setq-default web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  :config
  (set-face-attribute 'web-mode-comment-keyword-face nil
                      :foreground "Yellow")
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (cond ((string-equal "jsx" (file-name-extension (buffer-file-name)))
                                    (tide-setup)
                                    (tide-hl-identifier-mode))
                                   ((string-equal "js" (file-name-extension (buffer-file-name)))
                                    (tide-setup)
                                    (tide-hl-identifier-mode))))))
  
(use-package js2-mode
  :ensure t
  :after flycheck
  :init
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-bounce-indent-p t)
  (setq js2-pretty-multiline-declarations nil)
  :config
  (setq js2-basic-offset 2)
  (set-face-attribute 'js2-function-call nil
                      :inherit 'font-lock-function-name-face)
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table)

  (require 'js2-mode-setup)
  (add-hook 'js2-mode-hook (lambda ()
                             (tide-setup)
                             (tide-hl-identifier-mode)
                             ;; Need to set auto-rescan to nil since it is slow
                             (setq imenu-auto-rescan nil)
                             (fixme-mode)
                             (js2-reparse)
                             (company-mode))))

(use-package tide
  :ensure t
  :after (typescript-mode flycheck)
  :config
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  (add-hook 'tide-mode-hook (lambda ()
                              (evil-leader/set-key "df" 'tide-jump-to-definition)
                              (evil-leader/set-key "b" 'tide-jump-back)))
  (require 'init-tide)
  (evilify-tide-jumps))

(use-package haskell-mode
  :ensure t)

(use-package rjsx-mode
  ;; This is a derived mode from js2-mode, so it inherits a lot of the configuration
  :ensure t
  :after flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rjsx-mode)))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)

  (yas-global-mode 1))

(use-package company
  :ensure t
  :init
  (setq-default company-require-match nil)
  (setq-default company-dabbrev-downcase nil)
  :config
  (evil-define-key 'insert 'company-active-map (kbd "C-n") 'company-select-next)
  (evil-define-key 'insert 'company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (evil-define-key 'insert 'company-active-map (kbd "C-<return>") 'company-complete-selection))

(use-package auctex
  :defer t
  :ensure t)

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package yaml-mode
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package prettier-js
  :ensure t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(provide 'init)
;;; init.el ends here
