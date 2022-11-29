(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)


(save-place-mode 1)
(setq inhibit-compacting-font-caches t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq default-fill-column 80)
(global-display-fill-column-indicator-mode)
(global-display-line-numbers-mode)
(global-auto-revert-mode t)


(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c I") #'find-user-init-file)

(straight-use-package 'use-package)



(use-package straight
  :custom (straight-use-package-by-default t))


(use-package orderless
  :init
  :custom (completion-styles '(orderless)))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("x" "[]" "<>"))
  :init
  (global-fira-code-mode)
  )

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t 
        modus-themes-region '(bg-only no-extend))
  (setq modus-themes-completions
        '((matches . (extrabold background intense))
          (selection . (semibold accented intense))
          (popup . (accented))))
  (setq modus-themes-hl-line '(underline accented))
  (setq modus-themes-paren-match '(bold))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;line
(use-package telephone-line
  :config
  (telephone-line-mode 1)
  )

;; (straight-use-package
;;  '(awsome=-tray
;;   :type git
;;   :host github
;;   :repo "manateelazycat/awesome-tray"
;;   )
;;   )


(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5)
                     (agenda . 5)))
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ;icons
(use-package all-the-icons
  :if (display-graphic-p))


;; (use-package eldoc
;;   :custom
;;   (eldoc-echo-area-use-multiline-p nil))

;; (use-package rg)

;; (use-package wgrep)

;; (use-package vterm)

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))     

;evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )


(define-key evil-normal-state-map (kbd "<up>") 'nope)
(define-key evil-normal-state-map (kbd "<down>") 'nope)
(define-key evil-normal-state-map (kbd "<left>") 'nope)
(define-key evil-normal-state-map (kbd "<right>") 'nope)
(define-key evil-normal-state-map  "f" 'evil-avy-goto-word-1)
;; ;; (define-key evil-visual-state-map  "f" 'evil-avy-goto-char)
;; (define-key evil-normal-state-map  "gl" 'evil-avy-goto-line)

(use-package evil-commentary
  :config
  (evil-commentary-mode)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

(use-package evil-goggles
  :custom
  (evil-goggles-duration 0.100)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; (use-package treemacs
;;   :init
;;   (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
;;   :bind
;;   ("C-x C-t" . treemacs)
;;   :config ;;   (progn
;;     (setq 
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-show-cursor                     nil
;;           treemacs-width                           28
;;     )

;;     )
;;   (treemacs-resize-icons 16)
;;   )



(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; (use-package corfu
;;   :init
;;   :config
;;   (setq corfu-auto t)
;;   (setq corfu-auto-prefix 1)
;;   (setq corfu-auto-delay 0)
;;   (setq completion-styles '(orderless-fast))
;;   (global-corfu-mode)
;;   )



(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (setq company-show-numbers t)
  )

(use-package which-key
  :init
  (which-key-mode)
  )

;; ;; (use-package recentf
;; ;;   :init
;; ;;   (recentf-mode t)
;; ;;   (setq recentf-max-saved-items 50)
;; ;;   )



(use-package restart-emacs)

(use-package vertico
  :config
  (vertico-mode)
  )

(use-package consult
  :bind (
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-x C-r" . consult-recent-file)
         )
  )


;; (use-package embark
;;   :ensure t

;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-," . embark-export)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init

;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
   :config
   (marginalia-mode)
   )

(use-package all-the-icons-completion
  :init (all-the-icons-completion-marginalia-setup))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  )

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp)))) 

;; (use-package eglot
;;   :init
;;   )
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (TeX-mode . lsp)
         (tuareg-mode . lsp)
         (haskell-mode . lsp)
         )
  :commands lsp)


(use-package lsp-haskell)


(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package lsp-pyright
  :hook (python-mode . lsp)
  )

(use-package blacken
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80)
  )


(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.venv/")
  :config
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;; (use-package magit)

;; (use-package cdlatex
;;   :straight
;;   (
;;    cdlatex
;;    :repo "cdominik/cdlatex"
;;    :host github
;;    )
;;   :hook (
;;          (LaTeX-mode . turn-on-cdlatex)
;;          )
;;   )


(use-package langtool
  :straight
  (langtool
   :repo "mhayashi1120/Emacs-langtool"
   :host github
   )
  :config
  (setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
  )


(use-package cdlatex
  :config

  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (setq cdlatex-env-alist
        '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
          ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)))
  (setq cdlatex-command-alist
        '(("axm" "Insert axiom env"   "" cdlatex-environment ("axiom") t nil)
          ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)))
  )
  
(use-package evil-tex
  :hook
  (
   (LaTeX-mode . evil-tex-mode)
   )
  )

(setq TeX-view-program-selection '((output-pdf "Okular")))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

(use-package lsp-latex
  :config
  (setq lsp-latex-build-on-save t)
  )

;; (add-hook 'cdlatex-tab-hook 'LaTeX-indent-line)
          ;; (defun cdlatex-indent-maybe ()
          ;;   (when (or (bolp) (looking-back "^[ \t]+"))
          ;;     (LaTeX-indent-line))))

;; (use-package auctex-latexmk
;;   :config
;;   (auctex-latexmk-setup)
;;   )



;; ;; (add 'LaTeX-mode-hook
;; ;;      (lamnda ()
;; ;;              ()
;; ;;              )
;; ;;      )

;; ;; (load-library "~/.emacs.d/straight/local/auxlabel/auxlabel.el")

;; ;; (use-package pdf-tools)
;; ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;; ;;       TeX-source-correlate-start-server t
;; ;;  )
;; ;; ;; revert pdf-view after compilation
;; ;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; ;; ;; (use-package auctex-latexmk
;; ;; ;;   :config
;; ;; ;;   (auctex-latexmk-setup)
;; ;; ;;   (setq auctex-latexmk-inherit-TeX-PDF-mode t)
;; ;; ;; )

;; ;; (require 'ocamlformat)
;; (add-hook 'tuareg-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'ocamlformat-before-save)))

;; (use-package flycheck-ocaml)

;; (with-eval-after-load 'merlin
;;   ;; Disable Merlin's own error checking
;;   (setq merlin-error-after-save nil)

;;   ;; Enable Flycheck checker
;;   (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook #'merlin-mode)


(use-package tree-sitter
  :hook
  (python-mode . tree-sitter-hl-mode)
  (tuareg-mode . tree-sitter-hl-mode)
  (TeX-mode . tree-sitter-hl-mode)
  ;; (haskell-mode . tree-sitter-hl-mode)
  )

;; (use-package tree-sitter-langs
;;   )

(use-package tree-sitter-langs
  :straight (tree-sitter-langs
             :host github
             :depth full
             :repo "intermet/tree-sitter-langs"
             :branch "master"
             )
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(latex-mode . latex))
 )


(use-package ocamlformat)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'ocamlformat-before-save)
            )
          )

;; (use-package cdlatex
;;   :straight
;;   (
;;    cdlatex
;;    :repo "cdominik/cdlatex"
;;    :host github
;;    )
;;   :hook (
;;          (LaTeX-mode . turn-on-cdlatex)
;;          )
;;   )


(use-package hydra)

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun find-xmonadhs ()
  (interactive)
  (find-file "~/.xmonad/xmonad.hs")
  )

(defhydra hydra-edit-config-files (:color pink
                             :hint nil)
"
^Edit^
_i_: init.el    _j_: xinit
_x_: xmonad.hs  _z_: zshrc
_b_: xmobarrc
"
  ("i" find-user-init-file :color blue)
  ("x" (find-file "~/.xmonad/xmonad.hs") :color blue)
  ("b" (find-file "~/.xmobarrc") :color blue)
  ("z" (find-file "~/.zshrc") :color blue)
  ("j" (find-file "~/.xinitrc") :color blue)
  ("q" nil :color blue)
)

(global-set-key (kbd "C-c I") 'hydra-edit-config-files/body)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" default)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
