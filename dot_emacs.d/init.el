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

(add-hook 'compilation-mode-hook (lambda() (display-line-numbers-mode -1)))
(add-hook 'dired-mode-hook (lambda() (display-line-numbers-mode -1)))

(global-auto-revert-mode t)

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package orderless
  :init
  :custom (completion-styles '(orderless))
  :config
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  )

;; (use-package fira-code-mode
;;   :custom (fira-code-mode-disabled-ligatures '("x" "[]" "<>"))
;;   :init
;;   (global-fira-code-mode)
;;   )

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


(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  )

(use-package page-break-lines
  :config
  (global-page-break-lines-mode)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons
  :if (display-graphic-p))

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

;;; evil-mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (use-package evil-leader
    :config
    (evil-leader/set-leader "<SPC>")
    )
  (global-evil-leader-mode)
  (evil-mode 1)
  )

(use-package evil-tex
  :hook
  (
   (LaTeX-mode . evil-tex-mode)
   )
  )

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "<up>") 'nope)
  (define-key evil-normal-state-map (kbd "<down>") 'nope)
  (define-key evil-normal-state-map (kbd "<left>") 'nope)
  (define-key evil-normal-state-map (kbd "<right>") 'nope)
  (define-key evil-normal-state-map  "f" 'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map  "F" 'evil-avy-goto-word-1)
  (define-key evil-normal-state-map  "gl" 'evil-avy-goto-line)
  )

(use-package evil-commentary
  :config
  (evil-commentary-mode)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :custom
  (evil-goggles-duration 0.100)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds)
  (evil-define-key 'insert evil-multiedit-mode-map (kbd "C-n") #'evil-multiedit-next)
  (evil-define-key 'insert evil-multiedit-mode-map (kbd "C-p") #'evil-multiedit-prev)
  (evil-define-key 'insert evil-multiedit-mode-map (kbd "C-p") #'evil-multiedit-prev)
  )

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

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


(use-package marginalia
   :config
   (marginalia-mode)
   )

(use-package all-the-icons-completion
  :init (all-the-icons-completion-marginalia-setup))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :config
  (setq aw-ignore-on t)
  (setq aw-ignored-buffers popper-reference-buffers)
  )

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


(use-package lsp-haskell
  )

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
  

(setq TeX-view-program-selection '((output-pdf "Okular")))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
              #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(use-package lsp-latex
  :config
  (setq lsp-latex-build-on-save t)
  )

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

(use-package tuareg
  :hook
  (tuareg-mode . merlin-mode)
  :config
  (evil-set-initial-state 'utop-mode 'emacs)
  (add-hook 'utop-mode-hook (lambda() (display-line-numbers-mode -1)))
  )

(use-package utop
  :config
  (setq utop-command "opam exec -- dune utop . -- -emacs")
  )

;; (use-package tree-sitter
;;   :hook
;;   (python-mode . tree-sitter-hl-mode)
;;   (tuareg-mode . tree-sitter-hl-mode)
;;   (TeX-mode . tree-sitter-hl-mode)
;;   ;; (haskell-mode . tree-sitter-hl-mode)
;;   )
;; (use-package tree-sitter-fold
;;   :straight (:host github :repo "junyi-hou/tree-sitter-fold"))

;; (use-package tree-sitter-langs
;;   :straight (tree-sitter-langs
;;              :host github
;;              :depth full
;;              :repo "intermet/tree-sitter-langs"
;;              :branch "master"
;;              )
;;   :config
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(latex-mode . latex))
;;  )


(use-package ocamlformat
  :config
  (setq ocamlformat-show-errors "None")
  )

(add-hook 'tuareg-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'ocamlformat-before-save)
            )
          )


(use-package rg
  :config
  (rg-define-search ziyed/rg
    :query ask
    :format regexp
    :dir (
          let ((vc (vc-root-dir)))
           (if vc
               vc
             default-directory
               )
           )
    :flags ("--hidden -g !.git")
    )
  :bind
  ("C-c r" . ziyed/rg)
  )


(defun ziyed/dune-build ()
  (interactive)
  (let
    ((default-directory (vc-root-dir)))
    (compile "dune build bin/main.exe")
  )
  )

(defun ziyed/dune-exec ()
  (interactive)
  (let
    ((default-directory (vc-root-dir)))
    (compile "dune exe -- bin/main.exe")
  )
  )

(use-package hydra)

(defhydra ziyed/hydra-dune (:color pink
                             :hint nil)
"
^edit^
_b_: build
_x_: exec 
"
  ("b" ziyed/dune-build :color blue)
  ("x" ziyed/dune-exec :color blue)
)
(global-set-key (kbd "C-c c") 'ziyed/hydra-dune/body)



(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun find-xmonadhs ()
  (interactive)
  (find-file "~/.config/xmonad/xmonad.hs")
  )

(defhydra hydra-edit-config-files (:color pink
                             :hint nil)
"
^edit^
_i_: init.el    _j_: xinit
_x_: xmonad.hs  _z_: zshrc
_b_: xmobarrc
"
  ("i" find-user-init-file :color blue)
  ("x"  find-xmonadhs :color blue)
  ("b" (find-file "~/.xmobarrc") :color blue)
  ("z" (find-file "~/.zshrc") :color blue)
  ("j" (find-file "~/.xinitrc") :color blue)
  ("q" nil :color blue)
)

;; (use-package iedit)

(use-package window
  :straight (:type built-in)
  :custom
  (display-buffer-alist
   '(("magit"
      (display-buffer-in-side-window)
      (side . right)
      (window-width . 0.25)
      (window-parameters . ((mode-line-format . (" " "%b"))))
      ))
   )
  )


(defun ziyed/dired-vc ()
  (interactive)
  (let
  ((dir (if (vc-root-dir) (dired-noselect (vc-root-dir)) (dired-noselect default-directory))))
  (display-buffer-in-side-window
   dir
   `((side . left)
     (slot . 0)
     (window-width . 0.15)
     (window-parameters . ((mode-line-format . (" " "%b"))))
     )
   )
  (ace-window dir)
  )
  )

(use-package dired
  :straight (:type built-in)
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  ;; Colourful columns.
  (setq dired-listing-switches
        "-GFhlv --group-directories-first --time-style=long-iso")
  (setq dired-omit-files "[~#]$")
  (global-set-key (kbd "C-c d") 'ziyed/dired-vc)
  (use-package dired-subtree)
  (add-hook 'dired-mode-hook (lambda() (display-line-numbers-mode -1)))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
  )


(global-set-key (kbd "C-c I") 'hydra-edit-config-files/body)


(use-package perspective
  :bind
  ("C-x b" . persp-switch-to-buffer*)
  ;; :custom
  ;; (persp-mode-prefix-key (kbd "C-"))
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :config
  (define-key evil-normal-state-map (kbd "<SPC>p") 'perspective-map)
  )

(use-package find-file-in-project)

(use-package vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode -1)))
  )

(define-key evil-normal-state-map (kbd "<SPC>x") 'eval-buffer)

(use-package pdf-tools
  :config
  (add-hook 'doc-view-mode-hook (lambda() (display-line-numbers-mode -1)))
  )

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(use-package magit)

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
 
(use-package imenu-list)


(use-package caml)

(use-package yafolding
  :config
  (defvar yafolding-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
      (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
      map))
  )

(use-package iedit)

(use-package projectile
  :config
  (projectile-mode +1)
  )
(use-package persp-projectile)

(use-package rainbow-mode)

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

(use-package esup)

(setq org-html-validation-link nil)

(setq native-comp-async-report-warnings-errors nil)
