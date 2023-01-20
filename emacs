;; =======================
;; Visual

(when (display-graphic-p)
    (tool-bar-mode -1)                  ; Disable the button bar atop screen
    (scroll-bar-mode -1)                ; Disable scroll bar
    (setq inhibit-startup-screen t)     ; Disable startup screen with graphics
)

(setq tab-width 4)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

(if (eq system-type 'darwin)
	(set-frame-font "Akkurat Mono 14")
;;	(set-frame-font "Iosevka Light 13")
)

;; =======================
;; Package management

(setq package-list '(
		     ;; Core
		     org
		     evil
		     use-package

		     ;; Themes, visual
		     dashboard
		     recentf
		     all-the-icons
		     zerodark-theme
		     solarized-theme
		     monokai-theme
		     doom-modeline

		     ;; Tools
		     magit
		     git-gutter
		     json-reformat
		     exec-path-from-shell
		     clang-format
		     restclient

		     ;; Org, projects and navigation
		     ibuffer
		     neotree
		     helm
		     projectile
		     go-projectile

		     ;; LSP and auto complete
		     lsp-mode
		     lsp-ui
		     auto-complete
		     company

		     ;; YASnippet
		     yasnippet
		     yasnippet-snippets

		     ;; Checks
		     flycheck
		     wucuo

		     ;; Language modes
		     go-mode
		     elixir-mode
		     rust-mode
		     python-mode
		     yaml-mode
		     markdown-mode
		     json-mode
		     graphviz-dot-mode
		     dockerfile-mode
		     slime

		     ;; lintes et al
		     rustic
		     flycheck-golangci-lint

		     ;; IM
		     erc
		     )
      )


; list the repositories containing them
(setq package-archives '(
			("melpa" . "http://melpa.org/packages/")
			("elpa" . "http://tromey.com/elpa/")
			("gnu" . "http://elpa.gnu.org/packages/")
			("melpa stable" . "http://stable.melpa.org/packages/")
))

; activate all the packages (in particular auto loads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))


;; =======================
;; Backup files

(setq
	backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
	backup-by-copying t      ; don't clobber symlinks
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t)       ; use versioned backups


;; =======================
;; Packages specific setup
;; =======================

;; comments
(global-set-key (kbd "s-/") 'comment-line)

;; =======================
;; Helm

(require 'helm-config)
(require 'helm)


(global-set-key (kbd "C-c f r") 'xref-find-references)

;; =======================
;; Evil mode

;; Install Evil and disable C-i to jump forward to restore TAB functionality in Org mode.
(use-package evil
             :init (setq evil-want-C-i-jump nil)
             :config (evil-mode)
)

(eval-after-load "evil"
	'(progn
		(define-key evil-normal-state-map (kbd "C-w <left>") 'evil-window-left)
		(define-key evil-normal-state-map (kbd "C-w <down>") 'evil-window-down)
		(define-key evil-normal-state-map (kbd "C-w <up>") 'evil-window-up)
		(define-key evil-normal-state-map (kbd "C-w <right>") 'evil-window-right)

		;; Convinient shortcuts for split
		(define-key evil-normal-state-map (kbd "C--")
			(lambda ()
			(interactive)
			(split-window-vertically)
			(other-window 1))
		)

		(define-key evil-normal-state-map (kbd "C-|")
			(lambda ()
			(interactive)
			(split-window-horizontally)
			(other-window 1)))
		)
)

;; EPA
;; (setq epg-gpg-program "gpg2")
(setq epa-pinentry-mode 'loopback)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(evil-set-initial-state 'ibuffer-mode 'normal)

;; =======================
;; Projectile

(require 'projectile)
(setq projectile-completion-system 'helm)
(projectile-mode 1)


(global-set-key (kbd "C-c p p") 'projectile-switch-project)
(global-set-key (kbd "C-c p f") 'projectile--find-file)
(global-set-key (kbd "C-c p s") 'projectile-grep)

;; =======================
;; ibuffer

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; bind ':ls' command to 'ibuffer instead of 'list-buffers
(evil-ex-define-cmd "ls" 'helm-mini)


;; =======================
;; Recentf

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 35)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; =======================
;; Startup dashboard

(require 'dashboard)
(dashboard-setup-startup-hook)


;; =======================
;; Neo tree

(require 'neotree)
(define-key evil-motion-state-map (kbd "C-n") 'neotree-toggle)
(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
(local-set-key (kbd "C-n") 'neotree-toggle)
(setq neo-smart-open t)
;;(setq projectile-switch-project-action 'neotree-projectile-action)


;; =======================
;; Go mode

;; Install:
;; go get golang.org/x/tools/gopls@latest
;; go get -u golang.org/x/tools/cmd/goimports
;; Snag the user's PATH and GOPATH
(exec-path-from-shell-initialize)

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
    (exec-path-from-shell-copy-env "GOPATH") ;
    (exec-path-from-shell-copy-env "GOROOT") ; This is important for some tools gopls

    (require 'go-projectile)

    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")                ; gofmt uses invokes goimports

    ;; lsp-ui jump key binding
    (define-key evil-motion-state-map (kbd "C-]") 'lsp-ui-peek-find-definitions)
    (local-set-key (kbd "C-]") 'lsp-ui-peek-find-definitions)

    (define-key evil-motion-state-map (kbd "s-]") 'lsp-ui-peek-find-definitions)
    (local-set-key (kbd "s-]") 'lsp-ui-peek-find-definitions)

    (define-key evil-motion-state-map (kbd "s-[") 'lsp-ui-peek-jump-backward)
    (local-set-key (kbd "s-[") 'lsp-ui-peek-jump-backward)

    ;; This one is for org-capture
    (unbind-key "C-c C-j" go-mode-map)
    (local-set-key (kbd "C-c C-j") 'org-capture)
)

;; Elixir
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
		  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(add-hook 'go-mode-hook 'my-go-mode-hook)



;; rust mode
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; (setq rust-format-on-save t)


;; =======================
;; Org Mode
;; C-c / for search

(require 'org)

;; RETURN will follow links in org-mode files
(setq org-return-follows-link t)

;; When set to t, Org assumes that you write outlines by indenting
;; text in each node to align with the headline, after the stars.
(setq org-adapt-indentation t)

;; Protects against accidental removal of folded entries
(setq-default org-catch-invisible-edits 'smart)
(setq org-ctrl-k-protect-subtree t)
(setq org-catch-invisible-edits 'smart)

(setq org-log-done t)
(setq org-directory "~/Library/CloudStorage/Dropbox/Org/")

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@)" "PAUSED(p)" "|" "DONE(d)" "CANCELED(c@)")))

;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; use pretty things for the clocktable
(setq org-pretty-entities t)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Do not prompt to resume an active clock, just resume it
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq org-edit-src-content-indentation 2)
(setq org-src-preserve-indentation nil)

;;  http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
(defun oxeof/org-clock-in-if-starting ()
  "Clock in when the task is marked IN-PROGRESS."
  (when (and (string= org-state "IN-PROGRESS")
	     (not (string= org-last-state org-state)))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
	  'oxeof/org-clock-in-if-starting)
(defadvice org-clock-in (after oxeof activate)
	   "Set this task's status to 'IN-PROGRESS'."
	   (org-todo "IN-PROGRESS"))

(defun oxeof/org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING or PAUSED."
  (when (and (or (string= org-state "WAITING")
		 (string= org-state "PAUSED")
		 (string= org-state "CANCELED"))

	     (equal (marker-buffer org-clock-marker) (current-buffer))
	     (< (point) org-clock-marker)
	     (> (save-excursion (outline-next-heading) (point))
		org-clock-marker)
	     (not (string= org-last-state org-state)))
    (org-clock-out)))
(add-hook 'org-after-todo-state-change-hook
	  'oxeof/org-clock-out-if-waiting)

(setq org-time-clocksum-format '(:hours "%dh" :require-hours t :minutes ":%02dm" :require-minutes t))

;; Replace \emsp to better indents
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str " "))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(
	("t" "Todo" entry (file+headline ,(concat org-directory "tasks.org") "Tasks")
         "* TODO %?\n  %U\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")
	("n" "Note" entry (file+headline ,(concat org-directory "notes.org") "Notes")
         "* %? %^G\n%U" :empty-lines 1)
	)
)

(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

;; Flycheck
;;
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
)

;; =======================
;; Rest Client
(use-package restclient
  :ensure t
)

;; Elixir

(use-package elixir-mode
  :ensure t
)

;; LSP Mode
;;

;;(defun lsp-tramp-connection@override (local-command &optional generate-error-file-fn)
;;    "Create LSP stdio connection named name.
;;LOCAL-COMMAND is either list of strings, string or function which
;;returns the command to execute."
;;    (defvar tramp-connection-properties)
;;    (list :connect (lambda (filter sentinel name environment-fn)
;;                     (let* ((final-command (lsp-resolve-final-function
;;                                            local-command))
;;                            (process-name (generate-new-buffer-name name))
;;                            (stderr-buf (format "*%s::stderr*" process-name))
;;                            (err-buf (generate-new-buffer stderr-buf))
;;                            (process-environment
;;                             (lsp--compute-process-environment environment-fn))
;;                            (proc (make-process
;;                                   :name process-name
;;                                   :buffer (format "*%s*" process-name)
;;                                   :command final-command
;;                                   :connection-type 'pipe
;;                                   :coding 'no-conversion
;;                                   :noquery t
;;                                   :filter filter
;;                                   :sentinel sentinel
;;                                   :stderr err-buf
;;                                   :file-handler t)))
;;                       (cons proc proc)))
;;          :test? (lambda () (-> local-command lsp-resolve-final-function
;;                                lsp-server-present?))))
;;(advice-add 'lsp-tramp-connection :override #'lsp-tramp-connection@override)
;;
;;(executable-find "clangd")
(setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
(use-package lsp-mode
  :ensure t
  :hook ((
	  c-mode          ; clangd
	  c++-mode        ; clangd
	  c-or-c++-mode   ; clangd
	  go-mode         ; gopls
	  rust-mode       ; rust-analyze
	  ) . lsp-deferred)

 ;; :init
 ;; (add-hook 'go-mode-hook #'lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  ;; use flycheck, not flymake
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-restart 'auto-restart)
  (setq lsp-log-io t)
  (setq read-process-output-max (* 100 1024 1024)) ;; 100MB
  (setq lsp-clients-clangd-args '("-j=7"
                            "--background-index"
                            "--clang-tidy"
                            "--completion-style=detailed"
                            "--suggest-missing-includes"
                            "--header-insertion=never"))
  (lsp-register-client
      (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
				   :major-modes '(c-mode c++-mode)
				   :remote? t
				   :server-id 'clangd-remote)
  )
  (setq lsp-enable-file-watchers nil)
  )

;; LSP UI
;;
(use-package lsp-ui
  :requires lsp-mode flycheck
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-childframe nil)
  (setq lsp-ui-doc-position nil)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-list-position 'right)
  (setq lsp-ui-flycheck-live-reporting t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

;; This breaks ESC sequences
(defun my-c-mode-hook ()
	(define-key c-mode-map (kbd "C-[") 'evil-jump-backward)
	(define-key c++-mode-map (kbd "C-[") 'evil-jump-backward)
)
;;(add-hook 'c-mode-hook 'my-c-mode-hook)
;;(add-hook 'c++-mode-hook 'my-c-mode-hook)



;; Using golangci-lint in addition to flycheck
(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup)
  )

;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
	  (funcall fn checker property))
  ) 

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
		  (lambda ()
			(when (derived-mode-p 'go-mode)
			  (setq my/flycheck-local-cache '((lsp . ((next-checkers . (golangci-lint)))))))))


(use-package company
  :ensure t
  :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations nil)
)

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :commands yas-reload-all
)

;; =======================
;; YAML mode
(add-hook 'yaml-mode-hook
  (lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))


;; =======================
;; Spell check
(add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--camel-case" "--lang=en_US" "--run-together" "--run-together-limit=16"))


(dolist (my-spell-hook '(text-mode-hook
				org-mode-hook
				prog-mode-hook
				)
		)
(add-hook my-spell-hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined))
)


;; Rust
;; (use-package rustic)


(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4)
  )

;; Lisp
(setq inferior-lisp-program "sbcl")

;; =======================
;; Theme
;; Don't forget to run `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :ensure t
)
;;(load-theme 'solarized-dark t)
;; (load-theme 'zerodark t)
(load-theme 'monokai t)


(setq monokai-height-minus-1 0.8
      monokai-height-plus-1 1.1
      monokai-height-plus-2 1.15
      monokai-height-plus-3 1.2
      monokai-height-plus-4 1.3)


;; Modeline
;;(zerodark-setup-modeline-format)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (display-battery-mode t)
)

;; =======================
;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
)

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode)
  :config
  (setq
   git-gutter:modified-sign "**"
   git-gutter:added-sign "++"
   git-gutter:deleted-sign "--"
   git-gutter:update-interval 2
   )
)


;; ERC
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(load "~/.ercpass")

(global-set-key "\C-ci" (lambda () (interactive)
                           (erc :server "irc.libera.chat"
                                :port "6667"
                                :nick "bdev")))

;;(setq erc-nickserv-passwords
;;      `(Libera.Chat (bdev . ,libera-password)))

(setq erc-log-channels-directory "~/.emacs.d/logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)


(defconst ganesha-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset  . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((brace-entry-open before after)
                               (substatement-open before after)
                               (block-close . c-snug-do-while)
                               (arglist-cont-nonempty)))
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace))
    (c-offsets-alist . ((statement-block-intro . +)
                        (knr-argdecl-intro     . 0)
                        (substatement-open     . +)
                        (substatement-label    . 0)
                        (label                 . 0)
                        (brace-list-open . +)
                        (statement-cont        . +)))
    (indent-tabs-mode nil))
  "Ganesha C Style")

(c-add-style "ganesha" ganesha-c-style)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(lsp-ui-flycheck-list-position 'right)
 '(lsp-ui-flycheck-live-reporting t t)
 '(package-selected-packages
   '(slime yasnippet-snippets flycheck-golangci-lint monokai-theme solarized-theme zerodark-theme yasnippet yaml-mode use-package restclient prettier-js org-roam org-journal neotree memoize magit lsp-ui ledger-mode json-mode js2-mode helm graphviz-dot-mode go-projectile go-autocomplete flycheck-ledger exec-path-from-shell evil elixir-mode dashboard cquery company-lsp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
