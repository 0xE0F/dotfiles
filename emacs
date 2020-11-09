

;; =======================
;; Visual

(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
(setq tab-width 4)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

(if (eq system-type 'darwin)
	(set-frame-font "Iosevka Clam 15")
	(set-frame-font "Iosevka Clam 13")
)

;; Frame mode switch
;; https://stackoverflow.com/questions/9248996/how-to-toggle-fullscreen-with-emacs-as-default
(defun switch-fullscreen nil
  (interactive)
  (let* ((modes '(nil fullboth fullwidth fullheight))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))

(define-key global-map (kbd "C-=") 'switch-fullscreen)

;; =======================
;; Package mamagment

(setq package-list '(evil ibuffer org recentf dashboard go-mode zerodark-theme json-reformat
					auto-complete go-autocomplete go-rename magit prettier-js
					exec-path-from-shell yaml-mode flycheck neotree helm go-guru
					lsp-mode company company-lsp cquery use-package markdown-mode
					projectile go-projectile magit json-mode js2-mode org-journal
					restclient elixir-mode lsp-ui ledger-mode flycheck-ledger graphviz-dot-mode)
)

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			("gnu" . "http://elpa.gnu.org/packages/")
			("melpa stable" . "http://stable.melpa.org/packages/")
			("melpa" . "http://melpa.org/packages/"))
)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))

;;


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
(require 'evil)
(evil-mode 1)

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
(setq recentf-max-menu-items 25)
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
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; Snag the user's PATH and GOPATH
(exec-path-from-shell-initialize)

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
    (exec-path-from-shell-copy-env "GOPATH") ;
    (exec-path-from-shell-copy-env "GOROOT") ; This is important for some tools like godef

    (require 'go-projectile)

    (require 'go-guru)
    (go-guru-hl-identifier-mode)

    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")                ; gofmt uses invokes goimports

    ;; Godef jump key binding
    (define-key evil-motion-state-map (kbd "C-]") 'godef-jump)
    (local-set-key (kbd "C-]") 'godef-jump)

    (define-key evil-motion-state-map (kbd "s-]") 'godef-jump)
    (local-set-key (kbd "s-]") 'godef-jump)

    (define-key evil-motion-state-map (kbd "s-[") 'pop-tag-mark)
    (local-set-key (kbd "s-[") 'pop-tag-mark)

    ;; This one is for org-journal
    (unbind-key "C-c C-j" go-mode-map)
)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
;;  (require 'go-autocomplete)
;;  (ac-flyspell-workaround)
;; Workaround for spell checker and go-autocomplete
;;  (require 'auto-complete-config)
  ;; (ac-config-default)
)


(add-hook 'go-mode-hook 'my-go-mode-hook)


;; =======================
;; C/C++ Mode


(defun cquery//enable ()
  (condition-case nil
	  (lsp-cquery-enable)
	(user-error nil)))

(setq cquery-executable "/home/denis/Developer/Tools/cquery/build/release/bin/cquery")

;;(require 'company-lsp)
;;(push 'company-lsp company-backends)
;;(company-mode 1)

(use-package cquery
			 :commands lsp-cquery-enable
			 :init (add-hook 'c-mode-hook #'cquery//enable)
			 (add-hook 'c++-mode-hook #'cquery//enable))


;; =======================
;; Org Mode
;; C-c / for search

(require 'org)
(setq org-log-done t)
(setq org-agenda-files (directory-files-recursively "~/Org" "\\.org$"))

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

;; http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
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
  "Clock out when the task is marked WAITING or PAUSED"
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

;; =======================
;; Org-Journal mode
(use-package org-journal
  :ensure t
  :defer t
  :custom
    (org-journal-dir "~/Org/journal/")
    (org-journal-date-format "%A, %d %B %Y")
	(org-journal-file-format "%Y-%m-%d.org")
	:bind (
	       ("C-c C-j" . org-journal-new-entry)
	       ("C-c C-s" . org-journal-search)
    )
)

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

(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook #'lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
;; use flycheck, not flymake
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
)

;; LSP UI
;;
(use-package lsp-ui
  :requires lsp-mode flycheck
  ;;  :commands lsp-ui-mode
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
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package company-lsp
  :commands company-lsp)

;; optional package to get the error squiggles as you edit
(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations t)
)

;; =======================
;; YAML mode
(add-hook 'yaml-mode-hook
  (lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))


;; =======================
;; Spell check

;; Do not forget to install ispell (e.g. brew install ispell)
(dolist (hook '(text-mode-hook
		org-mode-hook
		))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                python-mode-hook
                go-mode-hook
                R-mode-hook))
  (add-hook mode '(lambda ()
               (flyspell-prog-mode))))

;; Mac OS does not like "mouse2" button
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))


;; JS
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
(add-hook 'js2-mode-hook (lambda () (prettier-js-mode 1)))

(add-hook 'js2-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint)
	      )
	    )
)

;; Try to highlight most ECMA built-ins
(setq js2-highlight-level 3)

;; turn off all warnings in js2-mode
(setq js2-mode-show-parse-errors t)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)

;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
))


(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4)
)

;; Reddit
(require 'helm)

(setq reddit-list '("emacs" "programming" "golang" "cpp" "space" "cyberpunk" "australia" "sydney"))

(defun reddit-browser ()
  "Choose a subreddit to browser using helm"
  (interactive)

  (helm
   :prompt "Reddit: "
   :sources  `((
		(name       . "File: ")
		(candidates . ,reddit-list)
		(action     . (lambda (r)
				(eww (concat "https://www.m.reddit.com/r/"
					     r
					     ))))
		))))

;; =======================
;; Theme
(load-theme 'zerodark t)

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


;; =======================
;; Budget
(use-package ledger-mode
  :mode ("\\.dat\\'"
	 "\\.ledger\\'")
  :hook (ledger-mode . ledger-flymake-enable)
  :custom (ledger-clear-whole-transactions t)
  (ledger-reconcile-default-commodity "AUD")
  (ledger-reports
   '(("account statement" "%(binary) reg --real [[ledger-mode-flags]] -f %(ledger-file) ^%(account)")
     ("balance" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities")
     ("balance sheet" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities ^equity")
     ("budget" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:budget")
     ("budget goals" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'goals'")
     ("budget obligations" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget obligations'")
     ("budget debts" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'debts'")
     ("cleared" "%(binary) cleared [[ledger-mode-flags]] -f %(ledger-file)")
     ("equity" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) equity")
     ("income statement" "%(binary) --invert --real -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^income ^expenses -p \"this month\""))
   (ledger-report-use-header-line nil))
  )

(use-package flycheck-ledger :after ledger-mode)

;; Modeline
(zerodark-setup-modeline-format)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(lsp-ui-flycheck-list-position 'right)
 '(lsp-ui-flycheck-live-reporting t)
 '(org-agenda-files
   '("~/Org/journal/2020-10-09.org" "~/Org/journal/2019-09-16.org" "~/Org/journal/2019-09-17.org" "~/Org/journal/2019-09-24.org" "~/Org/journal/2019-12-23.org" "~/Org/journal/2020-01-06.org" "~/Org/journal/2020-01-08.org" "~/Org/journal/2020-01-20.org" "~/Org/journal/2020-01-28.org" "~/Org/journal/2020-02-06.org" "~/Org/journal/2020-02-07.org" "~/Org/journal/2020-02-10.org" "~/Org/journal/2020-02-11.org" "~/Org/journal/2020-02-12.org" "~/Org/journal/2020-02-13.org" "~/Org/journal/2020-02-14.org" "~/Org/journal/2020-02-17.org" "~/Org/journal/2020-02-18.org" "~/Org/journal/2020-02-19.org" "~/Org/journal/2020-02-20.org" "~/Org/journal/2020-02-21.org" "~/Org/journal/2020-02-24.org" "~/Org/journal/2020-02-25.org" "~/Org/journal/2020-02-26.org" "~/Org/journal/2020-02-27.org" "~/Org/journal/2020-02-28.org" "~/Org/journal/2020-03-02.org" "~/Org/journal/2020-03-03.org" "~/Org/journal/2020-03-04.org" "~/Org/journal/2020-03-05.org" "~/Org/journal/2020-03-10.org" "~/Org/journal/2020-03-11.org" "~/Org/journal/2020-03-12.org" "~/Org/journal/2020-03-13.org" "~/Org/journal/2020-03-16.org" "~/Org/journal/2020-03-17.org" "~/Org/journal/2020-03-18.org" "~/Org/journal/2020-03-19.org" "~/Org/journal/2020-03-20.org" "~/Org/journal/2020-03-23.org" "~/Org/journal/2020-03-24.org" "~/Org/journal/2020-03-25.org" "~/Org/journal/2020-03-26.org" "~/Org/journal/2020-03-27.org" "~/Org/journal/2020-03-29.org" "~/Org/journal/2020-03-30.org" "~/Org/journal/2020-03-31.org" "~/Org/journal/2020-04-01.org"))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "~/Org/journal/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(package-selected-packages
   '(selectric-mode lsp-elixir org-journal js-mode helm-mini go-projectile js2-mode js2 markdown-mode company-mode company-lsp use-package cquery emacs-cquery lsp-mode hackernews zerodark-theme yaml-mode projectile neotree json-reformat helm go-rename go-guru go-autocomplete exec-path-from-shell evil dashboard autumn-light-theme atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
