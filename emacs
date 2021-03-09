

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

(setq package-list '(evil ibuffer org recentf dashboard go-mode all-the-icons zerodark-theme json-reformat
					auto-complete go-rename magit prettier-js
					exec-path-from-shell yaml-mode flycheck neotree helm
					lsp-mode company company-lsp cquery use-package markdown-mode
					projectile go-projectile magit json-mode js2-mode
					restclient elixir-mode lsp-ui flycheck-ledger graphviz-dot-mode)
)

; list the repositories containing them
(setq package-archives '(
			("melpa" . "http://melpa.org/packages/")
			("elpa" . "http://tromey.com/elpa/")
			("gnu" . "http://elpa.gnu.org/packages/")
			("melpa stable" . "http://stable.melpa.org/packages/")
))

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
;; go get -u golang.org/x/tools/cmd/goimports
;; Snag the user's PATH and GOPATH
(exec-path-from-shell-initialize)

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
    (exec-path-from-shell-copy-env "GOPATH") ;
    (exec-path-from-shell-copy-env "GOROOT") ; This is important for some tools gopls

    (require 'go-projectile)

    (require 'lsp-ui)
    (go-guru-hl-identifier-mode)

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
)

;; Elixir
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
		  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))


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
(setq org-directory "~/Org/")

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

(global-set-key (kbd "C-c C-j") 'org-capture)
(setq org-default-notes-file (concat org-directory "notes.org"))

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "tasks.org") "Tasks")
         "* TODO %?\n  %U\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "notes.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))

;;(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq org-agenda-files '("~/Org/tasks.org"))
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

;; optional package to get the error squiggles as you edit
(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations nil)
)

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode)
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
;; Don't forget to run `M-x all-the-icons-install-fonts`
(use-package all-the-icons)
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



;; Modeline
(zerodark-setup-modeline-format)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(lsp-ui-flycheck-list-position (quote right))
 '(lsp-ui-flycheck-live-reporting t)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
