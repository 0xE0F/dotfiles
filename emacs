;; Visual

(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
(set-default-font "Source Code Pro 13")
;; (set-default-font "Input Mono 14")
(setq tab-width 4)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell


;; Package mamagment
(setq package-list '(evil ibuffer org recentf dashboard go-mode gorepl-mode auto-complete go-autocomplete exec-path-from-shell yaml-mode flycheck))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			("gnu" . "http://elpa.gnu.org/packages/")
			("melpa stable" . "http://stable.melpa.org/packages/")
			("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))


;; Packages specific setup

;; Evil mode
(require 'evil)
(evil-mode 1)
;;(define-key evil-normal-state-map "M-x" 'execute-extended-command)

;; =======================

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; bind ':ls' command to 'ibuffer instead of 'list-buffers
(evil-ex-define-cmd "ls" 'ibuffer)

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

;; Go mode
;; Install:
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/nsf/gocode
;; Snag the user's PATH and GOPATH
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (auto-complete-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (add-hook 'go-mode-hook #'gorepl-mode)
  (global-flycheck-mode)

;; Godef jump key binding
  (define-key evil-motion-state-map (kbd "C-]") 'godef-jump)
  (local-set-key (kbd "C-]") 'godef-jump)
)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))


(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Org Mode
(require 'org)
(setq org-log-done t)

(setq org-directory "~/Dropbox/Org/")
(setq org-agenda-files (list org-directory))

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

;; YAML mode
(add-hook 'yaml-mode-hook
  (lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; =======================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
