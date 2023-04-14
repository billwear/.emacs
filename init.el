;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs file for stormrider       4240 stewart lane ;;
;; wowear@gmail.com                 perkinston, ms    ;;
;; (228) 363-3263                               39573 ;;
;; bill.wear@canonical.com          stormrider.io     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: Some items are set to the current default value
;; as of this writing, just in case the default value is
;; changed in a future version of emacs. This version of
;; my .emacs file, rendered 2021-01-08, switches my repo
;; from dropbox to github, since the continuous synch is
;; generating a lot of internet traffic and eating up my
;; monthly quota; putting the onus on myself to do daily
;; and/or periodic backups to avoid losing things.  Here
;; is a quick rundown of how this file is organized: the
;; first section deals with the package repos that I use
;; frequently to scan for new software; section 2 is all
;; about system-set variables (I try to leave that alone
;; and try not to duplicate it -- it's very easy to miss
;; a parenthesis and screw something up). 
;; 
;;
;;;;;;;;;;   REPOSITORY SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; enable package.el
(require 'package)
;;
;; configure emacs to use melpa and melpa-stable
(add-to-list 'package-archives '("melpa-stable" . "http://melpa.org/packages/") t)
;;
;; configure emacs to also use orgmode repository, which
;; contains a number of org-mode packages not maintained
;; at all on melpa* repositories
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;
;; warn if SSL can't be used; this is security-critical!
(let* ((no-ssl
	(and (memq system-type '(windows-nt ms-dos))
             (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
;; warn if SSL can't be used; this is security-critical!
Your current version of Emacs does not support using SSL 
connections; this is particularly unsafe since it allows 
man-in-the-middle attacks.  There are two things you can 
do about this warning: (1) install an Emacs version that
does support SSL and be safe, or (2) Remove this warning
from your init file so you won't see it again."))

;; Comment the following two lines to disable MELPA (and
;; MELPA Stable) as desired repositories
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto
				     "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
	       (cons "melpa-stable" (concat proto
					    "://stable.melpa.org/packages/")) t)

;; Address compatibility issues
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries eg, cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This line must occur before all
;; configurations of installed packages. Do *not* delete
;; this line.  If you don't want it, just comment it out
;; by adding a semicolon to the start of the line. It is
;; safe (but unwise) to delete this explanatory comment.
(package-initialize)

;; selected packages
(setq package-selected-packages
   '(adafruit-wisdom go-mode blacken writeroom-mode json-mode bang arc-dark-theme centered-window lxc discourse blog-minimal ledger-mode free-keys pandoc-mode pandoc org-web-tools))

;;;;;;;;;;;; COLOR AND STYLE SETTINGS ;;;;;;;;;;;;;;;;;;
(setq ansi-color-faces-vector
   [default default default italic underline success warning error])
(setq ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
(setq column-number-mode t)
(setq custom-safe-themes
   '("3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "24fc62afe2e5f0609e436aa2427b396adf9a958a8fa660edbaab5fb13c08aae6" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default))

;;;;;;;;;;;; MODE LINE SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-battery-mode t)
(setq display-time-mode t)
(setq save-place-mode t)
(setq show-paren-mode t)
(setq size-indication-mode t)

;;; C U S T O M   S E T   F A C E S ;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; E N D   C U S T O M   S E T   F A C E S ;;;

;;;;;;;;;;;; WORLD CLOCK SETTINGS ;;;;;;;;;;;;;;;;;;;;;;
;; show times for my team members, relative to me
(setq display-time-world-list
'(("Europe/Stockholm" "bjornt")
  ("Europe/Rome" "ack")
  ("Europe/London" "adam")
  ("America/Argentina/Buenos_Aires" "diego")
  ("America/New_York" "christian")
  ("America/Chicago" "*** me ***")
  ("America/Los_Angeles" "ltrager")))

;; customize the world time display to keep window small
(setq display-time-world-time-format "%a %R")

;;;;;;;;;;;; LEDGER SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ledger-reports
'(("balance" "ledger balance -f journal.ledger")
  ("balances" "ledger balance")
  ("okay" "ledger ")
  ("register" "ledger ")
  ("Assets" "ledger ")
  ("bal" "%(binary) -f %(ledger-file) bal")
  ("reg" "%(binary) -f %(ledger-file) reg")
  ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
  ("account" "%(binary) -f %(ledger-file) reg %(account)")))

;;;;;;;;;;;; TRAMP MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; good or bad, i use tramp to edit files remotely
(setq tramp-default-method "ssh")

;;; H T M L   E X P O R T ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; turn off TOC and section numbers, i don't use them.
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; don't timestamp exported HTML pages, not a fan
(setq org-export-with-timestamps nil)

;; don't substitute printer's marks for basic dashes
(setq org-export-with-special-strings nil)

;; do use subscripts and superscripts
(setq org-export-with-sub-superscripts t)

;; these are the export engines i frequently use
(setq org-export-backends '(ascii html md odt org))

;; inline images in HTML instead of linking the image
(setq org-html-inline-images t)

;; increase default number of headings to export
(setq org-export-headline-levels 6)

;; avoids lots of strange characters in output HTML
(setq org-export-coding-system 'utf-8)

;; use a postamble when publishing website
(setq org-html-postamble t)
(setq org-html-postamble-format '(("en" "<hr><p>Updated %d %T by %a (<a href=\"mailto:wowear@gmail.com\">stormrider</a>)</p><br/> <p>Copyright (C) 2020 by Bill Wear.  All rights reserved, but <a href=\"mailto:wowear@gmail.com\">asking to use is permitted and welcome</a>.</p>")))

;; use a preamble when publishing website
(setq org-html-preamble t)
(setq menubar (concat "<a href=\"http://stormrider.io/index.html\">home</a>"
		      " :: "
		      "<a href=\"http://stormrider.io/whoami.html\">whoami</a>"
		      " :: "
		      "<a href=\"http://stormrider.io/phone.html\">phone</a>"
		      " :: "
		      "<a href=\"http://stormrider.io/maas.html\">maas</a>"
		      " :: "
		      "<a href=\"http://stormrider.io/rpi4.html\">rPI4</a>"
		      " :: "
		      "<a href=\"http://stormrider.io/freebsd.html\">FreeBSD</a>"))
(setq org-html-preamble-format '(("en" "<a href=\"http://stormrider.io/index.html\">home</a> <a href=\"http://stormrider.io/whoami.html\">whoami</a> <a href=\"http://stormrider.io/phone.html\">phone</a> <a href=\"http://stormrider.io/maas.html\">maas</a> <a href=\"http://stormrider.io/rpi4.html\">rPI4</a> <a href=\"http://stormrider.io/freebsd.html\">FreeBSD</a>"))) 

;; don't use the HTML validation link, it makes no sense
(setq org-html-validation-link nil)

;;;;;;;; modified key settings ;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the standard org-mode keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; Disable keys in org-mode
;;    C-c [ 
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

;;; NON-ORG-MODE CUSTOMIZATIONS ;;;

;; set location for ephemeral calculations
(setq calendar-latitude 30.6)
(setq calendar-longitude -89.4)
(setq calendar-location-name "Crane Creek, Mississippi")

;; include the diary in the org agenda
(setq org-agenda-include-diary t)

;; blacken python code
(add-hook 'python-mode-hook 'blacken-mode)

;; customize dired to display human file sizes
(setq dired-listing-switches "-alh --ignore=.* --ignore=\\#* --ignore=*~")

;; make my text always wrap to the window, without breaking the lines
(global-visual-line-mode 1)
(put 'downcase-region 'disabled nil)

;; set up windmove
(windmove-default-keybindings 'control)

;;;;;;;;;;;; ORG AGENDA SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;
;; compact agenda blocks
(setq org-agenda-compact-blocks t)

;; custom agenda commands
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
	 ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))
	("P" "Planner" 
	 ((agenda "")
	  (tags-todo "agendas-projects")
	  (tags-todo "bezzies-projects")
	  (tags-todo "dropbox-projects")
	  (tags-todo "errands-projects")
	  (tags-todo "fossful-projects")
	  (tags-todo "gainful-projects")
	  (tags-todo "habitat-projects")
	  (tags-todo "leisure-projects")
	  (tags-todo "waiting")))
	("h" "Habits" tags-todo "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down))))
	(" " "Agenda"
	 ((agenda "" nil)
	  (tags "REFILE"
		((org-agenda-overriding-header "Tasks to Refile")
		 (org-tags-match-list-sublevels nil)))
	  (tags-todo "-CANCELLED/!"
		     ((org-agenda-overriding-header "Stuck Projects")
		      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-BKLG-CANCELLED/!"
		     ((org-agenda-overriding-header "Projects")
		      (org-agenda-skip-function 'bh/skip-non-projects)
		      (org-tags-match-list-sublevels 'indented)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-CANCELLED/!NPRO"
		     ((org-agenda-overriding-header
		       (concat "Project Next Tasks"
			       (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		      (org-tags-match-list-sublevels t)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-sorting-strategy
		       '(todo-state-down effort-up category-keep))))
	  (tags-todo "-REFILE-CANCELLED-WAITING-BKLG/!"
		     ((org-agenda-overriding-header
		       (concat "Project Subtasks"
			       (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-non-project-tasks)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-REFILE-CANCELLED-WAITING-BKLG/!"
		     ((org-agenda-overriding-header
		       (concat "Standalone Tasks"
			       (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-project-tasks)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-CANCELLED+WAITING|BKLG/!"
		     ((org-agenda-overriding-header
		       (concat "Waiting and Postponed Tasks"
			       (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-non-tasks)
		      (org-tags-match-list-sublevels nil)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
	  (tags "-REFILE/"
		((org-agenda-overriding-header "Tasks to Archive")
		 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		 (org-tags-match-list-sublevels nil))))
	 nil)))

;; don't show blocked tasks
(setq org-agenda-dim-blocked-tasks 'invisible)

;; only show the current repetition
(setq org-agenda-repeating-timestamp-show-all nil)

;; The following setting is different from the document so that you
;; can override the document org-agenda-files by setting your
;; org-agenda-files in the variable org-user-agenda-files
;;
(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/mnt/Dropbox/var/org"))))

;;;;;;;;;;;;;; ORG-MODE SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; obviously, we need org-mode; it should be the default, but i prefer
;; to set some critical settings -- even to the default values -- juxt
;; in case things should change in the future
(require 'org)
(require 'org-id)

;; can move top-level (*) subtree
(setq org-allow-promoting-top-level-subtree t)

;; org-capture-templates
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* NPRO Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/mnt/Dropbox/var/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/mnt/Dropbox/var/org/dropbox.org")
               "* PROJ %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: PROJ\n:END:\n"))))

;; default notes go in dropbox.org, in addition to captures
(setq org-default-notes-file "~/mnt/Dropbox/var/org/dropbox.org")

;; org directory
(setq org-directory "~/Dropbox/org")

;; roll up to-do stats, if invoked
(setq org-hierarchical-todo-statistics t)

;; log completion settings
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

; enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (ol-bbdb
                          ol-bibtex
                          org-crypt
                          ol-gnus
                          org-id
                          ol-info
                          org-habit
                          org-inlinetask
                          ol-irc
                          ol-mew
                          ol-mhe
                          org-protocol
                          ol-rmail
                          ol-vm
                          ol-wl
                          ol-w3m)))

;; w3m setup
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point)

;; de-emphasize priorites when assigned
(setq org-priority-faces '((0 . "dim grey")))

;; control priority settings
(setq org-priority-lowest 90)

;; turn off shift select to avoid various issues
(setq org-support-shift-select nil)

;; let each keyword have its own distinct color
(setq org-todo-keyword-faces
'(("TODO" :foreground "orange" :weight bold)
  ("PROJ" :foreground "blue" :weight bold)
  ("DONE" :foreground "forest green" :weight bold)
  ("WAIT" :foreground "purple" :weight bold)
  ("BKLG" :foreground "magenta" :weight bold)
  ("XOUT" :foreground "red" :weight bold)
  ("NPRO" :foreground "gray" :weight bold)
  ("MEET" :foreground "green" :weight bold)
  ("FONE" :foreground "pink" :weight bold)))

;; set org-todo keywords (4 char each for column align)
(setq org-todo-keywords
   '((sequence "TODO(t)" "PROJ(o)" "NPRO(n)" "MEET(m)" "|" "DONE(d)")
     (sequence "WAIT(w@/!)" "BKLG(b@/!)" "|" "XOUT(x@/!)" "FONE(f)")))

;; state sequences
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("BKLG" ("WAITING") ("BKLG" . t))
              (done ("WAITING") ("BKLG"))
              ("TODO" ("WAITING") ("CANCELLED") ("BKLG"))
              ("PROJ" ("WAITING") ("CANCELLED") ("BKLG"))
              ("DONE" ("WAITING") ("CANCELLED") ("BKLG")))))

;; make it easy to change states
(setq org-treat-S-cursor-todo-selection-as-state-change t)
(setq org-use-fast-todo-selection t)

;; don't use tag inheritance; it doesn't work well for me
(setq org-use-tag-inheritance nil)

;; set diary file
(setq diary-file "~/mnt/Dropbox/var/org/diary.org")
(setq org-agenda-diary-file "~/mnt/Dropbox/var/org/diary.org")

;; hide emphasis markers - nice, but makes entering headlines hard
;;(setq org-hide-emphasis-markers t)

;; catch invisible edits
(setq org-catch-invisible-edits "show")

;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-lisp-path
;;
(if (boundp 'org-mode-user-lisp-path)
    (add-to-list 'load-path org-mode-user-lisp-path)
  (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp")))

;; set extensions which default to org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

; targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; use full outline paths for refile targets - file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
(setq org-refile-target-verify-function 'bh/verify-refile-target)
(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;;;;;; MAJORITY OF CLOCKING STUFF ;;;;;;;;;;;;;;;;;;;;;;;
;; function variables and hooks
(setq bh/keep-clock-running nil)
(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; round minutes
(setq org-time-stamp-rounding-minutes (quote (1 1)))

;; check consistency
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

;; remove 0:00 clock entries
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:02 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
			    ("agendas" . ?a)
			    ("bezzies" . ?b)
			    ("dropbox" . ?d)
			    ("errands" . ?e)
			    ("fossful" . ?f)
			    ("gainful" . ?g)
			    ("habitat" . ?h)
			    ("leisure" . ?l)
			    ("someday" . ?s)
			    (:endgroup)
			    (:startgroup)
			    ("evening" . ?E)
			    ("lunch" . ?L)
			    ("morning" . ?M)
			    ("thisweek" . ?T)
			    ("nextweek" . ?N)
			    (:startgroup)
			    ("project" . ?p)
			    ("waiting" . ?w)
			    (:endgroup))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; phone call stuff
(require 'bbdb)
(require 'bbdb-com)

(global-set-key (kbd "<f9> p") 'bh/phone-call)

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-alphabetical-lists t)

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)

(setq org-ditaa-jar-path "~/git/org-mode/contrib/scripts/ditaa.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)


; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)


(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(setq org-latex-listings t)

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (bh/org-agenda-to-appt)

; Activate appointments so we get notifications
;; (appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; splantuml - PlantUML Source block
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

(define-skeleton skel-org-block-plantuml-activity
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
  (bh/plantuml-reset-counters)
  "@startuml\n"
  "skinparam activity {\n"
  "BackgroundColor<<New>> Cyan\n"
  "}\n\n"
  "title " str " - \n"
  "note left: " str "\n"
  "(*) --> \"" str "\"\n"
  "--> (*)\n"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(defvar bh/plantuml-if-count 0)

(defvar bh/plantuml-loop-count 0)

(define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

(define-skeleton skel-org-block-plantuml-activity-if
  "Insert a org plantuml block activity if statement"
  "" 
  "if \"\" then\n"
  "  -> [condition] ==IF" (setq ifn (bh/plantuml-if)) "==\n"
  "  --> ==IF" ifn "M1==\n"
  "  -left-> ==IF" ifn "M2==\n"
  "else\n"
  "end if\n"
  "--> ==IF" ifn "M2==")

(define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

(define-skeleton skel-org-block-plantuml-activity-for
  "Insert a org plantuml block activity for statement"
  "Loop for each: " 
  "--> ==LOOP" (setq loopn (bh/plantuml-loop)) "==\n"
  "note left: Loop" loopn ": For each " str "\n"
  "--> ==ENDLOOP" loopn "==\n"
  "note left: Loop" loopn ": End for each " str "\n" )

(define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

(define-skeleton skel-org-block-plantuml-sequence
  "Insert a org plantuml activity diagram block, querying for filename."
  "File appends (no extension): "
  "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
  "@startuml\n"
  "title " str " - \n"
  "actor CSR as \"Customer Service Representative\"\n"
  "participant CSMO as \"CSM Online\"\n"
  "participant CSMU as \"CSM Unix\"\n"
  "participant NRIS\n"
  "actor Customer"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

(global-set-key (kbd "<f5>") 'bh/org-todo)


(global-set-key (kbd "<S-f5>") 'bh/widen)


(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)


(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)


(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil)

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down alpha-up habit-down user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; enforce todo dependencies, making projects easier to cope with
;;(setq org-enforce-todo-dependencies t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; ;; Enable display of the time grid so we can see the marker for the current time
;; (setq org-agenda-time-grid (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (0900 1100 1300 1500 1700))))

;; Display tags farther right
;;(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-contrib-lisp-path
;;
(if (boundp 'org-mode-user-contrib-lisp-path)
    (add-to-list 'load-path org-mode-user-contrib-lisp-path)
  (add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp")))

(setq org-hide-leading-stars nil)

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)


(setq org-clock-sound "/usr/local/lib/tngchime.wav")

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(global-auto-revert-mode t)

(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
(setq org-crypt-key "F0B66B40")

(setq org-crypt-disable-auto-save nil)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(require 'org-protocol)

(setq require-final-newline t)

(defvar bh/insert-inactive-timestamp t)

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)


(setq org-return-follows-link t)



(setq org-remove-highlights-with-change t)

(add-to-list 'Info-default-directory-list "~/git/org-mode/doc")

(setq org-read-date-prefer-future 'time)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

(setq org-tags-match-list-sublevels t)

(setq org-agenda-persistent-filter t)

(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Bookmark handling
;;
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(require 'org-mime)

(setq org-agenda-skip-additional-timestamps-same-entry t)

(setq org-table-use-standard-references (quote from))

(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))

; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-src-fontify-natively t)

(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_ascii\n?\n#+end_ascii")
              ("A" "#+ascii: ")
              ("i" "#+index: ?" "#+index: ?")
              ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable keys in org-mode
;;    C-c [ 
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
          'append)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'error)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))))

(setq org-use-sub-superscripts nil)

(setq org-odd-levels-only nil)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; init functions
(defun set-cat()
  (interactive)
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
		    (org-agenda-error))))
    (org-with-point-at marker
      (org-back-to-heading t)
      (org-set-property "CATEGORY" (read-string "Enter category:")))))


(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to PROJ when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from PROJ back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NRPO")
     ((and (member (org-get-todo-state) (list "NPRO"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;;
;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NPRO Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NPRO " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NPRO " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NRPO tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NPRO")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NPRO")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun bh/plantuml-if () 
  (incf bh/plantuml-if-count)
  (number-to-string bh/plantuml-if-count))

(defun bh/plantuml-loop () 
  (incf bh/plantuml-loop-count)
  (number-to-string bh/plantuml-loop-count))

(defun bh/plantuml-reset-counters ()
  (setq bh/plantuml-if-count 0
        bh/plantuml-loop-count 0)
  "")

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))
(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NPRO states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NPRO"))
            (org-todo "TODO")))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(beacon-color "#ed0547ad8099")
 '(browse-url-browser-function 'w3m-browse-url)
 '(custom-enabled-themes '(manoj-dark))
 '(custom-safe-themes
   '("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "66faea8fbca048423ba9018fc0f2cde388fe0e8d8537173c8a14826a2764825c" "e01db763cd9daa56f75df8ebd057f84017ae8b5f351ec90c96c928ad50f3eb25" "08765d801b06462a3ce7e414cdb747436ccaf0c073350be201d8f87bd0481435" "1fbd63256477789327fe429bd318fb90a8a42e5f2756dd1a94805fc810ae1b62" "27b97024320d223cbe0eb73104f2be8fcc55bd2c299723fc61d20057f313b51c" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "068921e760fddb557be0c164fcf4fcee172244a41bf678669f9d40a48dc9e116" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(diff-hl-show-hunk-posframe-internal-border-color "#357535753575")
 '(emms-mode-line-icon-color "#1ba1a1")
 '(evil-emacs-state-cursor '("#E57373" hbar))
 '(evil-insert-state-cursor '("#E57373" bar))
 '(evil-normal-state-cursor '("#FFEE58" box))
 '(evil-visual-state-cursor '("#C5E1A5" box))
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "dark green")
 '(font-use-system-font t)
 '(gnus-logo-colors '("#1ec1c4" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors '(("#ed0547ad8099" . 0) ("#424242" . 100)))
 '(ispell-dictionary "en_GB")
 '(org-agenda-files '("~/mnt/Dropbox/var/org"))
 '(org-agenda-prefix-format
   '((agenda . "%t %e ")
     (todo . "")
     (tags . "")
     (search . "")))
 '(org-insert-heading-hook nil)
 '(org-priority-default 90)
 '(org-tags-column 1)
 '(org-tags-sort-function 'org-string-collate-lessp)
 '(package-selected-packages
   '(nov toc-org adafruit-wisdom go-mode blacken writeroom-mode json-mode bang arc-dark-theme centered-window lxc discourse blog-minimal ledger-mode free-keys pandoc-mode pandoc org-web-tools))
 '(pos-tip-background-color "#3a933a933a93")
 '(pos-tip-foreground-color "#9E9E9E")
 '(red "#ffffff")
 '(tabbar-background-color "#357535753575")
 '(vc-annotate-background "#404040")
 '(vc-annotate-color-map
   '((20 . "#c83029")
     (40 . "#db4334")
     (60 . "#959508")
     (80 . "#bcaa00")
     (100 . "#dc7700")
     (120 . "#c9d617")
     (140 . "#319448")
     (160 . "#078607")
     (180 . "#60a060")
     (200 . "#29b029")
     (220 . "#47cd57")
     (240 . "#4c8383")
     (260 . "#1ba1a1")
     (280 . "#0a7874")
     (300 . "#1e7bda")
     (320 . "#00a2f5")
     (340 . "#58b1f3")
     (360 . "#da26ce")))
 '(vc-annotate-very-old-color "#da26ce"))

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "/ssh:stormrider:/home/stormrider/stormrider.io/org/"
	 :base-extension "org"
	 :publishing-directory "/ssh:stormrider:/var/www/html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 5
	 :auto-preamble t
	 )
	("org-static"
	 :base-directory "/ssh:stormrider:/home/stormrider/stormrider.io/org/"
	 :base-extension "css\\|js\\|png\\|jpg\\|\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "/ssh:stormrider:/var/www/html/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("org" :components ("org-notes" "org-static"))
	))

;;; matterless.el --- MatterMost client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Gaelan D'costa

;; Author: Gaelan D'costa <gdcosta@gmail.com>
;; Version: 0.0.1
;; Created: April 07, 2019
;; Keywords: chat, mattermost
;; Homepage: https://github.com/RobotDisco/matterless

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I have no idea how Emacs Lisp programs are written, let's find out!

;; No doubt I will be shamelessly inspired by the amazing slack client written
;; by Yuya Minami over at https://github.com/yuya373/emacs-slack/

;;; Code:

(require 'json)
(require 'request)

(defgroup matterless nil
  "Emacs Mattermost Client"
  :prefix "matterless-"
  :group 'tools)

(defcustom matterless-server-url nil
  "URL of the mattermost server we will be talking to."
  :type 'string
  :group 'matterless)

(defun matterless-api-prefix ()
  "Return the full url of the base Mattermost API path."
  (concat matterless-server-url "/api/v4/"))

(defcustom matterless-username nil
  "Username of our mattermost server."
  :type 'string
  :group 'matterless)

(defcustom matterless-password nil
  "User password of our mattermost server."
  :type 'string
  :group 'matterless)

(defun matterless-request-login ()
  "Login to mattermost."
  (let ((req (request
              (concat (matterless-api-prefix) "users/login")
              :data (json-encode `(("login_id" . ,matterless-username)
                                  ("password" . ,matterless-password)))
              :parser 'json-read
              :sync t)))
    (request-response-data req)))

(provide 'matterless)
;;; matterless.el ends here

;; save desktop
;;(desktop-save-mode 1)

;; sunrise and sunset as part of the agenda view
(defun diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

;;;; macros below this line

;; skip macro
(fset 'skip
   (kmacro-lambda-form [?t ?x ?s ?k ?i ?p ?p ?d backspace ?e ?d ?  ?t ?o ?d ?a ?y ?\C-c ?\C-c] 0 "%d"))

;; mark projects
(fset 'mark-proj
   (kmacro-lambda-form [return ?: ?P ?R ?O ?P ?E ?R ?T ?I ?E ?S ?: return ?: ?C ?A ?E ?T ?G backspace backspace backspace ?G ?E ?T ?O backspace backspace backspace backspace ?T ?E ?G ?O ?R ?Y ?: ?  ?p ?r ?o ?j ?e ?c ?t ?s return ?: ?E ?N ?D ?:] 0 "%d"))

;; open rad ui comment
(fset 'oui
   (kmacro-lambda-form [?< ?! ?- ?- ?  ?d ?e ?b ?- ?2 ?- ?8 ?- ?u ?i ?  ?s ?n ?a ?p ?- ?2 ?- ?8 ?- ?u ?i ?  ?d ?e ?b ?- ?2 ?- ?9 ?- ?u ?i ?  ?d ?e ?b ?- ?2 backspace backspace backspace backspace backspace ?s ?n ?a ?p ?2 ?- backspace backspace ?- ?2 ?- ?9 ?- ?u ?i ?  ?d ?e ?b ?- ?2 ?- ?1 ?0 ?- ?u ?i ?  ?d ?e ?b backspace backspace backspace ?s ?n ?a ?p ?- ?2 ?- ?1 ?0 ?- ?u ?i] 0 "%d"))

;; close rad ui comment
(fset 'cui
   (kmacro-lambda-form [?  ?d ?e ?b ?- ?2 ?- ?8 ?- ?u ?i ?  ?d ?e ?b ?- ?2 ?- ?8 backspace backspace backspace backspace backspace backspace backspace ?s ?n ?a ?p ?- ?2 ?- ?8 ?- ?u ?i ?  ?d ?e ?b ?- ?2 ?- ?9 ?- ?u ?i ?  ?s ?n ?a ?p ?- ?2 ?- ?9 ?- ?u ?i ?  ?d ?e ?b ?- ?2 ?- ?1 ?0 ?- ?u ?i ?  ?d ?e ?b ?- backspace backspace backspace backspace ?s ?n ?a ?p ?- ?2 ?- ?1 ?0 ?- ?u ?i ?  ?- ?- ?>] 0 "%d"))

;; open rad cli comment
(fset 'ocli
   (kmacro-lambda-form [?< ?1 backspace ?! ?- ?- ?  ?d ?e ?b ?- ?2 ?- ?8 ?- ?c ?l ?i ?  ?s ?n ?a ?p ?2 backspace ?- ?2 ?- ?8 ?- ?c ?l ?i ?  ?d ?e ?b ?- ?2 ?- ?9 ?- ?c ?l ?i ?  ?s ?n ?a ?p ?- ?2 ?- ?9 ?- ?c ?l ?i ?  ?d ?e ?b ?- ?2 ?- ?1 ?0 ?- ?c ?l ?i ?  ?d ?e ?b ?- ?2 ?- backspace backspace backspace backspace backspace backspace ?s ?n ?a ?p ?- ?2 ?- ?1 ?- backspace backspace ?1 ?0 ?- ?c ?l ?i] 0 "%d"))

;; close rad cli comment
(fset 'ccli
   (kmacro-lambda-form [?  ?d ?e ?b ?- ?2 ?- ?8 ?- ?c ?l ?i ?  ?s ?n ?a ?p ?- ?2 ?- ?8 ?- ?c ?l ?i ?  ?d ?e ?b ?- ?2 ?3 ?- backspace backspace ?- ?9 ?- ?c ?l ?i ?  ?d ?e ?b ?- backspace backspace backspace backspace ?s ?n ?a ?p ?- ?2 ?- ?9 ?- ?c ?l ?i ?  ?d ?e ?b ?- ?2 ?- ?1 ?0 ?- ?c ?l ?i ?  ?s ?n ?a ?p ?- ?2 ?- ?1 ?0 ?- ?c ?l ?i ?  ?- ?- ?>] 0 "%d"))

;;;; screen setup
;; set up my display
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-splash-screen t)
(shell-command "ddate > /tmp/ephemera")
(shell-command "fortune -s >> /tmp/ephemera")
(shell-command "cat /tmp/ephemera | cowsay > /tmp/fortune")
(org-agenda-list)
(split-window)
(next-multiframe-window)
(split-window-horizontally)
(next-multiframe-window)
(next-multiframe-window)
(display-time-world)
(previous-multiframe-window)
(find-file "/tmp/fortune")
(shrink-window 8)
(previous-multiframe-window)
(shrink-window-horizontally 30)
(next-multiframe-window)
(split-window-horizontally)
(shrink-window-horizontally 15)
(next-multiframe-window)
(switch-to-buffer "*Org Agenda(a)*")
(beginning-of-buffer)


(setq backup-directory-alist
      (quote (("." . "~/mnt/Dropbox/var/backups/emacs-backups"))))

(fset 'add-3-0-ui
   (kmacro-lambda-form [?  ?s ?n ?a ?p ?- ?3 ?- ?0 ?- ?u ?i ?  ?d ?e ?b ?- ?3 ?- ?0 ?- ?u ?i ? ] 0 "%d"))

(fset 'add-3-0-cli
   (kmacro-lambda-form [?  ?s ?n ?a ?p ?- ?3 ?- ?0 ?- ?c ?l ?i ?  ?d ?e ?b ?- ?3 ?- ?0 ?- ?c ?l ?i ? ] 0 "%d"))

(fset 'fix-make
   (kmacro-lambda-form [?\C-a ?\C-n ?\C-e ?d ?e ?b ?  ?3 ?. ?0 ?  ?c ?l ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?d ?e ?b ?  ?3 ?. ?0 ?  ?u ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?d ?e ?b ?  ?2 ?. ?8 ?  ?c ?l ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?d ?e ?b ?  ?2 ?. ?8 ?  ?u ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?d ?e ?b ?  ?2 ?. ?9 ?  ?c ?l ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?d ?e ?b ?  ?2 ?. ?9 ?  ?u ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?s ?n ?a ?p ?  ?3 ?. ?0 ?  ?c ?l ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?s ?n ?a ?p ?  ?3 ?. ?0 ?  ?u ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?s ?n ?a ?p ?  ?2 ?. ?8 ?  ?c ?l ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?s ?n ?a ?p ?  ?2 ?. ?8 ?  ?u ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?s ?n ?a ?p ?  ?2 ?. ?9 ?  ?c ?l ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?s ?n ?a ?p ?  ?2 ?. ?9 ?  ?c ?l ?i backspace backspace backspace ?u ?i ?\C-n ?\C-n ?\C-n ?\C-n ?\C-e ?p ?u ?l ?l ?  ?r ?e ?f ?e ?r ?e ?n ?c ?e ?  ?v ?e ?r ?s ?i ?o ?n ?\C-n ?\C-n ?\C-n ?\C-e ?c ?o ?p ?  backspace ?y ?  ?r ?e ?f ?e ?r ?e ?n ?c ?e ?  ?v ?e ?r ?s ?i ?o ?n ?  ?t ?o ?  ?d ?i ?s ?c ?o ?u ?r ?s ?e ?  ?m ?a ?s ?t ?e ?r ?\C-n ?\C-n ?\C-n ?\C-e ?r ?e ?m ?o ?v ?e ?  ?r ?e ?f ?e ?r ?e ?n ?c ?e ?  ?v ?e ?r ?s ?i ?o ?n ?\C-x ?\C-s up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up down down down ?\C-a ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-w down down down down down down down down down down down down down down down down ?\C-y ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-w down down down down down down down down down down down down down down down down ?\C-y up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\M-w up up up up up up up up ?\C-y down down down down down down down down down down down down down down down down down down down down down down down down down down down down up up up up ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\M-w ?\C-y up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up escape ?% ?2 ?. ?8 return ?2 ?. ?7 return ?y ?y ?n ?n ?y ?y ?q up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up up down up up up left left left left left left left left left left left escape ?% ?2 ?- ?8 return ?2 ?- ?7 return ?y ?y ?y ?y ?y ?y ?n ?n ?n ?n ?n ?n ?y ?y ?y ?y ?y ?y ?q] 0 "%d"))


(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)

    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode)
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (warn "toc-org not found"))


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

