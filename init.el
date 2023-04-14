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
;;; calendar setup for ephemeris
;; calendar lat/long so sunrise/sunset times will report
;; accurate values for my location
(setq calendar-latitude 30.6)
(setq calendar-longitude -89.4)
(setq calendar-location-name "Crane Creek, MS")
;;
;;; package repository setup
;;
;; enable package.el
(require 'package)
;;
;; configure emacs to use melpa and melpa-stable
(add-to-list 'package-archives
	     '("melpa-stable"
	       . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" .
	      "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("nongnu"
	      . "https://elpa.nongnu.org/nongnu/") t)
;;
;; Warn if ssl can't be used; this is security-critical
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

;; comment the following two lines to disable melpa (and
;; melpa stable) as desired repositories
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto
				     "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
	       (cons "melpa-stable" (concat proto
					    "://stable.melpa.org/packages/")) t)
;; address compatibility issues
  (when (< emacs-major-version 24)
    ;; for important compatibility libraries eg, cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; added by package.el.  this line must occur before all
;; configurations of installed packages. do *not* delete
;; this line.  if you don't want it, just comment it out
;; by adding a semicolon to the start of the line. it is
;; safe (but unwise) to delete this explanatory comment.
(package-initialize)
;;
;;; general emacs settings
;;
;; change starup behavior
(setq inhibit-startup-message t
        initial-scratch-message ";; Without interfering, simply focus on connection, truth, and authenticity, leveraging the things I have (chief among them: skeptical faith in myself, strong initiative, and insatiable curiosity) to make plans and make them real through experimental kaizen."
        cursor-type 'bar)
;;
;; make display more minimalist
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
;;
;; slow down garbage collection
(setq gc-cons-threshold 100000000)
;;
;; modern behavior
(delete-selection-mode t)
(column-number-mode)
;;
;; set a sane backup strategy
(setq
 ;; don't clobber symlinks
 backup-by-copying t
 ;; don't litter my fs tree
   backup-directory-alist
   '(("." . "~/mnt/Dropbox/var/archives/emacs-backups/"))
   ;; use versioned backups
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)      

;; make paste over selection work like all the other cut
;; and paste tools in the damn world
(delete-selection-mode 1)

;; set up windmove
(windmove-default-keybindings 'control)

;; enable treemacs
;;(treemacs)

;; standard section to set custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#ffffff" "#032f62" "#6a737d" "#d73a49" "#6a737d" "#6a737d" "#6f42c1" "#6a737d"])
 '(battery-mode-line-format nil t)
 '(beacon-color "#F8BBD0")
 '(bongo-enabled-backends
   '(mpg123 vlc mplayer mpv ogg123 speexdec timidity mikmod afplay))
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(custom-enabled-themes '(ubuntu))
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "776c1ab52648f98893a2aa35af2afc43b8c11dd3194a052e0b2502acca02bfce" "e983c35ab806b2b6e442e7b45032694b303a2abde663fee376c2d809fd5ea210" "73c9d15440a12552459f4f73cf0d7c7a0255ec042ff66a3717574ec0be8fae7c" "c5a81a42df109b02a9a68dfe0ed530080372c1a0bbcb374da77ee3a57e1be719" "28eb6d962d45df4b2cf8d861a4b5610e5dece44972e61d0604c44c4aad1e8a9d" "42ec9eaa86da5f052feed0e35b578681015b9e21ab7b5377a5a34ea9a0a9e1b9" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "40555be85ace799a2952a93012fe8747bab0c2188c8a511f20f2b2b21c7cd62e" "47f188b3ae4a3cdf29b54d3cf4b09ea1ec2cd15253879bfebbc568e86a25a81d" "2f26d251e2b0d11e0a5f16b21785ab42192374259cfe41eed67262869c1b387f" "ec2c86933a6e0b96f68f71d4b39ebdd67b43b0b32091b7689acb9acdc2a3e03b" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "6ec768e90ce4b95869e859323cb3ee506c544a764e954ac436bd44702bd666c0" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" "e01db763cd9daa56f75df8ebd057f84017ae8b5f351ec90c96c928ad50f3eb25" "08765d801b06462a3ce7e414cdb747436ccaf0c073350be201d8f87bd0481435" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "00aa8bf5a2d4463b35091c6e07072fe0658adc2d60439fa476f88d5e0097fc4b" "a005dcaad2a779d5a772b4ee2248d2c0daff40da7d9a12d41c0afb661a2b3a5f" "8feca8afd3492985094597385f6a36d1f62298d289827aaa0d8a62fe6889b33c" "5078e1845735a69b21b5effe083998dc368853320f449530c2616cf70bc3c47b" "e7b7d1e49adc2b0533b4fe57617c358ecbca80f39d05a30b825b998fa86bc372" "f6cdb429a64db06d3db965871b45ed1c666fdce2d3e2c4b810868e4cf4244c92" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "7c20c453ad5413b110ccc3bb5df07d69999d741d29b1f894bd691f52b4abdd31" "1f35dedbeacbfe9ed72810478836105b5617da67ca27f717a29bbb8087e8a1ba" "4b62f25863fb026a14c1d7683dc83da4f0af07fe0cacd1b52f0e05639d483e83" "9375315e4786e5cc84b739537102802c18650f3168cf7c29f7fbb00a54f9b8e0" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "ae4aa4bf7418af9a2a8a0e9d172895a2f25fe725790fed3f259bba53159a8264" "3263bd17a7299449e6ffe118f0a14b92373763c4ccb140f4a30c182a85516d7f" "5eed5311ae09ed84cb2e4bf2f033eb4df27e7846a68e4ea3ab8d28f6b017e44a" "f4158db802ae689ed0e156cd02c8a3c0e22c5e778578e8eea6d4afc3a9d0e629" "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559" "9549755e996a2398585714b0af745d2be5387ecf7ec299ff355ec6bef495be88" "ba913d12adb68e9dadf1f43e6afa8e46c4822bb96a289d5bf1204344064f041e" "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" "e0628ee6c594bc7a29bedc5c57f0f56f28c5b5deaa1bc60fc8bd4bb4106ebfda" "2050674326d536ddd3dcea87e077d27071cfbbe974a4540b1a57b6b672f64c51" "437cd756e079901ccdecd9c397662a3ee4da646417d7469a1c35aa8e246562fe" "143d897548e5a7efb5cf92c35bd39fe7c90cbd28f9236225ad3e80e1b79cef8a" "45feb1f130c54e0fc116faa71c784562b41009ffc908cf5cef06b6df4bb60a9a" "660376e0336bb04fae2dcf73ab6a1fe946ccea82b25f6800d51977e3a16de1b9" "7575474658c34b905bcec30a725653b2138c2f2d3deef0587e3abfae08c5b276" "ab729ed3a8826bf8927b16be7767aa449598950f45ddce7e4638c0960a96e0f1" "0cd00c17f9c1f408343ac77237efca1e4e335b84406e05221126a6ee7da28971" "d2e0c53dbc47b35815315fae5f352afd2c56fa8e69752090990563200daae434" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "4dde6de8b6124c47593ff701bc33cb933d4dd4c4ed3430b1840256c8162fe05e" "00b463c48742afe509ae7d1dcfce09471f7203e13a118f1256b208017a978b4e" "ecc077ef834d36aa9839ec7997aad035f4586df7271dd492ec75a3b71f0559b3" "df85955fd38ee2dae7476a5fa93e58e594df96132871c10ecaf4de95bdae932a" "4eb69f17b4fa0cd74f4ff497bb6075d939e8d8bf4321ce8b81d13974000baac1" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "65f35d1e0d0858947f854dc898bfd830e832189d5555e875705a939836b53054" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "16ab866312f1bd47d1304b303145f339eac46bbc8d655c9bfa423b957aa23cc9" "43f03c7bf52ec64cdf9f2c5956852be18c69b41c38ab5525d0bedfbd73619b6a" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1" "aa6638f0cd2ba2c68be03220ea73495116dc6f0b625405ede34087c1babb71ae" "5a50b073c4dc5efc08be3a374cfda36b5ec0e52ab3aa1c800dc9b1706ece2667" "cd8d4376a1b94f7063b124adbeb50477fed3feb9bc37be01c66c6005589ad175" "bd82c92996136fdacbb4ae672785506b8d1d1d511df90a502674a51808ecc89f" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "fe36e4da2ca97d9d706e569024caa996f8368044a8253dc645782e01cd68d884" "1fbd63256477789327fe429bd318fb90a8a42e5f2756dd1a94805fc810ae1b62" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "9eecd688ffd00df3a218a323ceedf3f0f2950dd2347c9b708929a347bf46d2d4" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "2ba52d3cf6f138ca570d80c791b96341d44a763e1057562aea7114fe95c8e307" "ea066684e9ace1e618719fab683b24a0fbcd3de82692190b1fe54e6b1b2a29bc" "2ed177de0dfc32a6a32d6109ddfd1782a61bcc23916b7b967fa212666d1aa95c" "8426618fcc55f670f45b04f146933ae23caa1faad603a380e7a348660fd225ab" "28cf1f7cc54ab4ee1ba4a4644046bd661941be92ef8327af56909f340cb9d3d5" "6b234feec8db588ad5ec2a9d9d7b935f7a155104b25ccfb94d921c45a2ff7d22" "bf10bd6d21928bf87bc3032b498c62cb9d48c54c06d217c8b00bef8090e539f7" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "998975856274957564b0ab8f4219300bca12a0f553d41c1438bbca065f298a29" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(diff-hl-show-hunk-posframe-internal-border-color "#ffffffffffff")
 '(dired-listing-switches "-lLX")
 '(dired-sidebar-mode-line-format nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-format "%a")
 '(doc-view-continuous t)
 '(dropbox-access-token
   "sl.A9GG1jcIhMOEtW8dd35EsKdme9LFzHgG34qGjGyIK5t1lRx5lyVIXDwB8O60TPZlgj7lh_lo1Wm9VfDaXU4qeZH0x48IJiqWEd0Q8_u6KtoLP3_NznMk9SUTKnEAJddpzsNJNWs")
 '(dropbox-verbose t)
 '(evil-emacs-state-cursor '("#D50000" hbar))
 '(evil-insert-state-cursor '("#D50000" bar))
 '(evil-normal-state-cursor '("#F57F17" box))
 '(evil-visual-state-cursor '("#66BB6A" box))
 '(fancy-battery-mode t)
 '(fci-rule-color "#6a737d")
 '(focus-follows-mouse nil)
 '(font-use-system-font t)
 '(frame-brackground-mode 'dark)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-tab-line-mode t)
 '(gnus-interactive-exit nil)
 '(gnus-logo-colors '("#528d8d" "#c0c0c0") t)
 '(gnus-message-archive-group "\"Gmail/Sent Mail\"")
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
$\"##################\",
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
 '(gnus-secondary-select-methods '((nntp "news.gmane.io")))
 '(gnus-select-method '(nnimap "imap.gmail.com"))
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315"))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors
   '(("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100)))
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#ecf0f1" "#ecf0f1" "#c0392b"))
 '(ispell-dictionary "en_GB")
 '(jiralib-update-issue-fields-exclude-list '(components assignee reporter priority issuetype))
 '(jiralib-url "https://warthogs.atlassian.net/")
 '(ledger-reports
   '(("budget" "ledger balance -f budget.ledger")
     ("balance" "ledger balance -f journal.ledger")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(linum-format " %6d ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(message-send-mail-function 'smtpmail-send-it)
 '(mlscroll-in-color "#e67fe67fe67f")
 '(mlscroll-out-color "#FAFAFA")
 '(mode-line-compact t)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-modified mode-line-remote mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(mode-line-in-non-selected-windows nil)
 '(mode-line-percent-position '(-3 "%o"))
 '(mpages-content-directory "/home/stormrider/mnt/Dropbox/var/log")
 '(neo-autorefresh nil)
 '(neo-bannermessage "welcome, stormrider")
 '(neo-create-file-auto-open t)
 '(neo-cwd-line-style 'text)
 '(neo-hide-cursor t)
 '(neo-mode-line-type 'neotree)
 '(neo-show-hidden-files t)
 '(neo-theme 'nerd)
 '(neo-window-fixed-size nil)
 '(neo-window-width 10)
 '(notmuch-search-line-faces
   '(("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t)))
 '(nrepl-message-colors
   '("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d"))
 '(org-agenda-category-icon-alist
   '(("plan" "/home/stormrider/mnt/Dropbox/media/images/icons-avatars/emacs-dragon.jpg" nil t nil)))
 '(org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
 '(org-agenda-cmp-user-defined 'bh/agenda-sort)
 '(org-agenda-compact-blocks t)
 '(org-agenda-diary-file "~/mnt/Dropbox/etc/org/diary.org")
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-agenda-files
   '("~/mnt/Dropbox/etc/org/todo.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   '((agenda . "%?-12t")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-agenda-remove-tags 'prefix)
 '(org-agenda-remove-times-when-in-prefix nil)
 '(org-agenda-restriction-lock-highlight-subtree nil)
 '(org-agenda-skip-additional-timestamps-same-entry t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-sorting-strategy '(time-up priority-down category-down tag-up effort-up))
 '(org-agenda-span 'day)
 '(org-agenda-sticky t)
 '(org-agenda-tags-column -74)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-time-grid
   '((daily remove-match)
     (800 1000 1200 1400 1600 1800 2000)
     "......" "----------------"))
 '(org-agenda-window-setup 'current-window)
 '(org-archive-location
   "/home/stormrider/mnt/Dropbox/etc/org/%s_archive::* Archived Tasks")
 '(org-clock-report-include-clocking-task t)
 '(org-cycle-separator-lines 1)
 '(org-enforce-todo-dependencies t)
 '(org-export-preserve-breaks t)
 '(org-export-with-section-numbers nil)
 '(org-fancy-priorities-list '("I must" "I want to" "I should" "I might"))
 '(org-hide-leading-stars t)
 '(org-html-postamble t)
 '(org-html-postamble-format
   '(("en" "<span style=\"font-family:'Roboto Mono'\";>Changed %C in Crane Creek.<br/>Copyright (C) 2000-2022 by <big><a href=\"mailto:stormrider.io@proton.me\">stormrider</a></big>.<br/>All rights reserved.</span>")))
 '(org-html-preamble-format
   '(("en" "<a href=\"https://stormrider.io/index.html\" style=\"font-family: 'Roboto Mono';\">Home</a> ~  <a href=\"https://stormrider.io/about-me.html\" style=\"font-family: 'Roboto Mono';\">About/Blog</a> ~ <a href=\"https://stormrider.io/family.html\" style=\"font-family: 'Roboto Mono';\">Family</a> ~ <a href=\"https://stormrider.io/tech.html\" style=\"font-family: 'Roboto Mono';\">Tech</a>")))
 '(org-journal-dir "/home/stormrider/mnt/Dropbox/var/log")
 '(org-journal-file-format "%Y%m%d.org")
 '(org-journal-find-file 'find-file)
 '(org-md-headline-style 'atx)
 '(org-mind-map-display 'window)
 '(org-mind-map-engine "dot")
 '(org-modules
   '(org-habit org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m))
 '(org-priority-default 90)
 '(org-priority-lowest 90)
 '(org-src-window-setup 'current-window)
 '(org-tags-column -55)
 '(org-todo-keywords '((sequence "TODO" "DONE")))
 '(org-todo-repeat-to-state t)
 '(org-use-tag-inheritance t)
 '(package-selected-packages
   '(gptel toc-org 0blayout markdown-preview-eww markdown-soma markdown-preview-mode blacken impatient-mode jedi-direx anaconda-mode flycheck otama org-make-toc org-fancy-priorities org-contacts org-chef org-roam-ui org-roam fortune-cookie figlet eshell-info-banner neotree orgbox org-scrum org-listcruncher markdown-mode smtpmail-multi mbsync dired-sidebar jiralib2 writeroom-mode writegood-mode hledger-mode yahtzee xkcd world-time-mode ubuntu-theme starlit-theme smart-mode-line roguel-ike pass password-store password-vault org-recur org-jira org-ehtml org-autolist org-agenda-property org noaa navorski mw-thesaurus json-navigator id-manager hide-mode-line fireplace everlasting-scratch cowsay celestial-mode-line browse-kill-ring banner-comment balanced-windows auto-dim-other-buffers alarm-clock adafruit-wisdom accent le-thesaurus display-wttr bf-mode org-mind-map org-contrib chronos emacsql-libsqlite3 xmind-org org-brain abridge-diff epresent jq-format bash-completion org-bullets nimbus-theme nano-agenda multi-term mpages srcery-theme green-is-the-new-black-theme horoscope hackernews guru-mode dashboard dad-joke zweilight-theme addressbook-bookmark 750words nnhackernews nndiscourse hamburg-theme gruvbox-theme gruber-darker-theme green-screen-theme green-phosphor-theme grandshell-theme gotham-theme github-dark-vscode-theme github-modern-theme gandalf-theme forest-blue-theme foggy-night-theme flatui-theme flatui-dark-theme flatland-black-theme flatfluc-theme firecode-theme fancy-battery farmhouse-theme fantom-theme exotica-theme espresso-theme enlightened-theme eink-theme distinguished-theme darkroom constant-theme clues-theme chocolate-theme challenger-deep-theme caroline-theme calmer-forest-theme busybee-theme dark-mint-theme darkburn-theme darkmine-theme darkokai-theme darktooth-theme brutalist-theme brutal-theme boron-theme borland-blue-theme bliss-theme blackboard-theme base16-theme atom-dark-theme badwolf-theme badger-theme ayu-theme autumn-light-theme avk-emacs-themes atom-one-dark-theme arjen-grey-theme arc-dark-theme apropospriate-theme anti-zenburn-theme ancient-one-dark-theme ample-zen-theme ample-theme almost-mono-themes alect-themes airline-themes ahungry-theme afternoon-theme acme-theme abyss-theme dropbox wttrin bongo pandoc pandoc-mode nov w3m auto-org-md ledger-mode restclient discourse magit smex ox-rst org-mime org-journal bbdb))
 '(pdf-view-midnight-colors '("#6a737d" . "#fffbdd"))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(red "#ffffff")
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(smtpmail-stream-type nil)
 '(tab-line-close-button-show nil)
 '(tab-line-close-tab-function 'bury-buffer)
 '(tab-line-new-button-show nil)
 '(tabbar-background-color "#e4e1c0")
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(tool-bar-mode nil)
 '(vc-annotate-background "#3390ff")
 '(vc-annotate-color-map
   '((20 . "#6a737d")
     (40 . "#032f62")
     (60 . "#6a737d")
     (80 . "#6a737d")
     (100 . "#6a737d")
     (120 . "#d73a49")
     (140 . "#6a737d")
     (160 . "#6a737d")
     (180 . "#6a737d")
     (200 . "#6a737d")
     (220 . "#22863a")
     (240 . "#005cc5")
     (260 . "#6f42c1")
     (280 . "#6a737d")
     (300 . "#005cc5")
     (320 . "#6a737d")
     (340 . "#d73a49")
     (360 . "#6a737d")))
 '(vc-annotate-very-old-color "#6a737d")
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp)))
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(when
      (or
       (not
	(boundp 'ansi-term-color-vector))
       (not
	(facep
	 (aref ansi-term-color-vector 0)))))
 '(window-divider-default-bottom-width 2)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 3)
 '(window-divider-mode t)
 '(world-clock-list
   '(("Europe/Moscow" "Anton")
     ("Europe/Stockholm" "Everyone else")
     ("Europe/London" "Adam")
     ("America/Buenos_Aires" "Diego")
     ("America/Sao_Paulo" "Alexsander")
     ("America/New_York" "Christian")
     ("America/Chicago" "Me")))
 '(world-clock-time-format "%a %d %b %R"))

(global-tab-line-mode)
(display-time-mode 1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; H T M L   E X P O R T ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; turn off TOC and section numbers, i don't use them.
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
;;
;; don't timestamp exported HTML pages, not a fan
(setq org-export-with-timestamps nil)
;;
;; don't substitute printer's marks for basic dashes
(setq org-export-with-special-strings nil)
;;
;; do use subscripts and superscripts
(setq org-export-with-sub-superscripts t)
;;
;; these are the export engines i frequently use
(setq org-export-backends '(ascii html md odt org))
;;
;; inline images in HTML instead of linking the image
(setq org-html-inline-images t)
;;
;; increase default number of headings to export
(setq org-export-headline-levels 6)
;;
;; avoids lots of strange characters in output HTML
(setq org-export-coding-system 'utf-8)
;;
;; use a postamble when publishing website
(setq org-html-postamble t)
;;
;; don't use the HTML validation link, it makes no sense
(setq org-html-validation-link nil)

;; set diary file
(setq diary-file "~/mnt/Dropbox/etc/org/diary.org")
(setq org-agenda-diary-file "~/mnt/Dropbox/etc/org/diary.org")

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "/home/stormrider/mnt/Dropbox/var/www/stormrider.io/"
	 :base-extension "org"
	 :publishing-directory "/ssh:stormrider:/var/www/stormrider.io"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 5
	 :auto-preamble t
	 )
	("org-static"
	 :base-directory "/home/stormrider/mnt/Dropbox/var/www/stormrider.io/"
	 :base-extension "css\\|js\\|png\\|jpg\\|\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "/ssh:stormrider:/var/www/stormrider.io"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("org" :components ("org-notes" "org-static"))
	))

'(org-html-postamble-format
   '(("en" "<hr><small>Copyright (C) 2020,2021 by <a href=\"mailto:wowear@protonmail.com\">Bill Wear</a>.<br/>All rights reserved.<br/> Last updated %d %T.</small>")))

(require 'ox-rst)
(add-hook 'markdown-mode-hook 'pandoc-mode)

(fset 'ltalk
   (kmacro-lambda-form [?A ?0 ?0 ?: ?0 ?0 ?: ?3 ?0 ?/ ?i ?n ?t ?r ?o ?  ?+ ?  ?0 ?0 ?: ?0 ?0 ?: ?1 ?2 ?0 ?/ ?b ?a ?s ?i ?c ?s ?  ?+ ?  ?0 ?0 ?: ?0 ?0 ?: ?0 ?0 ?9 ?0 backspace backspace backspace backspace ?9 ?0 ?/ ?k ?e ?y ?  ?e ?t ?h ?o ?d ?s backspace backspace backspace backspace backspace backspace ?m ?e ?t ?h ?o ?d ?s ?  ?+ ?\S-  ?0 ?0 ?: ?0 ?0 ?: ?6 ?0 ?/ ?q ?u ?e ?s ?t ?i ?o ?n ?s return] 0 "%d"))

(fset 'motd
   (kmacro-lambda-form [return ?* ?  ?\C-u ?\C-c ?. return return ?\M-! ?d ?a ?t ?e return ?\C-u ?\M-! ?d ?a ?t ?e return ?\C-e return ?\C-u ?\M-! ?d ?d ?a ?t ?e return ?\C-e return ?\C-u ?\M-! ?f ?o ?r ?t ?u ?n ?e ?  ?- ?s ?  ?f ?o ?r ?t ?u ?n ?e ?s return ?\C-e return ?\C-u ?\M-! ?d ?a backspace backspace ?d ?a ?t ?e return ?\C-k ?\C-u escape ?x ?d ?a ?d ?- ?j ?o ?k ?e return return return ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- return return] 0 "%d"))

(fset '31ui
   (kmacro-lambda-form [?/ ?s ?n ?a ?p ?/ ?3 ?. ?1 ?/ ?u ?i ?  ?/ ?d ?e ?b ?/ ?3 ?. ?1 ?/ ?u ?i] 0 "%d"))

(fset '31cli
   (kmacro-lambda-form [?/ ?s ?n ?a ?p ?/ ?3 ?/ backspace ?. ?1 ?/ ?c ?l ?i ?  ?/ ?d ?e ?b ?/ ?3 ?. ?1 ?/ ?c ?l ?i] 0 "%d"))

(fset 'clear
   (kmacro-lambda-form [?\C-c escape ?o] 0 "%d"))

(require 'org-journal)
(global-set-key (kbd "C-c C-b") 'org-journal-new-scheduled-entry)

(fset 'redir8
   (kmacro-lambda-form [?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n escape ?% ?| ?/ return ?\C-q ?\C-j ?| ?/ return ?! up up up up up up up up up up up up up up up ?\C-a ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n escape ?x ?s ?o ?r ?t ?- ?l ?i ?n ?e ?s return up up up up up up up up ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n backspace up up up up up up up up ?\C-  ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\M-w ?\C-y up up up up up up up up up up up up up up up up S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down S-down escape ?x ?s ?o ?r ?t ?- ?l ?i ?n ?e ?s return up up up up up up up up up up up up up up up up escape ?% ?| ?\C-q ?\C-j ?| return ?| return ?y ?n ?y ?n ?y ?n ?y ?n ?y ?n ?y ?n ?y ?n ?y ?q] 0 "%d"))

(fset 'wx
   (kmacro-lambda-form [?\C-u escape ?! ?c ?u ?r ?l ?  ?w ?t ?t ?r ?. ?i ?n ?/ ?N ?e ?c ?a ?i ?s ?e ?? ?T ?A return] 0 "%d"))

;; EMMS basic configuration
;;(require 'emms-setup)
;;(emms-all)
;;(emms-default-players)
;;(setq emms-source-file-default-directory "~/mnt/Dropbox/music") 
(require 'dropbox)
;;(require 'hamburger-menu)
;;(setq mode-line-front-space 'hamburger-menu-mode-line)

;;; bernt hansen's org-mode config, modified to suit my needs
;;; zork here
;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-lisp-path
;;
;; add org-mode lisp to load path to get updates and new
;; commits from org-mode devs like this, if desired:
;; cd ~/git/org-mode
;; git pull
;; make uncompiled
(if (boundp 'org-mode-user-lisp-path)
    (add-to-list 'load-path org-mode-user-lisp-path)
  (add-to-list 'load-path (expand-file-name "~/mnt/Dropbox/src/git/org-mode/lisp"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/ejira")))

;; make .txt files org-mode by default
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; force emacs to load org.el
(require 'org)
;;
;; standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


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

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-default-notes-file "~/mnt/Dropbox/etc/org/reference.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates
(setq org-capture-templates
      (quote (
	      ("m" "maas team" entry (file+headline "~/mnt/Dropbox/etc/org/todo.org" "maas team") "** TODO %? :paid:\nSCHEDULED: %t")
	      ("t" "ta team" entry (file+headline "~/mnt/Dropbox/etc/org/todo.org" "ta team") "** TODO %? :paid:\nSCHEDULED: %t")
	      ("p" "personal" entry (file+headline "~/mnt/Dropbox/etc/org/todo.org" "personal") "** TODO %?\nSCHEDULED: %t")
	      )))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;;(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
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
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

;;(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
;;(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(require 'org-id)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
			    ("paid" . ?p)
			    ("daily". ?d)
			    ("2weeks" . ?2)
			    ("weekly" . ?w)
			    ("monthly" . ?m)
			    ("yearly" . ?y)
			    (:endgroup))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(require 'bbdb)
(require 'bbdb-com)

;; (global-set-key (kbd "<f9> p") 'bh/phone-call)

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

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

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
    (setqo rg-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAIT and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

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
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
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
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
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
             (member "WAIT" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
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
             (member (org-get-todo-state) (list "NEXT")))
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
             (member (org-get-todo-state) (list "NEXT")))
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

(setq org-archive-mark-done nil)
(setq org-archive-location "/home/stormrider/mnt/Dropbox/var/archives/emacs-archives/%s_archive::* Archived Tasks")

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

(setq org-alphabetical-lists t)

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)

(setq org-ditaa-jar-path "~/mnt/Dropbox/org-mode/contrib/scripts/ditaa.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (shell . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

; experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-org-files-command "fop %s %s")
(setq org-export-docbook-xslt-org-files-command "xsltorg-files --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
;
; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)
; Do not use sub or superscripts - I currently don't need this functionality in my documents
(setq org-export-with-sub-superscripts nil)
; Use org.css from the norang website for export document stylesheets
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
(setq org-html-head-include-default-style nil)
; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)
; Increase default number of headings to export
(setq org-export-headline-levels 6)

; I'm lazy and don't want to remember the name of the project to publish when I modify
; a file that is part of a project.  So this function saves the file, and publishes
; the project that includes this file
;
; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra)
        (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
    (org-publish-current-project force)))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(setq org-latex-listings t)

(setq org-html-xml-declaration (quote (("html" . "")
                                       ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                       ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

(setq org-export-allow-BIND t)

;; ; Erase all reminders and rebuilt reminders for today from the agenda
;; (defun bh/org-agenda-to-appt ()
;;   (interactive)
;;   (setq appt-time-msg-list nil)
;;   (org-agenda-to-appt))

;; ; Rebuild the reminders everytime the agenda is displayed
;; (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

;; ; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (bh/org-agenda-to-appt)

; Activate appointments so we get notifications
;; (appt-activate t)

;; ; If we leave Emacs running overnight - reset the appointments one minute after midnight
;; (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

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

(defun bh/plantuml-if () 
  (incf bh/plantuml-if-count)
  (number-to-string bh/plantuml-if-count))

(defvar bh/plantuml-loop-count 0)

(defun bh/plantuml-loop () 
  (incf bh/plantuml-loop-count)
  (number-to-string bh/plantuml-loop-count))

(defun bh/plantuml-reset-counters ()
  (setq bh/plantuml-if-count 0
        bh/plantuml-loop-count 0)
  "")

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

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

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

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

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

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

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

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

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

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)
(defvar bh/project-list nil)

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

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

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
(setq org-agenda-diary-file "~/mnt/Dropbox/etc/org/diary.org")

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; **** these lines are broken in current emacs version ****
;; Enable display of the time grid so we can see the marker for the current time
;; (setq org-agenda-time-grid (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (0900 1100 1300 1500 1700))))

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)
;;tork

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

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-contrib-lisp-path
;;
(if (boundp 'org-mode-user-contrib-lisp-path)
    (add-to-list 'load-path org-mode-user-contrib-lisp-path)
  (add-to-list 'load-path (expand-file-name "~/mnt/Dropbox/src/git/org-mode/contrib/lisp")))

;;(require 'org-checklist)

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

(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-clock-sound "/usr/local/lib/tngchime.wav")

; Enable habit tracking (and a bunch of other modules)
;; (setq org-modules (quote (org-bbdb
;;                           org-bibtex
;;                           org-crypt
;;                           org-gnus
;;                           org-id
;;                           org-info
;;                           org-jsinfo
;;                           org-habit
;;                           org-inlinetask
;;                           org-irc
;;                           org-mew
;;                           org-mhe
;;                           org-protocol
;;                           org-rmail
;;                           org-vm
;;                           org-wl
;;                           org-w3m)))

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

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(require 'org-protocol)

(setq require-final-newline t)

(defvar bh/insert-inactive-timestamp t)

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

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-return-follows-link t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#300a24" :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(bold ((t (:foreground "light sky blue" :weight bold))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "royal blue" :slant oblique :weight semi-bold :height 1.2))))
 '(dired-header ((t (:inherit font-lock-type-face :foreground "sandy brown" :weight semi-bold :height 1.5 :family "Ubuntu Mono"))))
 '(mode-line ((t (:background "#300a24" :foreground "#855e79" :box (:line-width (1 . 1) :color "#411b35" :style pressed-button) :slant italic :height 0.9 :width normal :family "Ubuntu"))))
 '(neo-expand-btn-face ((t (:foreground "SkyBlue"))))
 '(org-agenda-calendar-event ((t (:inherit default :foreground "deep sky blue" :weight ultra-bold))))
 '(org-agenda-calendar-sexp ((t (:inherit default :foreground "light coral"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :background "sea green" :foreground "white" :box (:line-width (2 . 2) :color "grey75" :style released-button) :weight extra-bold))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :stipple nil :background "firebrick4" :foreground "white" :box (:line-width (2 . 2) :color "grey75" :style released-button)))))
 '(org-agenda-diary ((t (:inherit default :foreground "dark gray"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :foreground "indian red"))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :foreground "dark salmon"))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :foreground "gold"))))
 '(org-level-4 ((t (:extend nil :foreground "medium spring green" :weight normal))))
 '(org-level-5 ((t (:inherit outline-5 :extend nil :foreground "deep sky blue"))))
 '(org-level-6 ((t (:inherit outline-6 :extend nil :foreground "light slate blue"))))
 '(org-level-7 ((t (:inherit outline-7 :extend nil :foreground "cyan"))))
 '(org-level-8 ((t (:inherit outline-8 :extend nil :foreground "chocolate"))))
 '(scroll-bar ((t (:background "indian red"))))
 '(tab-bar ((t (:inherit variable-pitch :background "gray25" :foreground "black" :height 0.9 :width condensed))))
 '(tab-line ((t (:inherit variable-pitch :background "#300a24" :foreground "dark gray" :box (:line-width (1 . 1) :color "#522c46" :style pressed-button) :weight normal :height 0.9))))
 '(tab-line-highlight ((t (:background "#300a24" :foreground "#fddfff"))))
 '(tab-line-tab ((t (:background "#300a24" :foreground "#888888"))))
 '(tab-line-tab-current ((t (:background "#300a24" :foreground "#ebbfdf"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "#300a24" :foreground "#744e68"))))
 '(window-divider ((t (:foreground "SteelBlue4" :weight normal :height 1.0))))
 '(window-divider-first-pixel ((t (:foreground "SteelBlue4"))))
 '(window-divider-last-pixel ((t (:foreground "SteelBlue4")))))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark todos with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(todo\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(setq org-remove-highlights-with-change t)

(add-to-list 'Info-default-directory-list "~/mnt/Dropbox/etc/org/documents")

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

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Bookmark handling
;;
;(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

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

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to todo"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "todo")))))))

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

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'error)

(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-org-filesess-coding-system '(utf-8-unix . utf-8-unix))

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

(setq org-duration-format 'h:mm)

(fset 'fixslist
   (kmacro-lambda-form [escape ?x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?^ ?\\ ?* ?  return ?- ?  return] 0 "%d"))

(fset 'fixnlist
   (kmacro-lambda-form [escape ?x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?^ ?\[ ?0 ?- ?9 ?\] ?* ?\\ ?. ?  backspace ?\[ ?  ?\] ?* return ?- ?  return] 0 "%d"))

(appt-activate)
(global-set-key (kbd "C-x C-a") 'accent-menu)
(display-wttr-mode)
(put 'downcase-region 'disabled nil)

(defun set-category ()
  (interactive "P")
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-agenda-error))))
    (org-with-point-at marker
      (org-back-to-heading t)
      (org-set-property "CATEGORY" "work"))))

(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file "/usr/share/games/fortunes/fortunes")


(defun fix-fuxed-gnus ()
  (remove-hook 'message-mode-hook 'orgstruct++-mode)
  (remove-hook 'message-mode-hook 'bbdb-define-all-aliases))

;; embedded ddate function
(defun ddate ()
  "Insert the ddate into the current buffer"
  (interactive)
  (insert (shell-command-to-string "ddate +'%A, %B %e, %Y'")))

;; embedded fortune function
(defun fortune ()
  "Insert a short fortune into the current buffer"
  (interactive)
  (insert (shell-command-to-string "fortune -s fortunes")))

;; embedded weather function
(defun weather ()
  "Insert the local into the current buffer"
  (interactive)
  (insert
   (with-timeout(2)
       (shell-command-to-string "/home/stormrider/mnt/Dropbox/bin/getwx"))))

(defun key-legend ()
  "Insert the function key legend into the current buffer"
  (interactive)
  (insert (shell-command-to-string "cat ~/usr/share/personal/banner-sig/key-legend")))

;; easy time insert
(defun insert-current-time ()
  "Insert the current time"
 (interactive "*")
 (insert
  (format "%s, epoch %0.0f\n%s"
	  (current-time-string) (float-time) (calendar-day-of-year-string))))
(global-set-key "\C-ct" 'insert-current-time)

;; journal time insert
(defun journal-time-insert ()
  "Insert just the current time in the journal"
  (interactive "*")
  (insert
   (format "%s" (current-time-string))))

(defun print-banner ()
  (insert "~~~~~~~~~~~~~~~~~~~ M  O  T  D ~~~~~~~~~~~~~~~~~~~\n")
;;  (add-to-list 'default-frame-alist
;;	       '(title . (string("MOTD:" "hello, world"))
  (insert-current-time)
  (newline)
  (ddate)
  (fortune)
  (newline)
;;  (weather)
  (beginning-of-buffer)
  )
  
(defun set-up-windows ()
  (interactive)
  (split-window)
  (split-window-horizontally)
  ;;  (w3m-goto-url "stormrider.io")
  (find-file "/home/stormrider/etc/org/clock-table.org")
  (next-multiframe-window)
  (print-banner)
  (next-multiframe-window)
  (split-window-horizontally)
  (nav/term)
  (nav/term)
  (nav/term)
  (nav/term)
  (nav/term)
  (next-multiframe-window)
  (org-agenda-list)
  (next-multiframe-window)
;;  (org-journal-new-entry (current-time-string))
  (find-file "/home/stormrider/etc/org/diary.org")  
  (split-window-horizontally)
  (next-multiframe-window)
  (org-todo-list)
  (balance-windows))

(set-up-windows)
(neotree-dir "/home/stormrider/mnt/Dropbox")

(add-hook 'gnus-started-hook #'fix-fuxed-gnus)

;; set up function keys (and ^H)
(global-set-key "\C-h" 'delete-backward-char)
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)
(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))
(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))
(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))
(defun point-to-bottom ()
  "Put point at beginning of last visible line."
  (interactive)
  (move-to-window-line -1))
(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))
(global-set-key (kbd "<f1>") 'beginning-of-buffer)
(global-set-key (kbd "S-<f1>") 'point-to-top)
(global-set-key (kbd "C-<f1>") 'scroll-n-lines-behind)
(global-set-key (kbd "M-<f1>") 'line-to-top)
(global-set-key (kbd "<f2>") 'end-of-buffer)
(global-set-key (kbd "S-<f2>") 'point-to-bottom)
(global-set-key (kbd "C-<f2>") 'scroll-n-lines-ahead)
(global-set-key (kbd "<f3>") 'scroll-down-command)
(global-set-key (kbd "S-<f3>") 'org-agenda-earlier)
(global-set-key (kbd "C-<f3>") 'w3m-previous-anchor)
(global-set-key (kbd "M-<f3>") 'w3m-view-previous-page)
(global-set-key (kbd "<f4>") 'scroll-up-command)
(global-set-key (kbd "S-<f4>") 'org-agenda-later)
(global-set-key (kbd "C-<f4>") 'w3m-next-anchor)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "S-<f5>") 'find-file)
(global-set-key (kbd "C-<f5>") 'insert-file)
(global-set-key (kbd "<f6>") 'markdown-live-preview-mode)
(global-set-key (kbd "S-<f6>") 'dired-other-window)
(global-set-key (kbd "C-<f6>") 'find-file-other-window)
(global-set-key (kbd "<f7>") 'org-capture)
(global-set-key (kbd "S-<f7>") 'org-journal-new-entry)
(global-set-key (kbd "C-<f7>") 'org-publish)
(global-set-key (kbd "<f8>") 'org-agenda-redo)
(global-set-key (kbd "S-<f8>") 'org-clock-in)
(global-set-key (kbd "C-<f8>") 'org-clock-out)
(global-set-key (kbd "<f9>") 'w3m-search)
(global-set-key (kbd "S-<f9>") 'w3m-goto-url)
(global-set-key (kbd "C-<f9>") 'w3m-download)
(global-set-key (kbd "M-<f9>") 'w3m-history)
(global-set-key (kbd "<f10>") 'context-menu-open)
(global-set-key (kbd "S-<f10>") 'buffer-menu-open)
(global-set-key (kbd "C-<f10>") 'menu-bar-open)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "S-<f11>") 'toggle-frame-maximized)
(global-set-key (kbd "C-<f11>") 'list-package)
(global-set-key (kbd "M-<f11>") 'unscroll)
(global-set-key (kbd "<f12>") 'help-command)
(global-set-key (kbd "S-<f12>") 'info)
(global-set-key (kbd "C-<f12>") 'help-with-tutorial)
(global-set-key (kbd "M-<f12>") 'info-display-manual)

;; make symlinks safer
(add-hook 'find-file-hooks
         '(lambda ()
            (if (file-symlink-p buffer-file-name)
                (progn
                  (setq buffer-read-only t)
                  (message "File is a symlink")))))
(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (find-alternate-file target)
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))
(defun clobber-symlink ()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (if (yes-or-no-p (format "Replace %s with %s? "
                                     buffer-file-name
                                     target))
              (progn
                (delete-file buffer-file-name)
                (write-file buffer-file-name)))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

;; pause before the exit, so i can abort
;; doesn't matter that it's broken, it still creates the pause (i'm lazy)
(defadvice save-buffers-kill-terminal (before did-you-mean-that activate compile)
  "Did you really mean to exit?"
  (interactive "c" "Y/n: "))

;; cure for me sometimes typing "C-v" when i mean "C-y"
(defvar unscroll-to nil
  "Text position for next call to 'unscroll'.")
(defadvice scroll-up (before remember-for-unscroll
                      activate compile)
  "Remember where we started from, for 'unscroll'."
  (if (not (eq last-command 'scroll-up))
      (setq unscroll-to (point))))
(defun unscroll ()
  "Jump to location specified by 'unscroll-to'."
  (interactive)
  (if (not unscroll-to)     ;i.e., if unscroll-to is nil
      (error "Cannot unscroll yet"))
  (goto-char unscroll-to))

;; simple debug without having to init from command line
;; (setq debug-on-error t)

(org-agenda-to-appt)

(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; macros

(fset 'mktopic
   (kmacro-lambda-form [?\C-n ?\C-e ?\C-r ?* ?\C-f ?\C-f ?\C-  ?\C-e ?\M-w ?\C-e ?\C-u escape ?! ?g ?r ?e ?p ?  ?\" ?\C-y ?\" ?  ?/ ?h ?o ?m ?e ?/ ?s ?t ?o ?r ?m ?r ?i ?d ?e ?r ?/ ?u ?s ?r ?/ ?s ?r ?c ?/ ?m ?a ?a ?s tab ?/ ?s ?r ?c ?/ ?* ?. ?m ?d ?  ?| ?\S-  ?g ?r ?e ?p ?  ?\" ?< ?a ?  ?h ?r ?e ?f ?\" ?  ?| ?  ?c ?u ?t ?  ?- ?f ?1 ?  ?- ?d ?: ?  ?| ?\S-  ?r ?e ?v ?  ?| ?\S-  ?c ?u ?t ?  ?- ?f ?1 ?  ?- ?d ?- ?  ?| ?  ?c ?u ?t ?  ?- ?f ?2 ?  ?- ?d ?. ?  ?| ?\S-  ?r ?e ?v return ?\C-e ?: ?\C-n ?\C-k ?\C-p] 0 "%d"))

(fset 'mkelink
   (kmacro-lambda-form [escape ?x ?r ?e return ?\( ?h ?t ?t ?p ?s ?: ?\\ ?\( ?. ?* ?\\ ?\) ?\) return ?\( ?h ?t ?t ?p ?s ?: ?\\ ?1 ?\) ?\C-y return] 0 "%d"))

(fset 'mkh2
   (kmacro-lambda-form [?\C-a ?< backspace ?\C-  ?\C-e ?\M-w ?\C-a ?< ?a ?  ?h ?r ?e ?f ?= ?\" ?# ?h ?e ?a ?d ?i ?n ?g ?- ?- ?\C-y ?\" ?> ?< ?h ?2 ?  ?i ?d ?= ?\" ?h ?e ?a ?d ?i ?n ?g ?- ?- ?\C-y ?\" ?> ?\C-e ?< ?/ ?h ?2 ?> ?< ?/ ?a ?> ?\C-a ?\C-s ?# ?\C-  ?\C-s ?\" escape ?x ?r ?e return ?  return ?- return ?\C-s ?h ?e ?a ?d ?i ?n ?g ?\C-f ?\C-  ?\C-s ?\" escape ?x return ?  return ?- return] 0 "%d"))

(fset 'mkh3
   (kmacro-lambda-form [?\C-a ?< backspace ?\C-  ?\C-e ?\M-w ?\C-a ?< ?a ?  ?h ?r ?e ?f ?= ?\" ?# ?h ?e ?a ?d ?i ?n ?g ?- ?- ?\C-y ?\" ?> ?< ?h ?3 ?  ?i ?d ?= ?\" ?h ?e ?a ?d ?i ?n ?g ?- ?- ?\C-y ?\" ?> ?\C-e ?< ?/ ?h ?3 ?> ?< ?/ ?a ?> ?\C-a ?\C-s ?# ?\C-  ?\C-s ?\" escape ?x ?r ?e return ?  return ?- return ?\C-s ?h ?e ?a ?d ?i ?n ?g ?\C-f ?\C-  ?\C-s ?\" escape ?x return ?  return ?- return] 0 "%d"))

(fset 'mkh4
   (kmacro-lambda-form [?\C-a ?< backspace ?\C-  ?\C-e ?\M-w ?\C-a ?< ?a ?  ?h ?r ?e ?f ?= ?\" ?# ?h ?e ?a ?d ?i ?n ?g ?- ?- ?\C-y ?\" ?> ?< ?h ?4 ?  ?i ?d ?= ?\" ?h ?e ?a ?d ?i ?n ?g ?- ?- ?\C-y ?\" ?> ?\C-e ?< ?/ ?h ?4 ?> ?< ?/ ?a ?> ?\C-a ?\C-s ?# ?\C-  ?\C-s ?\" escape ?x ?r ?e return ?  return ?- return ?\C-s ?h ?e ?a ?d ?i ?n ?g ?\C-f ?\C-  ?\C-s ?\" escape ?x return ?  return ?- return] 0 "%d"))

(fset 'h2h
   (kmacro-lambda-form [?\C-a ?\C-  ?\C-n escape ?x return return] 0 "%d"))

(fset 'mkpic
   (kmacro-lambda-form [?\C-a ?< ?a ?  ?h ?r ?e ?f ?= ?\" ?\C-e ?\" ?  ?t ?a ?r ?g ?e ?t ?= backspace ?  ?- ?= ?  backspace backspace ?  backspace backspace ?= ?  ?\" ?_ ?b ?l ?a ?n ?k ?\" ?> ?< ?i ?m ?g ?  ?s ?r ?c ?e backspace ?= ?\" ?\C-y ?\" ?> ?< ?/ ?a ?>] 0 "%d"))

(fset 'reset
   (kmacro-lambda-form [?\C-s ?R ?E ?S ?E ?T ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b delete delete delete delete delete ?N ?E ?X ?T ?\C-s ?C ?L ?O ?S ?E ?D ?\C-a ?\C-  ?\C-e ?\C-a ?\C-  ?\C-s ?S ?C ?H ?\C-b ?\C-b ?\C-b backspace ?\C-x ?\C-s] 0 "%d"))

