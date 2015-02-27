(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'org-fstree)
(when *is-a-mac*
  (require-package 'org-mac-link)
  (autoload 'org-mac-grab-link "org-mac-link" nil t)
  (require-package 'org-mac-iCal))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-agenda-files (quote ("~/Dropbox/notes/TODOList.org" "~/Dropbox/notes/work.org" "~/Dropbox/notes/entertainment.org" "~/Dropbox/notes/practice.org" "~/Dropbox/notes/learning.org" "~/Dropbox/notes/life.org")))

(setq org-directory "/home/leo/Dropbox/notes")
(setq org-default-notes-file (concat org-directory "/capture.org"))

(require 'org-habit)
(add-to-list 'org-modules "org-habit")

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80
      org-agenda-tags-column -100
      org-habit-following-days 3
      org-habit-preceding-days 14
      org-habit-graph-column 100
      org-habit-show-habits-only-for-today nil)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing " jar-name " for org.")
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (file-exists-p org-ditaa-jar-path)
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))



(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        (flyspell-mode 1)
        (when (fboundp 'visual-line-mode)
          (visual-line-mode 1)))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'show-trailing-whitespace)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (when (fboundp 'visual-line-mode)
      (visual-line-mode -1))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(setq org-capture-templates
      '(("t" "Normal TODO item with a link"
         entry (file "~/Dropbox/notes/TODOList.org")
         "* %^{prompt|READY|TODO|BEGINED|TOBREAKDOWN|SOMEDAY} %? %^g\n%i\n %a")
        ("l" "Normal Life item with a link"
         entry (file "~/Dropbox/notes/life.org")
         "* %^{prompt|READY|TODO|BEGINED|TOBREAKDOWN|SOMEDAY} %? %^g\n%i\n %a")
        ("w" "Normal work item with a link"
         entry (file "~/Dropbox/notes/work.org")
         "* %^{prompt|READY|TODO|BEGINED|TOBREAKDOWN|SOMEDAY} %? %^g\n%i\n %a")
        ("e" "Normal entertainment item with a link"
         entry (file "~/Dropbox/notes/entertainment.org")
         "* %^{prompt|READY|TODO|BEGINED|TOBREAKDOWN|SOMEDAY} %? %^g\n%i\n %a")
        ("p" "Normal Practice item with a link"
         entry (file "~/Dropbox/notes/practice.org")
         "* %^{prompt|READY|TODO|BEGINED|TOBREAKDOWN|SOMEDAY} %? %^g\n%i\n %a")
        ("L" "Normal Learning item with a link"
         entry (file "~/Dropbox/notes/learning.org")
         "* %^{prompt|READY|TODO|BEGINED|TOBREAKDOWN|SOMEDAY} %? %^g\n%i\n %a")

        ("d" "Log the task that already done"
         entry (file "~/Dropbox/notes/TODOList.org")
         "* %^{prompt|DONE|BEGINED} %? %^g\n:LOGBOOK:\nCLOCK: %^U--%^U\n:END:\n%i\n %a" :clock-keep t)

        ("b" "Log the task that already begined"
         entry (file "~/Dropbox/notes/TODOList.org")
         "* BEGINED %? %^g\n:LOGBOOK:\nCLOCK: %^U\n:END:\n%i\n %a" :clock-in t :clock-resume t)

        ("c" "add checkItem"
         checkitem (file "~/Dropbox/notes/TODOList.org")
         "- [ ] %? %a %i")
        ))



;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "SOMEDAY(s)" "READY(r)" "BEGINED(b!)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "TOBREAKDOWN(o)" "PROJECT(p)" "REPEATING(R/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-repeat-to-state "REPEATING")

(setq org-todo-keyword-faces
      '(("PROJECT" . "cyan")
        ("TODO" . "red")
        ("TOBREAKDOWN" . "tomato")
        ("SOMEDAY" . "chocolate")
        ("WAITING" . "sandy brown")
        ("REPEATING" . "goldenrod")
        ("READY" . "khaki")
        ("BEGINED" . "yellow green")
        ))

(setq org-priority-faces
      (quote ((65 . "tomato") (66 . "dark salmon") (67 . "rosy brown"))))

;; Strike through headlines for DONE tasks in Org
;; http://sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "LightSalmon" :strike-through t)))))



;; Change TODO to DONE automatically after all subtask are done.
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "PROJECT"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-tag-persistent-alist '((:startgroup . nil)
                                 ("acg" . ?A)
                                 (:grouptags . nil)
                                 ("animate" . ?a)
                                 ("comics" . ?C)
                                 ("game" . nil)
                                 ("music" . ?m)
                                 (:endgroup . nil)

                                 (:startgroup . nil)
                                 ("learn" . ?l)
                                 ("work" . ?w)
                                 ("life" . ?f)
                                 (:endgroup . nil)

                                 (:startgroup . nil)
                                 ("L_term" . nil)
                                 ("S_term" . nil)
                                 (:endgroup . nil)

                                 ("coding" . ?c)
                                 ("shell" . ?s)
                                 ("emacs" . ?e)
                                 ("linux" . ?L)
                                 ("git" . ?g)
                                 ("reading" . ?r)

                                 (:newline . nil)

                                 ("@offic" . ?o)
                                 ("@home" . ?h)
                                 ("@pc" . ?P)

                                 (:newline . nil)

                                 ("#MIT#" . ?M)
                                 ("#TOBE#" . ?T)
                                 ("#URGENT#" . ?U)

                                 ))

(defun sacha/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

(defvar sacha/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")

(defvar sacha/org-agenda-contexts
  '((tags-todo "+@learn")
    (tags-todo "+@work")
    (tags-todo "+@reading")
    (tags-todo "+@hacking")
    (tags-todo "+@computer")
    (tags-todo "+@home"))
  "Usual list of contexts.")



;;; Agenda views

(setq org-agenda-compact-blocks t
      org-agenda-sticky t
      org-agenda-start-on-weekday nil
      org-agenda-span 'day
      org-agenda-include-diary nil
      org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
        (todo category-up effort-up)
        (tags category-up effort-up)
        (search category-up))
      org-agenda-window-setup 'current-window
      org-agenda-custom-commands
      '(
        ("u" "Timeline for today and tasks that under going"
         ((agenda "" )
          (todo "BEGINED")
          (todo "READY")
          (todo "REPEATING")
          (todo "WAITING"))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-clockreport-mode t)
          (org-agenda-log-mode-items '(clock closed))))

        ("w" "Weekly Review"
         ((agenda ""
                  ((org-agenda-ndays 7)          ;; review upcoming deadlines and appointments
                   (org-agenda-show-log t)
                   ;;                   (org-agenda-start-on-weekday 1) ;; agenda start with monday
                   (org-agenda-log-mode-items '(clock closed))
                   (org-agenda-clockreport-mode t)
                   (org-agenda-repeating-timestamp-show-all nil)));; ensures that repeating events appear only once

          (todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
          (todo "BEGINED")
          (todo "READY")
          (todo "REPEATING")
          (todo "WAITING")
          (todo "SOMEDAY")
          (todo "TOBREAKDOWN")
          (todo "TODO")
          (todo "DONE")
          (todo "CANCELLED")))

        ("x" "Items that under going" todo "READY|BEGINED")

        ("P" "By priority"
         ((tags-todo "+PRIORITY=\"A\"")
          (tags-todo "+PRIORITY=\"B\"")
          (tags-todo "+PRIORITY=\"\"")
          (tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-prefix-format "%-10c %-10T %e ")
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))

        ("g" . "Go to specific category")
        ("ge" "Entertainment" tags-todo "CATEGORY=\"Play\"")
        ("gw" "Work" tags-todo "CATEGORY=\"Work\"")
        ("gl" "Learn" tags-todo "CATEGORY=\"Learn\"")
        ("gL" "Life" tags-todo "CATEGORY=\"Life\"")
        ("gp" "Practice" tags-todo "CATEGORY=\"Practice\"")

        ("n" "Agenda and all TODO's"
         ((agenda ""
                  ((org-agenda-ndays 7)
                   (org-agenda-show-log t)
                   ))
          (tags-todo "/-TOBREAKDOWN-SOMEDAY")))
        ))



;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

(setq org-clock-idle-time 10)

;; Change task state to BEGINED when clocking in
(setq org-clock-in-switch-to-state "BEGINED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))



;; Remove empty LOGBOOK drawers on clock out
(defun sanityinc/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'sanityinc/remove-empty-drawer-on-clock-out 'append)



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")





(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t))))


;; Add function to directly create src-block.
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))


(add-hook 'org-mode-hook '(lambda ()
                            ;; turn on flyspell-mode by default
                            (flyspell-mode 1)
                            ;; C-TAB for expanding
                            ;; (local-set-key (kbd "C-<tab>")
                            ;;                'yas/expand-from-trigger-key)
                            ;; keybinding for editing source code blocks
                            (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-c s i")
                                           'org-insert-src-block)
                            ;; turn on indent mode by default
                            (org-indent-mode t)
                            ))


;;;; Mobile Org
(setq org-enforce-todo-dependencies t)
(setq org-mobile-directory "~/Dropbox/mobileorg")
(setq org-mobile-inbox-for-pull "~/Dropbox/mobileorg/from-mobil.org")

(setq org-mobile-agendas '("w"))
;; push and pull everytime start and quit emacs
;; causing desktop not save
;;(add-hook 'after-init-hook 'org-mobile-pull)
;;(add-hook 'kill-emacs-hook 'org-mobile-push)

;; mobile sync
(defvar org-mobile-sync-timer nil)
;; sync if emacs has entered idle state for 10 mins
(defvar org-mobile-sync-idle-secs (* 60 10))
(defun org-mobile-sync ()
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))
(defun org-mobile-sync-enable ()
  "enable mobile org idle sync"
  (interactive)
  (setq org-mobile-sync-timer
        (run-with-idle-timer org-mobile-sync-idle-secs t
                             'org-mobile-sync)));
(defun org-mobile-sync-disable ()
  "disable mobile org idle sync"
  (interactive)
  (cancel-timer org-mobile-sync-timer))
(org-mobile-sync-enable)

(require 'org-depend)

(provide 'init-org)
