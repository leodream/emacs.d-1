;; -*- lexical-binding: t -*-
(require-package 'autopair)
(require-package 'better-registers)
(require-package 'company)
(require-package 'emacs-eclim)
(require-package 'fold-dwim)
(require-package 'fold-dwim-org)
(require-package 'fold-this)
(require-package 'ggtags)
(require-package 'god-mode)
(require-package 'goto-last-change)
(require-package 'groovy-mode)
(require-package 'helm)
(require-package 'helm-projectile)
(require-package 'hide-lines)
(require-package 'projectile)
(require-package 'vlf)
(require-package 'w3m)
(require-package 'yasnippet)

;;----------------------------------------------------------------------------
;; Goto-the-last-change
;;----------------------------------------------------------------------------
;;(load-file (expand-file-name "~/.emacs.d/goto-last-change.el"))
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)




;;----------------------------------------------------------------------------
;; Highlight-symbol mode
;;----------------------------------------------------------------------------
;;(global-set-key [(control f3)] 'highlight-symbol-query-replace)
(global-set-key [f3] 'highlight-symbol-at-point)
(global-set-key [f5] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-next)

(add-hook 'groovy-mode-hook     'highlight-symbol-mode)
(add-hook 'groovy-mode-hook     'highlight-symbol-nav-mode)
(add-hook 'visual-basic-mode-hook 'highlight-symbol-mode)
(add-hook 'visual-basic-mode-hook 'highlight-symbol-nav-mode)




;;----------------------------------------------------------------------------
;; YASnippet Setting
;;----------------------------------------------------------------------------
(require 'yasnippet)
;;(yas-global-mode 1)



;;----------------------------------------------------------------------------
;; emacs-eclim setting
;;----------------------------------------------------------------------------
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(custom-set-variables
 '(eclim-eclipse-dirs '("~/Software/eclipse")))

;; Variables
(setq eclim-auto-save t
      eclim-executable "/home/leo/Software/eclipse/eclim"
      eclimd-executable "/home/leo/Software/eclipse/eclimd"
      eclimd-wait-for-process nil
      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)



;;----------------------------------------------------------------------------
;; Tags Setting
;;----------------------------------------------------------------------------
(require 'ggtags)
(add-auto-mode 'java-mode "\\.java\\'")
(add-hook 'java-mode-hook '(lambda ()
                             (setq tab-width 4)
                             (eclim-mode t)
                             (ggtags-mode t)))

(add-hook 'nxml-mode-hook '(lambda ()
                             (setq tab-width 4)))

(defun find-adexc-file (pattern)
  (interactive (list (read-string "Pattern of the file name:")))
  (find-dired "/home/leo/Program/src/ADExchange_git/ADExchange"
              (concatenate 'string "-iname \"*" pattern "*\"")))



;;----------------------------------------------------------------------------
;; hs-mode Setting
;;----------------------------------------------------------------------------
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(global-set-key (kbd "M-9") 'fold-dwim-toggle)

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)

;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open t)

;; Add more here

;; Displaying overlay content in echo area or tooltip
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

(setq hs-set-up-overlay 'display-code-line-counts)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(add-hook 'c-mode-common-hook   'fold-dwim-org/minor-mode)
(add-hook 'emacs-lisp-mode-hook 'fold-dwim-org/minor-mode)
(add-hook 'java-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'lisp-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'perl-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'sh-mode-hook         'fold-dwim-org/minor-mode)


(setq outline-regexp "\\(?:\\([ \t]*.*\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)\\|[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{\\)" )




;;----------------------------------------------------------------------------
;; re-open file if it is read-only
;;----------------------------------------------------------------------------
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (sudo-find-file (ad-get-arg 0))
    ad-do-it))

(defun sudo-find-file (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defun su-edit ()
  "Edit the current buffer file as superuser."
  (interactive)
  (let((window-start (window-start))
       (point (point))
       (pres-reg-beg (if (use-region-p) (region-beginning) nil)))
    (find-alternate-file (format "/sudo::%s" (buffer-file-name)))
    (message (format "The variable is %d." pres-reg-beg))
    (if pres-reg-beg (set-mark pres-reg-beg)) ; same: set-mark-command
    (goto-char point)
    (set-window-start nil window-start) )) ; nil - the selected window




;;----------------------------------------------------------------------------
;; sdcv
;;----------------------------------------------------------------------------
(load-file (expand-file-name "~/.emacs.d/showtip.el"))
(load-file (expand-file-name "~/.emacs.d/sdcv.el"))
(require 'showtip)
(require 'sdcv)
(global-set-key (kbd "C-c d") 'sdcv-search-pointer+)
(global-set-key (kbd "C-c D") 'sdcv-search-pointer)
(global-set-key (kbd "C-c i") 'sdcv-search-input)
(global-set-key (kbd "C-c I") 'sdcv-search-input+)
(setq sdcv-dictionary-simple-list        ;; a simple dictionary list
      '(
        "朗道英汉字典5.0"
        "朗道汉英字典5.0"
        ))




;;----------------------------------------------------------------------------
;; select current word. Twrice to select all woard in ""
;;----------------------------------------------------------------------------
;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (region-active-p)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key (kbd "M-8") 'extend-selection)



;;----------------------------------------------------------------------------
;; Kill emacs server using client
;;----------------------------------------------------------------------------
(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to shutdown save buffers and
shutdown the emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers or active clients
or frame.  If so an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

                                        ; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
                                        (> (length (frame-list)) 1)
                                        ))

                                        ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
                                        ;        (x-initialize-window-system)
        )
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

                                        ; Save the current frame.
    (setq new-frame (selected-frame))


                                        ; When displaying the number of clients and frames:
                                        ; subtract 1 from the clients for this client.
                                        ; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when ( or (not active-clients-or-frames)
               (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

                                        ; If the user quits during the save dialog then don't exit emacs.
                                        ; Still close the terminal though.
      (let((inhibit-quit t))
                                        ; Save buffers
        (with-local-quit
          (save-some-buffers))

        (if quit-flag
            (setq quit-flag nil)
                                        ; Kill all remaining clients
          (progn
            (dolist (client server-clients)
              (server-delete-client client))
                                        ; Exit emacs
            (kill-emacs)))
        ))

                                        ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )

(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-base-buffer buffer))
                 (or
                  (buffer-file-name buffer)
                  (progn
                    (set-buffer buffer)
                    (and buffer-offer-save (> (buffer-size) 0))))
                 )
        (setq modified-found t)
        )
      )
    modified-found
    )
  )



;;----------------------------------------------------------------------------
;; Holidays
;;----------------------------------------------------------------------------
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
;;(setq holiday-other-holidays cal-china-x-chinese-holidays)

(setq calendar-holidays ;;cal-china-x-important-holidays
      (append cal-china-x-important-holidays '(
                                               (holiday-lunar 1 15 "元宵节" 0)
                                               (holiday-lunar 7 7 "七夕" 0)
                                               (holiday-lunar 9 9 "重阳节" 0)

                                               (holiday-lunar 3 5 "Brother's Birthday" 0)

                                               (holiday-chinese-winter-solstice)

                                               (holiday-fixed 2 14 "Valentine's Day")
                                               (holiday-float 5 0 2 "Mother's Day")
                                               (holiday-float 6 0 3 "Father's Day")
                                               (holiday-fixed 12 25 "Christmas")
                                               )))



;;----------------------------------------------------------------------------
;; Press % to jump to the matching (){}
;;----------------------------------------------------------------------------
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))




;;---------------------------------------------------------------------------
;; vlf-setting
;;---------------------------------------------------------------------------
(require 'vlf-setup)



;;---------------------------------------------------------------------------
;; Helm
;;---------------------------------------------------------------------------
(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)


(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(setq helm-locate-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq session-save-print-spec '(t nil 40000))  ;; need this to make helm-all-mark-ring works with session.el


(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)

(helm-autoresize-mode 1)
(helm-mode 1)



;;---------------------------------------------------------------------------
;; others
;;---------------------------------------------------------------------------
(require-package 'tabbar)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(global-hl-line-mode 1)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))



;;---------------------------------------------------------------------------
;; utils functions
;;---------------------------------------------------------------------------
(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun hide-ctrl-M ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; {{ message buffer things
(defun erase-message-buffer (&optional num)
  "Erase the content of the *Messages* buffer in emacs.
    Keep the last num lines if argument num if given."
  (interactive "p")
  (let ((buf (cond
              ((eq 'ruby-mode major-mode) "*server*")
              (t "*Messages*"))))
    (erase-specific-buffer num buf)))

;; {{ @see http://emacs.stackexchange.com/questions/14129/which-keyboard-shortcut-to-use-for-navigating-out-of-a-string
(defun font-face-is-similar (f1 f2)
  (let (rlt)
    ;; (message "f1=%s f2=%s" f1 f2)
    (if (eq f1 f2) (setq rlt t)
      ;; C++ comment has different font face for limit and content
      ;; f1 or f2 could be a function object because of rainbow mode
      (if (and (string-match "-comment-" (format "%s" f1)) (string-match "-comment-" (format "%s" f2)))
          (setq rlt t)))
    rlt))

(defun goto-edge-by-comparing-font-face (&optional step)
  "Goto either the begin or end of string/comment/whatever.
If step is -1, go backward."
  (interactive "P")
  (let ((cf (get-text-property (point) 'face))
        (p (point))
        rlt
        found
        end)
    (unless step (setq step 1)) ;default value
    (setq end (if (> step 0) (point-max) (point-min)))
    (while (and (not found) (not (= end p)))
      (if (not (font-face-is-similar (get-text-property p 'face) cf))
          (setq found t)
        (setq p (+ p step))))
    (if found (setq rlt (- p step))
      (setq rlt p))
    ;; (message "rlt=%s found=%s" rlt found)
    (goto-char rlt)))
;; }}



;;-----------------------------
;; Wubi Eim
;;----------------------------
(add-to-list 'load-path "~/.emacs.d/site-lisp/eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")

;; 用 ; 暂时输入英文
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)

(set-fontset-font "fontset-default" 'unicode"WenQuanYi Zen Hei") ;;for linux
(setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))



;;------------------------
;; org publish
;;------------------------
(require 'ox-publish)
(require 'ox-html)
(setq org-publish-project-alist
      '(  ("org-notes"
           :base-directory "~/Dropbox/notes/"
           :base-extension "org"
           :publishing-directory "~/Dropbox/public_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)
          ("org-static"
           :base-directory "~/Dropbox/notes/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Dropbox/public_html/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org" :components ("org-notes" "org-static"))))



;;------------------------
;; scheme setting
;;------------------------
(require 'cmuscheme)

(defun kh/get-scheme-proc-create ()
  "Create one scheme process if no one is created."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name))))

(defun kh/scheme-send-last-sexp ()
  "A replacement of original `scheme-send-last-sexp':
1. check if scheme process exists, otherwise create one
2. make sure the frame is splitted into two windows, current one is the scheme
   source code window, the other one is the scheme process window
3. run `scheme-send-last-sexp'

PS: this function is inspired by Wang Yin."
  (interactive)
  (kh/get-scheme-proc-create)
  (cond ((= 2 (count-windows))
         (other-window 1)
         (unless (string= (buffer-name)
                          scheme-buffer)
           (switch-to-buffer scheme-buffer))
         (other-window 1))
        (t
         (delete-other-windows)
         (split-window-vertically (floor (* 0.68 (window-height))))
         (other-window 1)
         (switch-to-buffer scheme-buffer)
         (other-window 1)))
  (scheme-send-last-sexp))

(setq scheme-program-name "mit-scheme")

(defun lhe/scheme-send-last-sexp ()
  "Run 'scheme-send-last-sexp' and move scheme buffer's
  point to the end"
  (interactive)
  (let ((scheme-window (get-buffer-window scheme-buffer)))
    (if scheme-window
        (set-window-point scheme-window
                          (+ 1 (buffer-size (window-buffer scheme-window))))))
  (scheme-send-last-sexp))
(defun lhe/new-parenth (&optional n)
  "(a|) => (a) (|)"
  (interactive "P")
  (if n
      (dotimes (i (- n 1))
        (paredit-close-round)))
  (paredit-close-round-and-newline)
  (paredit-open-round))

(defun lhe/move-to-new-parenth ()
  "(a |b) => (a) (b|)"
  (interactive))

(add-hook 'scheme-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-x C-M-e") 'kh/scheme-send-last-sexp)
              (local-set-key (kbd "C-x C-e") 'lhe/scheme-send-last-sexp)
              (local-set-key (kbd "C-c e") 'scheme-send-definition)
              (local-set-key (kbd "C-x e") 'lhe/scheme-send-last-sexp)
              (local-set-key (kbd "C-M-(") 'lhe/new-parenth)
              (rainbow-delimiters-mode t)
              (paredit-mode t)
              (autopair-mode t)))

(require 'autopair)
(require 'init-evil)

(provide 'init-leo)
