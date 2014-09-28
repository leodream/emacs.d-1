(require-package 'company)
(require-package 'ecb)
(require-package 'emacs-eclim)
(require-package 'fold-dwim)
(require-package 'fold-dwim-org)
(require-package 'fold-this)
(require-package 'ggtags)
(require-package 'goto-last-change)
(require-package 'groovy-mode)
(require-package 'hide-lines)
(require-package 'powershell-mode)
(require-package 'vlf)
(require-package 'w3m)
(require-package 'yasnippet)
(require-package 'better-registers)

;;----------------------------------------------------------------------------
;; Goto-the-last-change
;;----------------------------------------------------------------------------
;;(load-file (expand-file-name "~/.emacs.d/goto-last-change.el"))
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)



;;----------------------------------------------------------------------------
;; ECB Setting
;;----------------------------------------------------------------------------
(require 'ecb-autoloads)
(custom-set-variables
 '(ecb-windows-width 0.25))

;; Make Winner mode runable after ecb-deactivate
(add-hook 'ecb-deactivate-hook
          '(lambda ()
             (ecb-disable-advices 'ecb-winman-not-supported-function-advices t)))




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
(require 'eclimd)

;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("~/Software/eclipse")))

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
(add-hook 'powershell-mode-hook 'hs-minor-mode)

(add-hook 'c-mode-common-hook   'fold-dwim-org/minor-mode)
(add-hook 'emacs-lisp-mode-hook 'fold-dwim-org/minor-mode)
(add-hook 'java-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'lisp-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'perl-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'sh-mode-hook         'fold-dwim-org/minor-mode)
(add-hook 'powershell-mode      'fold-dwim-org/minor-mode)



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

(global-linum-mode 1)



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
;; browse-kill-ring
;;---------------------------------------------------------------------------
(require-package 'browse-kill-ring)
(global-set-key (kbd"M-y") 'browse-kill-ring)





;;---------------------------------------------------------------------------
;; vlf-setting
;;---------------------------------------------------------------------------
;; (add-to-list 'load-path "/usr/bin/")

;; (defun my-find-file-check-make-large-file-read-only-hook ()
;;   "If a file is over a given size, make the buffer read only."
;;   (when (> (buffer-size) (* 1024 1024))
;;     (setq buffer-read-only t)
;;     (buffer-disable-undo)
;;     (fundamental-mode)))

;; (add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)
(require 'vlf-integrate)







;;---------------------------------------------------------------------------
;; others
;;---------------------------------------------------------------------------
(require-package 'tabbar)

(menu-bar-mode -1)

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
;; copy file name
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


;;---------------------------------------------------------------------------
;; better-registers
;;---------------------------------------------------------------------------
(require 'better-registers)
(better-registers-install-save-registers-hook)
(load better-registers-save-file)
(provide 'init-leo)
p
