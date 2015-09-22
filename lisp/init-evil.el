(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-nerd-commenter)


;;------------------------
;; evil setting
;;------------------------
(require 'evil)
(require 'evil-leader)
(setq evil-toggle-key "")   ; remove default evil-toggle-key C-z, manually setup later
(setq evil-want-C-i-jump nil)   ; don't bind [tab] to evil-jump-forward
(evil-mode 1)
(global-evil-leader-mode 1)

(setq evil-move-cursor-back nil) ; move to the end of the line

;; remove all keybindings from insert-state keymap, use emacs-state when editing
(setcdr evil-insert-state-map nil)

;; ESC to switch back normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; TAB to indent in normal-state
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

;; C-i to jump forward in normal-state
(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Use j/k to move one visual line insted of gj/gk
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "dd" 'sdcv-search-pointer+
  "dD" 'sdcv-search-pointer
  "di" 'sdcv-search-input+
  "dI" 'sdcv-search-input

  "mmm" 'mpc-which-song
  "mmn" 'mpc-next-prev-song
  "mmp" '(lambda () (interactive) (mpc-next-prev-song t))

  "ls" 'highlight-symbol
  "lq" 'highlight-symbol-query-replace
  "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols

  "rnr" 'rinari-web-server-restart
  "rnc" 'rinari-find-controller
  "rnv" 'rinari-find-view
  "rna" 'rinari-find-application
  "rnk" 'rinari-rake
  "rnm" 'rinari-find-model
  "rnl" 'rinari-find-log
  "rno" 'rinari-console
  "rnt" 'rinari-find-test

  "rw" 'window-configuration-to-register
  "rj" 'jump-to-register
  "rm" 'bookmark-set
  "rl" 'bookmark-bmenu-list
  "rb" 'bookmark-jump

  "hst" 'hs-toggle-fold
  "hsa" 'hs-toggle-fold-all
  "hsh" 'hs-hide-block
  "hss" 'hs-show-block

  "xnn" 'narrow-or-widen-dwim
  "xnw" 'widen
  "xnd" 'narrow-to-defun
  "xnr" 'narrow-to-region

  "fb" 'flyspell-buffer
  "fe" 'flyspell-goto-next-error
  "fa" 'flyspell-auto-correct-word
  "pe" 'flymake-goto-prev-error
  "ne" 'flymake-goto-next-error
  "fw" 'ispell-word

  "wrn" 'httpd-restart-now
  "wrd" 'httpd-restart-at-default-directory

  "hd" 'describe-function
  "hf" 'find-function
  "hk" 'describe-key
  "hv" 'describe-variable

  "xm" 'smex
  "xb" 'ido-switch-buffer
  "kk" 'ido-kill-buffer
  "xk" 'ido-kill-buffer
  "ri" 'yari-helm
  "mx" 'helm-M-x
  "hM" 'helm-bookmarks
  "hh" 'browse-kill-ring
  "hr" 'helm-recentf
  "ha" 'helm-apropos
  "hi" 'helm-semantic-or-imenu
  "hI" 'helm-imenu-in-all-buffers
  "hl" 'helm-locate
  "ho" 'helm-occur
  "hp" 'helm-list-emacs-process

  "hS" 'helm-surfraw
  "ht" 'helm-top
  "hx" 'helm-register
  "h<tab>" 'helm-lisp-completion-at-point
  "hm" 'helm-man-woman
  "h," 'calc
  "he" 'helm-etags-select
  "h/" 'helm-find
  "hb" 'helm-resume
  "ff" 'helm-for-files ;; "C-c f"
  "xf" 'helm-find-files
  "y" 'helm-show-kill-ring
  "B" 'helm-mini
  "bb" 'helm-mini
  "SPC" 'helm-all-mark-rings

  "p!" 'projectile-run-shell-command-in-root
  "p&" 'projectile-run-async-shell-command-in-root
  "pD" 'projectile-dired
  "pF" 'helm-projectile-find-file-in-known-projects
  "pI" 'projectile-ibuffer
  "pP" 'projectile-test-project
  "pR" 'projectile-regenerate-tags
  "pS" 'projectile-save-project-buffers
  "pT" 'projectile-find-test-file
  "pa" 'helm-projectile-find-other-file
  "pb" 'helm-projectile-switch-to-buffer
  "pc" 'projectile-compile-project
  "pd" 'helm-projectile-find-dir
  "pe" 'helm-projectile-recentf
  "pf" 'helm-projectile-find-file
  "pg" 'helm-projectile-find-file-dwim
  "ph" 'helm-projectile
  "pi" 'projectile-invalidate-cache
  "pj" 'projectile-find-tag
  "pk" 'projectile-kill-buffers
  "pl" 'projectile-find-file-in-directory
  "pm" 'projectile-commander
  "po" 'projectile-multi-occur
  "pp" 'helm-projectile-switch-project
  "pr" 'projectile-replace
  "pt" 'projectile-toggle-between-implementation-and-test
  "pu" 'projectile-run-project
  "pv" 'projectile-vc
  "pz" 'projectile-cache-file

  "ma" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  ;; "opt" is occupied by my-open-project-todo
  ;; recommended in html
  "md" 'mc/mark-all-like-this-dwim

  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cy" 'evilnc-copy-and-comment-lines
  "ch" 'evilnc-comment-or-uncomment-paragraphs
  "cg" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key

  "/" 'goto-last-change
  ";" 'ace-jump-mode
  ":" 'ace-jump-line-mode
  "[" 'beginning-of-defun
  "bf" 'beginning-of-defun
  "bu" 'backward-up-list
  "ef" 'end-of-defun
  "]" 'end-of-defun
  "mf" 'mark-defun
  "bs" '(lambda () (interactive) (goto-edge-by-comparing-font-face -1))
  "es" 'goto-edge-by-comparing-font-face

  "." 'evil-ex
  "<escape>" 'keyboard-quit
  "jp" 'jsons-print-path
  "oc" 'occur
  "ut" 'undo-tree-visualize
  "ar" 'align-regexp
  "sd" 'su-edit
  "sc" 'shell-command
  "sl" 'sort-lines
  "dj" 'dired-jump ;; open the dired from current file
  "eb" 'eval-buffer
  "ee" 'eval-expression
  "xe" 'eval-last-sexp

  "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
  "xc" 'save-buffers-kill-terminal
  "xh" 'mark-whole-buffer
  "ww" 'save-buffer
  "ss" 'save-buffer
  "xs" 'save-some-buffers
  "xx" 'cua-exchange-point-and-mark

  "oo" 'switch-window
  "ee" 'back-to-previous-buffer
  "pb" 'back-to-previous-buffer
  "|"  'split-window-horizontally
  "_"  'split-window-vertically-instead
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wh" 'windmove-left
  "wl" 'windmove-right
  "xr" 'rotate-windows
  "cu" 'winner-undo
  "cr" 'winner-redo
  "bk" 'buf-move-up
  "bj" 'buf-move-down
  "bh" 'buf-move-left
  "bl" 'buf-move-right
  "vv" 'scroll-other-window
  "vu" 'scroll-other-window-up
  "tff" 'toggle-frame-fullscreen
  "tfm" 'toggle-frame-maximized
  "xz" 'suspend-frame

  "0" 'delete-window
  "1" 'sanityinc/toggle-delete-other-windows
  "2" (split-window-func-with-other-buffer 'split-window-vertically)
  "3" (split-window-func-with-other-buffer 'split-window-horizontally)

  "bm" 'pomodoro-start ;; beat myself
  "cc" 'org-capture
  "ca" 'org-agenda
  "cn" 'outline-next-visible-heading
  "cp" 'outline-previous-visible-heading

  "gg" 'magit-status
  "gt" 'ggtags-find-tag-dwim
  "gr" 'ggtags-find-reference
  ;; @see https://github.com/pidu/git-timemachine
  ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
  "gm" 'git-timemachine-toggle
  )

(evil-leader/set-key-for-mode 'org-mode
  "cf" 'org-forward-heading-same-level
  "cb" 'org-backward-heading-same-level
  "si" 'org-insert-src-block
  "se" 'org-edit-src-code
  "c$" 'org-archive-subtree ; `C-c $'
  "c<" 'org-promote-subtree ; `C-c C-<'
  "c>" 'org-demote-subtree ; `C-c C->'
  "xi" 'org-clock-in ; `C-c C-x C-i'
  "xo" 'org-clock-out ; `C-c C-x C-o'
  "cxr" 'org-clock-report ; `C-c C-x C-r'
  "cxb" 'org-tree-to-indirect-buffer
  "otl" 'org-toggle-link-display
  )




;;;
(defun swap-np (mode)
  "swap n/p with j/k for a given mode string"
  (interactive)
  (let ((map (symbol-value (intern (format "%s-map" mode)))))
    (let ((jkey (lookup-key map (kbd "j")))
          (kkey (lookup-key map (kbd "k"))))
      (define-key map "j" (lookup-key map (kbd "n")))
      (define-key map "k" (lookup-key map (kbd "p")))
      (define-key map "n" jkey)
      (define-key map "p" kkey)
      (define-key map "`" (lookup-key map (kbd "SPC"))))
    )
  )

(defun swap-np-in-magit ()
  "invoke swap-np for all map related to magit"
  (interactive)
  (mapc (lambda (mode) (swap-np mode))
        '("magit-status-mode"
          "magit-mode"
          "magit-staged-section"
          "magit-unstaged-section"
          "magit-untracked-section"
          "magit-stash-section"
          "magit-remote-section"
          "magit-commit-section"
          "magit-branch-section"))
  (remove-hook 'magit-status-mode-hook 'swap-np-in-magit))

;; (add-hook 'magit-status-mode-hook
;;           (lambda () (swap-np-in-magit)))

(swap-np "org-agenda-mode")
(swap-np "package-menu-mode")


;; (loop for mode in
;;       '(magit-status-mode
;;         org-agenda-mode)
;;       do
;;       (add-hook (intern (format "%s-hook" mode))
;;                 (lambda () (swap-np mode)))
;;       ;; (eval-after-load mode
;;       ;;   '(swap-np mode))
;;       )

(setq evil-leader/in-all-states 1)
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode" "org-agenda-mode" "package-menu-mode"))

(provide 'init-evil)
