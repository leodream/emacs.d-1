(require-package 'goto-last-change)
(require-package 'ecb)
(require-package 'yasnippet)
(require-package 'groovy-mode)
(require-package 'emacs-eclim)
(require-package 'company)
(require-package 'popup-kill-ring)


;;----------------------------------------------------------------------------
;; Goto-the-last-change
;;----------------------------------------------------------------------------
(load-file (expand-file-name "~/.emacs.d/goto-last-change.el"))
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)



;;----------------------------------------------------------------------------
;; ECB Setting
;;----------------------------------------------------------------------------
(require 'ecb-autoloads)
(custom-set-variables
 '(ecb-source-path (quote ("/home/leo/Program" ("/home/leo/Program/TestProject" "yes"))))
 '(ecb-windows-width 0.25))

;; Make Winner mode runable axfter ecb-deactivate
(add-hook 'ecb-deactivate-hook
          '(lambda ()
             (ecb-disable-advices 'ecb-winman-not-supported-function-advices t)))



;;----------------------------------------------------------------------------
;; YASnippet Setting
;;----------------------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)



;;----------------------------------------------------------------------------
;; groovy-mode setting
;;----------------------------------------------------------------------------
;; (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
;; (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-auto-mode 'groovy-mode "\\.groovy\\'")

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))



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
      eclimd-default-workspace "/home/leo/Program/MyEclipse 10backup"
      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

(add-auto-mode 'eclim-mode "\\.java\\'")

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)



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

(provide 'init-leo)
