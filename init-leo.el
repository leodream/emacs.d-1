(require-package 'goto-last-change)
(require-package 'ecb)

;; Goto-the-last-change
(load-file (expand-file-name "~/.emacs.d/goto-last-change.el"))
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)



;; ECB Setting
(require 'ecb-autoloads)
(custom-set-variables
 '(ecb-source-path (quote ("/home/leo/Program" ("/home/leo/Program/TestProject" "yes"))))
 '(ecb-windows-width 0.25))



;; re-open file if it is read-only
;; TODO cannot use this feature in ido-find-file
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


(provide 'init-leo)
