(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defun latex-init-settings ()
  (add-hook 'LaTeX-mode-hook '(progn (turn-on-cdlatex-mode) (turn-on-reftex)))
  (setq cdlatex-math-modify-alist
	'((98 "\\mathbb" nil t nil nil)))
  (setq cdlatex-math-symbol-alist
	'((120 "\\chi" "\\otimes"))))

(use-package auctex
  :defer t
  :config (latex-init-settings))

(defun latex-in-org-settings ()
  (progn
    (require 'ox-bibtex)
    (setq org-format-latex-options
	  '(:foreground default
			:background: default
			:scale 2.0
			:html-foreground "Black"
			:html-background "Transparent"
			:html-scale 1.0
			:matchers ("begin" "$1" "$")))
    ))

(defun org-indent-paragraph () (interactive)
       (org-backward-paragraph)
       (push-mark)
       (org-forward-paragraph)
       (org-indent-region
	(mark) (point)))

(defun org-init-settings ()
  (add-hook 'Org-mode-hook '(progn (visual-line-mode) (org-cdlatex-mode)))
  (latex-in-org-settings)
  (setq org-agenda-start-on-weekday 0)
  (global-set-key (kbd "C-<tab>") 'org-indent-paragraph)
  (setq org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "POSTPONED" "|" "DONE" "CANCELLED"))))

(use-package org
  :defer t
  :config (org-init-settings))

(defun file-manipulation-settings () (interactive)
       (progn
	 (use-package projectile)
	 (use-package magit)
	 (use-package dired-x)
	 (projectile-mode +1)
	 (setq dired-listing-switches "-ahl")
	 (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
	 (setq dired-guess-shell-alist-user
	       '(("\\.pdf\\" "zathura")))))

(defun init-mail-settings () (interactive)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (use-package mu4e)
  (setq
   message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   user-mail-address "samueltwallace@gmail.com"
   user-full-name "Sam Wallace"
   smtpmail-local-domain "gmail.com")
  (setq
   mu4e-get-mail-command "offlineimap -q -o"
   mu4e-update-interval 3000))

(defun smart-kill-word () (interactive)
  (backward-word)
  (kill-word 1))

(defun editing-settings () (interactive)
       (use-package counsel)
       (use-package crux)
       (ivy-mode +1)
       (global-set-key (kbd "C-x s") 'swiper)
       (global-set-key (kbd "C-k") 'crux-smart-kill-line)
       (global-set-key (kbd "M-d") 'smart-kill-word))

(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)

(defun format-for-nyxt-eval (list)  (shell-quote-argument (format "%S" list))) ;; prepare lisp code to be passed to the shell
(defun eval-in-nyxt (s-exps)  (call-process "nyxt" nil nil nil (concat "--remote --eval " (format-for-nyxt-eval s-exps))))

(defun set-in-nyxt (variable elisp) (eval-in-nyxt `(setq ,variable (list ,@elisp))))
(defun eval-region-in-nyxt (start end) (interactive "r") (eval-in-nyxt (read (buffer-substring start end))))

(defun get-nyxt-buffers () (eval-in-nyxt
			    '(eval-in-emacs
			      `(setq nyxt-buffer-list
				     (list ,@(mapcar #'title (buffer-list)))))))
(defun search-in-nyxt (search-term) (interactive "sSeach in Nyxt:") (eval-in-nyxt
								     `(buffer-load (make-instance 'new-url-query
												  :query ,search-term
												  :engine (first (last (search-engines (current-buffer))))))))
