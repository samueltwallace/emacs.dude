(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defun add-menu-item (key command)
       (global-set-key (kbd (concat "C-; " key)) command))
(defun find-init-file () (interactive) (find-file "~/.emacs.d/index.org"))
(add-menu-item "m" 'mu4e)
(add-menu-item "i" 'find-init-file)
(add-menu-item "a" 'org-agenda)
(add-menu-item "s" 'search-in-nyxt)

(defun latex-init-settings ()
  (setq TeX-parse-self t)
  (setq cdlatex-math-modify-alist
	'((98 "\\mathbb" nil t nil nil)))
  (setq cdlatex-math-symbol-alist
	'((120 "\\chi" "\\otimes")
	  (62 "\\geq" "\\geqsim" "\\langle")
	  (60 "\\leq" "\\lesssim" "\\rangle")
	  (40 "\\left(")
	  (41 "\\right)")
	  (91 "\\left[")
	  (92 "\\right]"))))

(defun latex-hook ()
  (cdlatex-mode 1)
  (reftex-mode 1)
  (prettify-symbols-mode 1))

(use-package latex
  :defer t
  :ensure auctex
  :config (latex-init-settings))

(defun latex-in-org-settings ()
  (progn
    (require 'ox-bibtex)
    (plist-put org-format-latex-options :scale 2.0)
    ))

(defun org-indent-paragraph () (interactive)
       (org-backward-paragraph)
       (push-mark)
       (org-forward-paragraph)
       (org-indent-region
	(mark) (point)))

(defun org-init-settings ()
  (latex-in-org-settings)
  (setq org-agenda-start-on-weekday 0)
  (setq org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "POSTPONED" "|" "DONE" "CANCELLED"))))

(defun org-hook () ()
       (visual-line-mode)
       (org-cdlatex-mode)
       (local-set-key (kbd "C-<tab>") 'org-indent-paragraph)
       (local-set-key (kbd "<C-Up>") 'org-previous-visible-heading)
       (local-set-key (kbd "<C-Down>") 'org-next-visible-heading)
       )

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
	       '(("\\.pdf\\'" "zathura")))))

(defun init-mail-settings () ()
  (setq
   mu4e-get-mail-command "offlineimap -q -o"
   mu4e-update-interval 3000))

(use-package mu4e
  :load-path  "/usr/share/emacs/site-lisp/mu4e"
  :init (init-mail-settings))

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
(add-hook 'LaTeX-mode-hook 'latex-hook)
(add-hook 'org-mode-hook 'org-hook)

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
