(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(defun neofetch () (interactive) (async-shell-command "neofetch" "*Neofetch*"))
(setq inhibit-startup-screen t)
(neofetch)

(defun latex-init-settings ()
  (setq TeX-parse-self t)
  (add-to-list 'TeX-command-list
	       '("latexmk lualatex compile" "latexmk -lualatex %s" TeX-run-command nil t))
  (add-to-list 'TeX-command-list
	       '("latexmk lualatex preview" "latexmk -lualatex -pvc -view=none %s" TeX-run-command nil t))
  (add-to-list 'TeX-command-list
	       '("latexindent" "latexindent -w %s" TeX-run-command nil t))
  (setq TeX-view-program-selection '((output-pdf "Okular")))
  (setq cdlatex-math-modify-alist
	'((98 "\\mathbb" nil t nil nil)
	  (124 "\\abs*" nil t nil nil)
	  (123 "\\set*" nil t nil nil)
	  (102 "\\mathfrak" nil t nil nil)))
  (setq cdlatex-math-symbol-alist
	'((120 "\\chi" "\\otimes")
	  (62 "\\geq" "\\geqsim" "\\rangle")
	  (60 "\\leq" "\\lesssim" "\\langle")
	  (40 "\\left(")
	  (41 "\\right)")
	  (91 "\\left[")
	  (93 "\\right]"))))

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
  (require 'org-ref-ivy)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
 org-ref-insert-cite-function 'org-ref-cite-insert-ivy
 org-ref-insert-label-function 'org-ref-insert-label-link
 org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (require 'org-ref)
  (require 'org-noter)
  (setq org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "POSTPONED" "|" "DONE" "CANCELLED"))))

(defun org-hook () ()
       (visual-line-mode)
       (local-set-key (kbd "C-c ]") 'org-ref-insert-link-hydra)
       (org-cdlatex-mode))

(use-package org
  :defer t
  :config (org-init-settings))

(defun init-mail-settings () ()
  (setq
   mu4e-get-mail-command "offlineimap -q -o"
   mu4e-update-interval 30000))

(use-package mu4e
  :load-path  "/usr/share/emacs/site-lisp/mu4e"
  :init (init-mail-settings))

(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
(add-hook 'LaTeX-mode-hook 'latex-hook)
(add-hook 'org-mode-hook 'org-hook)

(defun file-manipulation-settings () (interactive)
       (progn
	 (use-package projectile)
	 (use-package magit)
	 (use-package dired-x)
	 (projectile-mode +1)
	 (setq dired-listing-switches "-ahl")
	 (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
	 (setq dired-guess-shell-alist-user
	       '(("\\.pdf$" "okular *")))))

(defun zotero-store () (interactive)
       (find-dired "~/Zotero/storage" "-name '*.pdf'"))

(defun smart-kill-word () (interactive)
  (forward-word)
  (kill-word -1))

(setq sentence-end-double-space nil)

(defun editing-settings () (interactive)
       (use-package counsel)
       (use-package crux)
       (ivy-mode +1)
       (counsel-mode +1)
       (counsel-projectile-mode +1)
       (setq bibtex-completion-bibliography '("~/zoterolib.bib"))
       (global-set-key (kbd "C-x s") 'swiper)
       (global-set-key (kbd "C-k") 'crux-smart-kill-line)
       (global-set-key (kbd "M-d") 'smart-kill-word))

(global-set-key (kbd "C-c v v") 'view-mode)
(global-set-key (kbd "C-c v f") 'follow-mode)
(unbind-key "C-z")
(defun machine-uptime () (interactive) (shell-command "uptime"))
(defun pacman-update () (interactive) (async-shell-command "sudo pacman -Syu"))
(defun get-weather () (interactive)
       (async-shell-command "curl -s 'https://wttr.in/chicago?0p'" "*wttr.in*" nil))
(global-set-key (kbd "C-z p") 'ping)
(global-set-key (kbd "C-z t") 'machine-uptime)
(global-set-key (kbd "C-z b") 'battery)
(global-set-key (kbd "C-z u") 'pacman-update)
(global-set-key (kbd "C-z w") 'get-weather)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x b") 'display-buffer)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "M-s m") 'counsel-imenu)
(global-set-key (kbd "M-s b") 'counsel-ibuffer)
(global-set-key (kbd "M-z") 'counsel-linux-app)

(defun exwm-settings ()
  (setq exwm-workspace-number 4)
  (add-hook 'exwm-update-class-hook
	(lambda ()
	  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
		      (string= "gimp" exwm-instance-name))
	    (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
	    (lambda ()
	  (when (or (not exwm-instance-name)
		    (string-prefix-p "sun-awt-X11-" exwm-instance-name)
		    (string= "gimp" exwm-instance-name))
	    (exwm-workspace-rename-buffer exwm-title))))
  (setq exwm-input-global-keys
	`((,(kbd "s-SPC") . (lambda (command)
			      (interactive (list (read-shell-command "$ ")))
			      (start-process-shell-command command nil command)))
	  (,(kbd "s-r") . exwm-reset)
	  (,(kbd "s-o") . exwm-workspace-switch)
	  (,(kbd "s-g") . (lambda () (start-process "slock" nil "slock")))
	  )
	)
  (exwm-enable)
  )
(use-package exwm-randr
  :config (progn
	    (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "eDP-1" 2 "HDMI-1" 3 "HDMI-1" 4 "DP-1" 5 "DP-1"))
	    (add-hook 'ewm-randr-screen-change-hook
		      (lambda ()
			(start-process-shell-command
			 "xrandr" nil "xrandr --output HDMI-1 --right-of eDP-1 --auto")))
	    (exwm-randr-enable)))
(use-package exwm
  :config (exwm-settings))

(defun xmonad-tree-navigator (tree)
  (if (windowp tree) tree
    (if (listp tree) (xmonad-tree-navigator (car (last tree)))
	(error "Encountered a non-list or non window argument"))))

(defun xmonad-tall (curr-win)
       (if (one-window-p) (split-window-right)
	 (progn
	   (select-window (xmonad-tree-navigator (car (window-tree))))
	   (split-window-below))))

(defun bsp-tree-navigator (tree)
  (if (windowp tree) tree
    (if (listp tree) (bsp-tree-navigator (car (last tree)))
      (error "Encountered a non-list or non-window argument"))))

(defun bspwm (curr-win)
       (let ((to-window (bsp-tree-navigator (car (window-tree)))))
	 (progn
	   (select-window to-window)
	   (if (window-combined-p to-window t)
	       (split-window-below)
	     (split-window-right)))))

(defun bspwm-vert (curr-win)
       (let ((to-window (bsp-tree-navigator (car (window-tree)))))
	 (progn
	   (select-window to-window)
	   (if (window-combined-p to-window t)
	       (split-window-right)
	     (split-window-below)))))

(setq layout-list '(split-window-sensibly xmonad-tall bspwm bspwm-vert))
(defun select-window-layout (symbol) (interactive "Slayout: ")
       (if (member symbol layout-list) (setq split-window-preferred-function symbol)
	 (error "Not a layout in layout-list")))
(defun current-window-layout () (interactive)
       (message split-window-preferred-function))

(defun runner () (interactive)
       (setq default-minibuffer-frame (make-frame
				       '((minibuffer . only) (title . "erunner") (left . 0.25) (top . 0.25) (height . 0.5) (width . 0.5))))
       (setq minibuffer-auto-raise t))

(use-package avy) 
(global-set-key (kbd "M-g g") 'avy-goto-char-2)
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "M-g f") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g e") 'avy-goto-end-of-line)

(defun add-menu-item (key command)
       (global-set-key (kbd (concat "C-; " key)) command))
(defun find-init-file () (interactive) (find-file "~/.emacs.d/index.org"))
(add-menu-item "m" 'mu4e)
(add-menu-item "i" 'find-init-file)
(add-menu-item "a" 'org-agenda)
(add-menu-item "p" 'proced)
(add-menu-item "b" 'ivy-bibtex)

(use-package elfeed
  :ensure t)
(global-set-key (kbd "C-; e") 'elfeed)
(setq elfeed-feeds
      '(("http://arxiv.org/rss/math.AP" preprint)
	("https://cvgmt.sns.it/papers/rss.xml" preprint)
	("http://arxiv.org/rss/cond-mat.soft" preprint)
	("https://planet.emacslife.com/atom.xml" emacs)
	("https://kbd.news/rss2.php" keyboard)
	("https://sachachua.com/blog/feed/" emacs)
	))

(defun wiki-search (search-term) (interactive "sSearch Wikipedia: ") (browse-url (concat "https://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" search-term)))

(use-package pdf-tools
  :config (pdf-tools-install))
