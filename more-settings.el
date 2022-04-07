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
	  (,(kbd "s-M-o") . exwm-workspace-switch-to-buffer)
	  (,(kbd "s-o") . exwm-workspace-switch)
	  (,(kbd "s-g") . (lambda () ((interactive (start-process "slock" nil "slock")))))
	  )
	)
  (exwm-enable)
  )
(use-package exwm-randr
  :config (progn
	    (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "eDP-1" 2 "HDMI-1" 3 "HDMI-1"))
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
	   (if (window-combined-p to-window)
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

(repeat-mode)

(defvar make-window-repeat-map
  (let ((map (make-sparse-keymap)))
	(define-key map "2" 'split-window-below)
	(define-key map "3" 'split-window-right)
	(define-key map "0" 'delete-window)
	(define-key map "=" 'balance-windows)
	(define-key map "b" 'switch-to-buffer) map)
    "making, breaking, and switching window. for use in repeat-mode")

(put 'split-window-below 'repeat-map 'make-window-repeat-map)
(put 'split-window-right 'repeat-map 'make-window-repeat-map)
(put 'delete-window 'repeat-map 'make-window-repeat-map)
(put 'balance-windows 'repeat-map 'make-window-repeat-map)
(put 'switch-to-buffer 'repeat-map 'make-window-repeat-map)

(defvar move-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "a" 'beginning-of-line-text)
    (define-key map "e" 'end-of-line)
    (define-key map "f" 'forward-word)
    (define-key map "b" 'backward-word)
    (define-key map (kbd "SPC") 'set-mark-command)
    (define-key map "w" 'kill-region)
    (define-key map "y" 'yank) map)
  "movement and editing commands. tiny vim normal mode.")

(put 'next-line 'repeat-map 'move-map)
(put 'previous-line 'repeat-map 'move-map)
(put 'beginning-of-line-text 'repeat-map 'move-map)
(put 'end-of-line 'repeat-map 'move-map)
(put 'set-mark-command 'repeat-map 'move-map)
(put 'kill-region 'repeat-map 'move-map)
(put 'yank 'repeat-map 'move-map)
(put 'forward-word 'repeat-map 'move-map)
(put 'backward-word 'repeat-map 'move-map)

(defvar cumulative-object-ring nil "An object to be repeatedly acted on by stick-cmd")

(defun cumulative-push-object (lisp-object) (interactive "XLisp Object:") (push lisp-object cumulative-object-ring))

(defvar cumulative-action-ring nil  "A list of symbols to act on cumulative-object")

(defun cumulative-push-action (command) (interactive "CCumulative Action:") (push command cumulative-actions))

(defun cumulative-exec ()
  (interactive)
  (dolist (cmd cumulative-actions)
    (dolist (cumulative-object cumulative-object-ring)
    (eval `(,cmd ,cumulative-object))))
  (setq cumulative-object nil)
  (setq cumulative-actions nil))

(defun cumulative-push-buffer (buf) (interactive "bCumulative Buffer:") (push buf cumulative-object-ring))
(defun cumulative-push-file (fil) (interactive "FCumulative File:") (push buf cumulative-object-ring))
(defun cumulative-push-region (beg end) (interactive "r") (push (list beg end) cumulative-object-ring))

(defun cumulative-push-save-and-kill () (interactive) (cumulative-push-action 'save-buffer) (cumulative-push-action 'kill-buffer))
(defun cumulative-push-find-other-window () (interactive) (cumulative-push-action 'find-file-other-window))
(defun cumulative-push-kill () (interactive) (cumulative-push-action 'kill-region))

(defun cumulative-clear-actions () (interactive) (setq cumulative-action-ring nil))
(defun cumulative-clear-objects () (interactive) (setq cumulative-object-ring nil))

(defvar cumulative-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-c o") 'cumulative-push-object)
    (define-key map (kbd "M-c a") 'cumulative-push-action)
    (define-key map (kbd "M-c x") 'cumulative-exec)
    (define-key map (kbd "M-c b") 'cumulative-push-buffer)
    (define-key map (kbd "M-c f") 'cumulative-push-file)
    (define-key map (kbd "M-c r") 'cumulative-push-region)
    (define-key map (kbd "M-c s") 'cumulative-push-save-and-kill)
    (define-key map (kbd "M-c 4 f") 'cumulative-push-find-other-window)
    (define-key map (kbd "M-c k") 'cumulative-push-kill) map)
  "keymap for some common cumulative commands")

(define-minor-mode cumulative-mode
  "collect functions and targets for cumulative actions that can be executed."
  :global t
  :init-value nil
  :lighter " cum"
  :keymap cumulative-map)

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
