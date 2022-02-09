(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)
(use-package hydra)

(defun neofetch () (interactive) (async-shell-command "neofetch" "*Neofetch*"))
(setq inhibit-startup-screen t)
(neofetch)

(defun latex-init-settings ()
  (defhydra hydra-latex-jump (latex-mode-map "C-x C-j")
    ("i" latex-next-item "next \\item")
    ("I" latex-last-item "previous \\item")
    ("s" latex-next-section "next \\.*section")
    ("S" latex-last-section "previous \\.*section"))
  (setq TeX-parse-self t)
  (setq cdlatex-math-modify-alist
	'((98 "\\mathbb" nil t nil nil)))
  (setq cdlatex-math-symbol-alist
	'((120 "\\chi" "\\otimes")
	  (62 "\\geq" "\\geqsim" "\\rangle")
	  (60 "\\leq" "\\lesssim" "\\langle")
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
       (global-set-key (kbd "M-d") 'smart-kill-word)
       (defhydra hydra-editing (global-map "M-.")
	 ("h" backward-char "back char")
	 ("j" forward-line "forward line")
	 ("k" previous-line "prev line")
	 ("l" forward-char "forward char")
	 ("o" open-line "open line")
	 ("RET" newline "newline")
	 ("/" swiper "swiper")
	 ("f" avy-goto-char-in-line "jump char")
	 ("w" forward-word "forward word")
	 ("b" backward-word "back word")
	 ("J" join-line "join line")
	 ("K" kill-region "kill region")
	 ("r" kill-rectangle "kill rect")
	 ("R" string-rectangle "replace rect")
	 ("SPC" set-mark-command "set mark")
       ))

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

(setq layout-list '(split-window-sensibly xmonad-tall bspwm))
(defun select-window-layout (symbol) (interactive "Slayout: ")
       (if (member symbol layout-list) (setq split-window-preferred-function symbol)
	 (error "Not a layout in layout-list")))
(defun current-window-layout () (interactive)
       (message split-window-preferred-function))

(use-package avy)
(global-set-key (kbd "M-g g") 'avy-goto-char-2)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "M-g k") 'avy-kill-region)
(global-set-key (kbd "M-g t") 'avy-move-region)
(global-set-key (kbd "M-g w") 'avy-copy-region)

(defun add-menu-item (key command)
       (global-set-key (kbd (concat "C-; " key)) command))
(defun find-init-file () (interactive) (find-file "~/.emacs.d/index.org"))
(add-menu-item "m" 'mu4e)
(add-menu-item "i" 'find-init-file)
(add-menu-item "a" 'org-agenda)
(add-menu-item "s" 'search-in-nyxt)
(add-menu-item "p" 'proced)
