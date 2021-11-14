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

(defun latex-last-item () (interactive)
       (isearch-backward "\\item"))
(defun latex-last-section () (interactive)
       (isearch-backward "\\.*section"))
(defun latex-next-item () (interactive)
       (isearch "\\item"))
(defun latex-next-section () (interactive)
       (isearch "\\.*section"))

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

(defhydra hydra-org-commands (org-mode-map "C-x h")
("<Up>" 'org-previous-visible-heading "prev heading")
("<Down>" 'org-next-visible-heading "next heading")
("<tab>" 'org-indent-paragraph))

(use-package org
  :defer t
  :config (org-init-settings))

(defun init-mail-settings () ()
  (setq
   mu4e-get-mail-command "offlineimap -q -o"
   mu4e-update-interval 3000))

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
	 (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
	 (setq dired-guess-shell-alist-user
	       '(("\\.pdf$" "zathura *")))))

(defun zotero-store () (interactive)
       (find-dired "~/Zotero/storage" "-name '*.pdf'"))

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

(global-set-key (kbd "C-x p v") 'view-mode)
(global-set-key (kbd "C-x p f") 'follow-mode)

(require 'windmove)
(defhydra hydra-window-manip (global-map "C-x w")
  ("h" shrink-window-horizontally "shrink horizontally")
  ("l" enlarge-window-horizontally "grow horizontally")
  ("j" enlarge-window "grow vertically")
  ("0" delete-window "delete window")
  ("1" delete-other-windows "fullscreen this one")
  ("2" split-window-below "split below")
  ("3" split-window-right "split right")
  ("H" windmove-left "move left")
  ("J" windmove-down "move down")
  ("K" windmove-up "move up")
  ("=" balance-windows "equal sizing")
  ("L" windmove-right "move right")
  ("<tab>" other-window "cycle-move")
  ("b" display-buffer "select buffer")
  ("c" clone-indirect-buffer-other-window "clone buffer")
  )

(defun xmonad-tree-navigator (tree)
  (if (windowp tree) tree
    (if (listp tree) (xmonad-tree-navigator (car (last tree)))
	(error "Encountered a non-list or non window argument"))))

(defun xmonad-tall (curr-win)
       (if (one-window-p) (split-window-right)
	 (progn
	   (select-window (xmonad-tree-navigator (car (window-tree))))
	   (split-window-below)
	   (balance-windows))))

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

(defun add-menu-item (key command)
       (global-set-key (kbd (concat "C-; " key)) command))
(defun find-init-file () (interactive) (find-file "~/.emacs.d/index.org"))
(add-menu-item "m" 'mu4e)
(add-menu-item "i" 'find-init-file)
(add-menu-item "a" 'org-agenda)
(add-menu-item "s" 'search-in-nyxt)

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

(setq stumpish-path "~/.stumpwm.d/modules/util/stumpish/stumpish")
(defun eval-in-stumpwm (s-exps) (call-process stumpish-path nil nil nil (format "eval %S" s-exps)))
(defun eval-region-in-stumpwm (start end) (interactive "r") (eval-in-stumpwm (read (buffer-substring start end))))

;; write some functions!
