(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)
(load "~/.emacs.d/more-settings.el")
(editing-settings)
(file-manipulation-settings)
(init-mail-settings)
