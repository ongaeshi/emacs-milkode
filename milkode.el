;;; milkode.el --- Run the shell command asynchronously that you specified when you save the file. 

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: milkode, search, keyword, tag, jump, direct
;; Version: 0.1
;; Package-Requires:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Milkode (http://milkode.ongaeshi.me) support.

;; Feature
;;   1. Jump to direct-path(/path/to/dir:15)
;;   2. Search keyword

;; URL
;;   https://github.com/ongaeshi/emacs-milkode

;;; Install:
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/emacs-milkode.el")

;;; Initlial Setting:

;; (require 'emacs-milkode)
;; 
;; ;; Shortcut setting
;; (global-set-key (kbd "C-x C-m") 'milkode:jump)    ; Jump to direct-path
;; (global-set-key (kbd "C-x C-,") 'milkode:search)  ; Search keyword

;;; Code:

(eval-when-compile (require 'cl))

;;; Public:

;; Notify function
;;;###autoload
(defun milkode:jump (path)
  (interactive "MPath: ")
  (message (shell-command-to-string (concat "gmilk " path)))
  )

;;; Private:

(provide 'milkode)
;;; milkode.el ends here
