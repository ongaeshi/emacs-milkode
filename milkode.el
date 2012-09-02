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
;; (global-set-key (kbd "M-j") 'milkode:search)

;;; Code:

(eval-when-compile (require 'cl))

;;; Public:

;; Notify function
;;;###autoload
(defun milkode:search ()
  (interactive)
  (let ((at-point (thing-at-point 'filename)))
    (if (milkode:is-directpath at-point)
        (milkode:jump at-point)
      (let ((input (read-string "gmilk: ")))
        (if (milkode:is-directpath input)
            (milkode:jump input)
          (milkode:grep input))))))
  
;;; Private:

(defun milkode:jump (path)
  (with-temp-buffer
      (call-process "gmilk" nil t nil path)
      (goto-char (point-min))
      (milkode:goto-line (thing-at-point 'filename))
      ))

(defun milkode:grep (path)
  (grep (concat "gmilk " path)))

(defun milkode:is-directpath (str)
  (string-match "^/.*:[0-9]+" str))

(defun milkode:goto-line (str)
  (let ((list (split-string str ":")))
    (find-file (nth 0 list))
    (goto-line (string-to-number (nth 1 list)))
  ))

(provide 'milkode)
;;; milkode.el ends here
