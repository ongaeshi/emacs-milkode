;;; milkode.el --- Command line search and direct jump with Milkode

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: milkode, search, grep, jump, keyword
;; Version: 0.2
;; Package-Requires:

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Command line search and direct jump with Milkode.
;; Milkode(http://milkode.ongaeshi.me) of the installation is required. 

;; Feature
;;   1. Search (milkode:search). Jump to row C-c C-c.
;;   2. When you search for direct pass ('/path/to/dir:15') jump directly to the specified row.
;;   3. Move the cursor to direct pass on a text file, (milkode:search) can jump
;;
;; Displayed direct pass to click the line number in the 'milk web' (ex. http://kodeworld.ongaeshi.me/)

;; URL
;;   https://github.com/ongaeshi/emacs-milkode

;;; Install:
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/milkode.el")

;;; Initlial Setting:
;; (require 'milkode)
;; 
;; ;; Shortcut setting (Your favorite things)
;; (global-set-key (kbd "M-g") 'milkode:search)

;;; Code:

;;; Variables:
(setq milkode:cygwin-p (eq system-type 'cygwin)
      milkode:nt-p (eq system-type 'windows-nt)
      milkode:meadow-p (featurep 'meadow)
      milkode:windows-p (or milkode:cygwin-p milkode:nt-p milkode:meadow-p)) ; Windows?

(defvar milkode:history nil
  "History of gmilk commands.")

(defvar gmilk-command
  (if milkode:windows-p "gmilk.bat" "gmilk")
  "gmilk command.")

(defvar milk-command
  (if milkode:windows-p "milk.bat" "milk")
  "milk command.")

;;; Public:

;;;###autoload
(defun milkode:search ()
  (interactive)
  (let ((input (read-string "gmilk: " (thing-at-point 'symbol) 'milkode:history)))
    (milkode:grep input)))

;;;###autoload
(defun milkode:jump ()
  (interactive)
  (let ((at-point (thing-at-point 'filename)))
    (if (milkode:is-directpath at-point)
        (progn
          (setq milkode:history (cons at-point milkode:history)) 
          (milkode:jump-directpath at-point)) 
      (let ((input (read-string "milk jump: " (thing-at-point 'symbol) 'milkode:history)))
        (if (milkode:is-directpath input)
            (milkode:jump-directpath input)
          (message "Not direct path."))))))

;;;###autoload
(defun milkode:display-history ()
  (interactive)
  (with-current-buffer (get-buffer-create "*milkode*")
    (delete-region (point-min) (point-max))
    (insert (mapconcat #'identity milkode:history "\n"))
    (pop-to-buffer "*milkode*")))

;;; Private:

(defun milkode:jump-directpath (path)
  (with-temp-buffer
    (message (format "Jump to %s ..." path))
    (call-process gmilk-command nil t nil path)
    (goto-char (point-min))
    (milkode:goto-line (thing-at-point 'filename))
    ))

(defun milkode:grep (path)
  (grep (concat gmilk-command " " path)))

(defun milkode:is-directpath (str)
  (unless (null str)
    (string-match "^/.*:[0-9]+" str)))

(defun milkode:is-windows-abs (str)
  (string-match "^[a-zA-Z]:" str))

(defun milkode:goto-line (str)
  (let ((list (split-string str ":")))
    (if (milkode:is-windows-abs str)
        (progn 
          (find-file (concat (nth 0 list) ":" (nth 1 list)))
          (goto-line (string-to-number (nth 2 list))))
      (find-file (nth 0 list))
      (goto-line (string-to-number (nth 1 list))))))

;; 

(provide 'milkode)
;;; milkode.el ends here