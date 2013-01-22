;;; anything-milkode.el --- Command line search with Milkode

;; Copyright (C) 2013 ongaeshi

;; Author: ongaeshi
;; Keywords: milkode, anything, search, grep, jump, keyword
;; Version: 0.1
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

;;; Install:
;;   (auto-install-from-url "http://www.emacswiki.org/cgi-bin/wiki/download/anything-grep.el")
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/milkode.el")
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/anything-milkode.el")

;;; Initlial Setting:
;; (require 'anything-milkode)
;;
;; ;; Use anything-grep single line mode
;; (setq anything-grep-multiline nil)                
;; 
;; ;; Shortcut setting
;; (global-set-key (kbd "M-g") 'anything-milkode)
;;
;; ;; popwin setting (Optional)
;; (push '("*grep*" :noselect t) popwin:special-display-config)
;; (push '("*anything milkode*") popwin:special-display-config)

;;; Code:

;;; Variables:

;;; Public:
(require 'anything-grep)
(require 'milkode)

;;;###autoload
(defun anything-milkode ()
  "TODO: comment"
  (interactive)
  (let ((at-point (thing-at-point 'filename)))
    (if (milkode:is-directpath at-point)
        (progn
          (setq milkode:history (cons at-point milkode:history)) 
          (milkode:jump-directpath at-point))
      (let* ((input   (read-string "anything-milkode: " (thing-at-point 'symbol) 'milkode:history))
         (command (concat gmilk-command " " input))
         (pwd     default-directory))
        (if (milkode:is-directpath input)
            (milkode:jump-directpath input)
          (anything-grep-base (list (agrep-source (agrep-preprocess-command command) pwd))
                              (format "*anything milkode*" command (abbreviate-file-name pwd))))))))

(defun anything-c-sources-milkode-files (pwd)
  (loop for elt in
        '(("milk files (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (unless (anything-candidate-buffer)
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format (concat milk-command " files -r")
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))

;;;###autoload
(defun anything-milkode-files ()
  (interactive)
  (let* ((pwd default-directory)
         (sources (anything-c-sources-milkode-files pwd)))
    (anything-other-buffer sources
                           (format "*anything milkode files*"))))

;; 

(provide 'anything-milkode)
;;; anything-milkode.el ends here
