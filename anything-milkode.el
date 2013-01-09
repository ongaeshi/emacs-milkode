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
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/anything-milkode.el")

;;; Initlial Setting:
;; (require 'anything-milkode)

;;; Code:

;;; Variables:

;;; Public:
(require 'anything-grep)
(require 'milkode)

;;;###autoload
(defun anything-milkode (command)
  "TODO: comment"
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer "Run grep (like this): "
				   (if current-prefix-arg
				       default grep-command)
				   nil nil 'grep-history
				   (if current-prefix-arg nil default))
             ))))
  (anything-grep-base (list (agrep-source (agrep-preprocess-command command) default-directory))
                      (format "*anything milkode*" command (abbreviate-file-name default-directory))))

;; 

(provide 'anything-milkode)
;;; anything-milkode.el ends here