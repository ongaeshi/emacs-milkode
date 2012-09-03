# README

## Name

milkode

## Description

Command line search and direct jump with Milkode.
Milkode(http://milkode.ongaeshi.me) of the installation is required. 

Feature

1. Search `(milkode:search)`. Jump to row `C-c C-c`.
2. When you search for direct pass `/path/to/dir:15` jump directly to the specified row.
3. Move the cursor to direct pass on a text file, `(milkode:search)` can jump

Displayed direct pass to click the line number in the 'milk web' (ex. http://kodeworld.ongaeshi.me/)

## Authors

* ongaeshi <ongaeshi0621@gmail.com>

## License

MIT

## Install

```emacs-lisp
(auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/milkode.el")
```

## .emacs.d/init.el

```emacs-lisp
(require 'milkode)
;; Shortcut setting (Your favorite things)
(global-set-key (kbd "M-g") 'milkode:search)
```
