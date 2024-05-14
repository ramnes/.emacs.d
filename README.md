.emacs.d
========

My Emacs (-nw) configuration, mainly for Python, TS and Go
programming. I happen to tinker with other languages too.

Dependencies
-----------
* `virtualenv`
* `pip`

Language servers (LSP yay):
* `python-lsp-server`
* `typescript-language-server` + `typescript` (`tsserver`)
* `gopls`

Optional, but recommended:
* `aspell`
* `flake8`
* `shellcheck`
* `xsel`

Install
-------
```
$ git clone git@github.com:ramnes/.emacs.d.git --recursive
$ .emacs.d/bin/compile
```
