.emacs.d
========

My Emacs (-nw) configuration, mainly for Python programming. I happen to tinker
with other languages too.

Dependencies
-----------
* `virtualenv`
* `pip`

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
$ emacs --load ~/.emacs.d/init.el --batch -f "jedi:install-server"
```
