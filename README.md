.emacs.d
========

My Emacs 26+ (-nw) configuration, mainly for Python and Go programming.

I also try to maintain a `emacs25` branch for outdated distributions.

Dependencies
-----------
* `virtualenv`
* `pip`

Optional, but recommended:
* `aspell`
* `flake8`
* `shellcheck`

Install
-------
```
$ git clone git@github.com:ramnes/.emacs.d.git --recursive
$ .emacs.d/bin/compile
$ emacs --load ~/.emacs.d/init.el --batch -f "jedi:install-server"
```
