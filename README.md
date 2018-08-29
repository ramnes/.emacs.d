.emacs.d
========

My Emacs 26+ (-nw) configuration, mainly for Python programming.

Dependencies
-----------
* `virtualenv`
* `pip`

Install
-------
```
pip install --user epc jedi
git clone git@github.com:ramnes/.emacs.d.git --recursive
.emacs.d/bin/compile
emacs --load ~/.emacs.d/init.el --batch -f "jedi:install-server"
```
