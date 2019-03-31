This project supports language-independent
[literate programming](https://en.wikipedia.org/wiki/Literate_programming).
It's a reimplementation of
the [nuweb project](https://sourceforge.net/projects/nuweb/) in
Python.

So far, the parts implemented (you may like to refer to
the [nuweb user guide](http://nuweb.sourceforge.net/nuwebdoc.pdf))
are:

* files (`@o` and `@O`, but no flags)
* fragments (`@d` and `@D`, but no flags)
* scraps (delimited by `@{` `@}` only)
* user-defined identifiers
* old-style fragment parameters
* indices `@f`, `@m` and `@u` (I've laid `@u` out a little differently)
* `@%` (anywhere in the document, not just in scraps)
* `@#` (put code line at left margin)
* `@@` handling (this one was tricky, and I may not have caught all the cases)
* switch `-r` (generate hyperlinks), aliased `--hyperlinks`.

Includes Emacs support in `poly-nuweb.el`, a minor mode based
on [https://github.com/polymode/polymode](polymode). See its header
for installation notes.
