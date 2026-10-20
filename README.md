<!--*- markdown -*-->

ob-maple.el an Org Babel mode for Maple integration
===================================================

**Version 0.1**

This package contains Org Babel [1] functions for in-line Maple [2] evaluation in Org Babel notebooks.

It is very much a work in progress compared to the very mature maplev package on which this is built.

Requirements
------------
The maplev package [3] is required to be installed
and configured for the maple integration to work.



Installation and configuration
------------------------------

**Using use-package:vc**

```
(use-package ob-maple
  :vc (:url "https://github.com/JoeRiel/ob-maple" :branch "main")
  :ensure t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(maple . t))
  (add-to-list 'org-src-lang-modes '("maple" . maplev))
  (add-to-list 'org-babel-tangle-lang-exts '("maple" . "mpl"))

  ;; Bindings which might be usefull
  ;;(keymap-set org-babel-map "S" #'org-babel-maple-kill-session)
  ;;(keymap-set org-babel-map "K" #'org-babel-maple-kill-all-sessions)
  )
```


**Manual installation**

Copy the `ob-maple.el` file to wherever you like and add it to `load-path`:

Configure the binding to org mode
```
(with-eval-after-load 'org
   (add-to-list 'org-babel-load-languages '(maple . t))
   (add-to-list 'org-src-lang-modes '("maple" . maplev))
   (add-to-list 'org-babel-tangle-lang-exts '("maple" . "mpl"))
   (require 'ob-maple))

```


Documentation
-------------
Session and non-session evaluation is allowed, graphics and LaTeX output is supported.

See: [doc/demo.org](doc/demo.org) for a demonstration of most features.

Resources
---------

[1] https://orgmode.org/worg/org-contrib/babel/index.html
[2] https://maplesoft.com/
[3] https://github/JoeRiel/maplev/
