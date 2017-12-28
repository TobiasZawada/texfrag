<!-- This is https://github.com/TobiasZawada/TeXfrag/README.md -->
# TeXfrag
Emacs package for previewing LaTeX fragments such as in doxygen comments.
Other use-cases are MathJax snippets in html pages and
LaTeX equations in [stackexchange questions](https://github.com/vermiculus/sx.el).

## Installation and activation
Install TeXfrag via Melpa by `<M-x>package-install<RET>TeXfrag<RET>`.

Switch the customization option `TeXfrag-global-mode` on to activate `TeXfrag-mode`
in all buffers with supported major modes.

If you want to activate TeXfrag only in a specific major mode add `TeXfrag-mode` to
its mode hook. This works out of the box if the major mode is derived from one of
the modes registered in `TeXfrag-setup-alist`.

## Usage
A buffer with activated TeXfrag has an additional sub-menu "TeX".
There you find menu-items for generating AUCTeX previews at point for
region and for the full document.
The usual key-sequences from AUCTeX such as `C-c C-p C-p` for _Toggle preview at point_
are also available.

## Supported major modes
At the time of writing the list of supported major modes is:
 - `html-mode`
 - `eww-mode`
 - `sx-question-mode`
 - `prog-mode` (formulas in doxygen comments)
 - `trac-wiki-mode`
 - `org-mode` (at the time of writing just a proof that cdlatex could be replaced by AUCTeX preview)
 
All major modes derived from the listed modes are automatically also supported.

Please, inspect the help of the variable `TeXfrag-setup-alist` for a complete list of supported modes.
