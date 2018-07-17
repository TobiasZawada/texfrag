<!-- This is https://github.com/TobiasZawada/texfrag/README.md -->
# Texfrag
Emacs package for previewing LaTeX fragments such as in doxygen comments.
Other use-cases are MathJax snippets in html pages and
LaTeX equations in [stackexchange questions](https://github.com/vermiculus/sx.el).

## Installation and activation
Install texfrag via Melpa by `<M-x>package-install<RET>texfrag<RET>`.

Switch the customization option `texfrag-global-mode` on to activate `texfrag-mode`
in all buffers with supported major modes.

If you want to activate texfrag only in a specific major mode add `texfrag-mode` to
its mode hook. This works out of the box if the major mode is derived from one of
the modes registered in `texfrag-setup-alist`.

## Usage
A buffer with activated texfrag has an additional sub-menu "TeX".
There you find menu-items for generating AUCTeX previews at point for
region and for the full document.
The usual key-sequences from AUCTeX such as `C-c C-p C-p` for _Toggle preview at point_
are also available.

### Examples
Single line equation:
To preveiw the formula `\vec{x}\cdot \mathbf{A} = \lambda \vec{x}` simply wrap it in `\f$ \f$`, so we would write `\f$  \mathbf{A} \cdot \vec{x} = \lambda \vec{x}\f$` and then run the preview command. For multi-line equations, we use $\f[ \f]$ instead.

## Supported major modes
At the time of writing the list of supported major modes is:
 - `html-mode`
 - `eww-mode`
 - `sx-question-mode`
 - `prog-mode` (formulas in doxygen comments)
 - `trac-wiki-mode`
 - `org-mode` (at the time of writing just a proof that cdlatex could be replaced by AUCTeX preview)
 
All major modes derived from the listed modes are automatically also supported.

Please, inspect the help of the variable `texfrag-setup-alist` for a complete list of supported modes.
