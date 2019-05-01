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

## Supported major modes
At the time of writing the list of supported major modes is:
 - `html-mode`
   - MathJax LaTeX fragments `$$...$$`, `\[...\]`, `$...$`, `\(...\)`, `\begin{...}...\end{...}`  are recognized in the body of the document
   - `&amp;`, `&gt;`, and `&lt;` are translated to `&`, `>`, and `<` in the LaTeX code
 - `eww-mode`
   - same features as `html-mode`
   - sets LaTeX file name on the base of the url (which may be a file name `file://...`)
 - `sx-question-mode`
   - recognizes the same LaTeX fragments as `html-mode`
 - `prog-mode` (formulas in doxygen comments)
   - LaTeX fragments `\f$...\f$`, `\f[...\f]`, `\f{...}...\f}`, and `\f{...}{...\f}` are recognized within comments of the source code
     (example for the last case: `\f{align*}{E=mc^2\f}`)
 - `trac-wiki-mode`
   - recognized LaTeX fragments: `^{{{\n#!latex\n...^}}}` (thereby `^` stands for beginning of line, and `\n` for a linebreak), `$...$`, `\(...\)`, `\[...\]`, and `\begin{...}...\end{...}`
 - `org-mode` (at the time of writing just a proof that cdlatex could be replaced by AUCTeX preview)
   - recognized LaTeX fragments. `$...$`, `\(...\)`, `\[...\]`, and `\begin{...}...\end{...}`
   - restriction: `org-html-with-latex` is set internally to `'dvipng` because only that variant works with `texfrag`
   - separate function `texfrag-org-header` for `texfrag-header-function` that ensures that org LaTeX header options are recognized
   
All major modes derived from the listed modes are automatically also supported.

Please, inspect the help of the variable `texfrag-setup-alist` for a complete list of supported modes.

## Known issues

- DONE texfrag-global-mode does not switch on texfrag-mode for eww
  #13 opened 2 minutes ago by TobiasZawada
- DONE sx: equation end with $$ is misinterpreted as new equation beginning
  #12 opened 7 hours ago by TobiasZawada
- DONE LaTeX does not accept the UTF8 Byte Order Mark.
  #11 opened 8 hours ago by TobiasZawada
- DONE Query whether LaTeX should be killed at opening text with `texfrag-global-mode` and `texfrag-preview-buffer-at-start` turned on.
  #10 opened a day ago by TobiasZawada
- DONE TeXfrag leaves LaTeX buffers and output buffers open.
  #9 opened 14 days ago by TobiasZawada 
