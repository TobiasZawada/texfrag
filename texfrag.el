;;; texfrag.el --- preview LaTeX fragments in alien major modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: tex, languages, wp
;; URL: https://github.com/TobiasZawada/texfrag
;; Version: 0.1
;; Package-Requires: ((emacs "25") (auctex "11.90.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable AUCTeX-preview in non-LaTeX modes.
;; Strategy:
;; - collect all header contents and latex fragments from the source buffer
;; - prepare a LaTeX document from this information
;; - let preview do its stuff in the LaTeX document
;; - move the preview overlays from the LaTeX document to the source buffer
;;
;; Install texfrag via Melpa by M-x package-install texfrag.
;;
;; If texfrag is activated for some buffer the image overlays for LaTeX fragments
;; such as equations known from AUCTeX preview can be generated
;; with the commands from the TeX menu (e.g. "Generate previews for document").
;;
;; The major mode of the source buffer should have a
;; texfrag setup function registered in `texfrag-setup-alist'.
;; Thereby, it is sufficient if the major mode is derived
;; from one already registered in `texfrag-setup-alist'.
;;
;; The default prefix-key sequence for texfrag-mode is the same as for preview-mode, i.e., C-c C-p.
;; You can change the default prefix-key sequence by customizing `texfrag-prefix'.
;; If you want to modify the prefix key sequence just for one major mode use
;; `texfrag-set-prefix' in the major mode hook before you run texfrag-mode.
;;
;; You can adapt the LaTeX-header to your needs by buffer-locally setting
;; the variable `texfrag-header-function' to a function without arguments
;; that returns the LaTeX header as a string.  Inspect the definition of
;; `texfrag-header-default' as an example.
;;
;; There are three ways to add TeXfrag support for your new major mode.
;;
;;  1. Derive your major mode from one of the already supported major modes
;;     (see doc of variable `texfrag-setup-alist').
;;     You do not need to do anything beyond that if your major mode does not
;;     change the marks for LaTeX equations (e.g., "\f$" for LaTeX equations
;;     in doxygen comments for `prog-mode').
;;
;;  2. Add a setup function to `texfrag-setup-alist' (see the doc for that variable).
;;     The minor mode function `texfrag-mode' calls that setup function
;;     if it detects that your major mode out of all major modes
;;     registered in `texfrag-setup-alist' has the closest relationship
;;     to the major mode of the current buffer.
;;     The setup function should adapt the values of the buffer local special variables
;;     of texfrag to the needs of the major mode.
;;     In many cases it is sufficient to set `texfrag-comments-only' to nil
;;     and `texfrag-frag-alist' to the equation syntax appropriate for the major mode.
;;
;;  3. Sometimes it is necessary to call `texfrag-mode' without corresponding
;;     setup function.  For an instance editing annotations with `pdf-annot-edit-contents-mouse'
;;     gives you a buffer in `text-mode' and the minor mode `pdf-annot-edit-contents-minor-mode'
;;     determines the equation syntax.
;;     One should propably not setup such general major modes like `text-mode' for texfrag.  Thus, it is
;;     better to call `TeXfrag-mode' in the hook of `pdf-annot-edit-contents-minor-mode'.
;;     Note, that the initial contents is not yet inserted when `pdf-annot-edit-contents-minor-mode' becomes active.
;;     Therefore, one needs to call `texfrag-region' in an advice of `pdf-annot-edit-contents-noselect'
;;     to render formulas in the initial contents.
;;     Further note, that there is already a working version for equation preview in pdf-tools annotations.
;;     Pityingly, that version is not ready for public release.
;;
;; The easiest way to adapt the LaTeX fragment syntax of some major mode
;; is to set `texfrag-frag-alist' in the mode hook of that major mode.
;; For `org-mode' the function `texfrag-org'
;; can be used as minimal implementation of such a hook function.
;; Note that this function only handles the most primitive
;; syntax for LaTeX fragments in org-mode buffers, i.e., $...$ and \[\].
;;
;; For more complicated cases you can install your own
;; parsers of LaTeX fragments in the variable
;; `texfrag-next-frag-function' (see the documentation of that variable).
;;

;;; Requirements:
;; - depends on Emacs "25" (because of when-let)
;; - requires AUCTeX with preview.el.

;;; Code:

(defgroup texfrag nil "Preview LaTeX fragments in buffers with non-LaTeX major modes."
  :group 'preview)

(defcustom texfrag-preview-buffer-at-start nil
  "Preview buffer at start of command `texfrag-mode' when non-nil."
  :group 'texfrag
  :type 'boolean)

(defcustom texfrag-setup-alist
  '((texfrag-html html-mode)
    (texfrag-eww eww-mode)
    (texfrag-sx sx-question-mode)
    (texfrag-prog prog-mode)
    (texfrag-trac-wiki trac-wiki-mode)
    (texfrag-org org-mode))
  "Alist that maps texfrag setup functions to lists of major modes.
Each element is a `cons'.
The `car' of the cons is the symbol of the texfrag setup function
and the `cdr' of the cons is the list of major modes supported by that
setup function."
  :group 'texfrag
  :type '(repeat (cons :tag "Pair of texfrag setup function and list of corresponding major modes"
		       (symbol :tag "Symbol of texfrag setup function")
		       (repeat :tag "Major modes" (symbol)))))

(defcustom texfrag-header-default
  "\\documentclass{article}
\\usepackage{amsmath,amsfonts}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\begin{document}"
  "LaTeX header inserted by the function `texfrag-header-default' into the LaTeX buffer."
  :group 'texfrag
  :type 'string)

(defcustom texfrag-subdir "texfrag"
  "Name of the sub-directory for the preview data."
  :group 'texfrag
  :type 'string)

(defcustom texfrag-prefix (kbd "C-c C-p")
  "Prefix for texfrag keys.
Defaults to the prefix of preview-mode.
If there is a collision with the major mode you can change this prefix in the major mode's hook function."
  :type 'key-sequence
  :group 'texfrag)

(defcustom texfrag-eww-default-file-name '(make-temp-file "texfrag-eww")
  "LaTeX file name for TeX fragments in eww if the url is not a file:// url.
This can be a file name or a sexp that generates the file name."
  :group 'texfrag
  :type '(choice file sexp))

(defcustom texfrag-pdf-annot-LaTeX-file '(make-temp-file "texfrag-pdf" nil ".tex")
  "LaTeX file name for TeX fragments in pdf-annotations.
This can be a file name or a sexp that generates the file name."
  :group 'texfrag
  :type '(choice file sexp))
  
(define-widget 'texfrag-regexp 'string
  "A regular expression."
  :match 'texfrag-widget-regexp-match
  :validate 'texfrag-widget-regexp-validate
  ;; Doesn't work well with terminating newline.
  ;; :value-face 'widget-single-line-field
  :tag "Regexp")

(defun texfrag-not-regexp-with-refs-p (val)
  "Check whether VAL is *not* a valid regular expression with group references.
Returns the error message of `string-match' if VAL is not a regular expression.
Group references are just ignored.
If VAL is a widget instead of a string (widget-value val) is tested."
  (when (widgetp val)
    (setq val (widget-value val)))
  (let ((pos 0))
    (while (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(\\\\\\)[0-9]" val pos)
      (setq val (replace-match "\\\\\\\\" nil nil val 1)
            pos (match-end 0))))
  (condition-case err
      (string-match val "")
    (error (error-message-string err))))

(defun texfrag-widget-regexp-match (_widget value)
  "Return non-nil if VALUE is a valid regexps with additional group references."
  (and (stringp value)
       (null (texfrag-not-regexp-with-refs-p value))))

(defun texfrag-widget-regexp-validate (widget)
  "Return WIDGET with non-nil :error property if VALUE is not a valid regexps with additional group references."
  (let ((err (texfrag-not-regexp-with-refs-p widget)))
    (when err
      (widget-put widget :error err)
      widget)))

(defcustom texfrag-LaTeX-frag-alist
  '(("\\$\\$" "\\$\\$" "$$" "$$" display)
    ("\\$" "\\$" "$" "$" embedded)
    ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]" display)
    ("\\\\(" "\\\\)" "\\\\(" "\\\\)" embedded)
    ("\\\\begin{\\([a-z*]+\\)}" "\\\\end{\\1}" "\\\\begin{\\2}" "\\\\end{\\2}" display)
    )
  "`texfrag-frag-alist' for LaTeX."
  :group 'texfrag
  :type '(repeat
          (list :tag "Equation filter"
                (regexp :tag "Regexp matching the equation beginning")
                (texfrag-regexp :tag "Regexp matching the equation end")
                (string :tag "Replacement of beginning in LaTeX buffer")
                (string :tag "Replacement of end in LaTeX buffer")
                (choice :tag "Equation type" (const display) (const embedded)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'newcomment)
(require 'tex-site nil t)
(require 'preview nil t)
(require 'cl-lib)
(require 'tex-site)
(require 'tex)
(require 'subr-x)

(defvar texfrag-header-function #'texfrag-header-default
  "Function that collects all LaTeX header contents from the current buffer.")

(defvar texfrag-tail-function "\n\\end{document}"
  "String with the LaTeX tail or Function that collects all preview-LaTeX tail contents from the current buffer.")

(defvar texfrag-next-frag-function #'texfrag-next-frag-default
  "Function that searches for the next LaTeX fragment starting at point.
It is called with:

\(funcall texfrag-next-frag-functionx BOUND)

It returns nil if there is no LaTeX fragment within the region.
Otherwise it returns a list
\(b e eqn match)
with
b: beginning of equation region
e: end of equation region
eqn: equation text inclusive delimiters, e.g.,
     $ or \\begin{align*}...\\end{align*}
     (it is not necessarily equal to (buffer-substring b e))
match: entry from `texfrag-frag-alist' associated with the match")

(defvar texfrag-previous-frag-function #'texfrag-previous-frag-default
  "Function that searches for the previous LaTeX fragment starting at point.
It is called with:

\(funcall \\[texfrag-next-frag-function] BOUND)

It returns nil if there is no LaTeX fragment within the region.
Otherwise it returns a list
\(b e eqn)
with
b: beginning of equation region
e: end of equation region
eqn: equation text inclusive delimiters,
     e.g., $ or \\begin{align*}...\\end{align*}
     (it is not necessarily equal to (buffer-substring b e))
match: entry from `texfrag-frag-alist' associated with the match")

(defvar-local texfrag-comments-only t
  "Only collect LaTeX fragments from comments.
Modify this variable in the major mode hook.")

(defvar-local texfrag-frag-alist
  '(("\\\\f\\$" "\\\\f\\$" "$" "$" embedded)
    ("\\\\f\\[" "\\\\f\\]" "\\[" "\\]" display)
    ("\\\\f{\\([a-z]+[*]?\\)}{" "\\\\f}" "\\\\begin{\\2}" "\\\\end{\\2}" display) ;; e.g., \f{align*}{ some formula \f}
    )
  "Regular expressions for the beginning and the end of formulas.
Override the default in the hook for the major mode.
The default works for some LaTeX fragments in doxygen.

The value is a list of equation-filters.
Each equation-filter is a list of four strings and an optional flag.

The first two strings are regular expressions and match the beginning and the end of an equation in the original buffer.
The last two strings are the beginning and the end of the corresponding equation in the LaTeX buffer.

If the optional flag equals the symbol display then this equation should be displayed on a separate line. It should be embedded otherwise.

Capturing groups can be used in the first two regular expressions. These groups can be referred to in the last two strings.
The indexes for the captures are determined as match for the combined regular expression
\\(beginning regexp\\)\\(end regexp\\).")

(defvar-local texfrag-equation-filter #'identity
  "Filter function transforming the equation text from the original buffer into the equation text in the LaTeX buffer.")

(defun texfrag-combine-regexps (re-template data &optional str)
  "Return a regular expression constructed from RE-TEMPLATE (a string).
Thereby use the previous `match-data' DATA with corresponding string STR
if this was a `string-match'.
Let N be the number of sub-expressions of the previous match then
\\n with n=0,...,N are replaced by the quoted matches from the previous match
and \\n with n=N+1,\ldots are replaced by n-(N+1)."
  (let ((data-n (/ (length data) 2))
	new-pos
	(pos 0)
	(re ""))
    (while (setq new-pos (string-match "\\\\" re-template pos))
      (setq re (concat re (substring re-template pos new-pos))
	    pos (1+ new-pos))
      (cond
       ((eq pos (length re-template))
	(setq re (concat re "\\"))) ;; next match will fail
       ((eq (aref re-template pos) ?\\)
	(setq re (concat re "\\\\"))
	(cl-incf pos))
       ((eq (string-match "[0-9]+" re-template pos) pos)
	(let* ((num-str (match-string 0 re-template))
	       (num (string-to-number num-str)))
	  (setq re (concat re (if (< num data-n)
				  (regexp-quote (save-match-data
						  (set-match-data data)
						  (or (match-string num str) "")))
				(format "\\%d" (- num data-n -1))))
		pos (+ pos (length num-str)))
	  ))
       (t (setq re (concat re "\\")))))
    (setq re (concat re (substring re-template pos)))
    re))

(defun texfrag-combine-match-data (&rest args)
  "Combines the match data in ARGS of multiple `string-match' commands.
The combined data appears as if it originates from one `string-match' command.
Let str_1, str_2, ... be strings and re_1, re_2, ... regular expressions.
Set matches_k to the `match-data' of (`string-match' re_k str_k) for k=1,2,....
\(texfrag-combine-match-data str_1 matches_1 str_2 matches_2 ...)
returns the result of
\(string-match \"\\\\(re_1\\\\)\\\\(re_2\\\\)...\" \"str_1str_2...\")

\(fn str_1 re_1 str_2 re_2 ...)"
  (let (ret
	(offset 0))
    (while args
      (let* ((str (car args))
	     (match-data (cadr args))
	     (str-length (length str)))
	(setq ret (append ret
			  (mapcar (lambda (i) (+ i offset)) match-data))
	      offset (+ offset str-length)
	      args (cddr args))))
    (cons 0 (cons offset ret))))

(defun texfrag-next-frag-default (bound)
  "Search for the next LaTeX fragment in region from `point' to BOUND.
See the documentation of `texfrag-next-frag-function'
for further details about the argument and the return value."
  (let ((re-b (concat (mapconcat 'car texfrag-frag-alist "\\|"))))
    (when (re-search-forward re-b bound t)
      (let* ((bOuter (match-beginning 0))
             (bInner (point))
             (bStr (match-string 0))
             (matchList (cl-assoc bStr texfrag-frag-alist :test (lambda (key candidate) (string-match candidate key))))
             (bMatches (match-data))
	     (e-re (texfrag-combine-regexps (nth 1 matchList) bMatches bStr))
             (eOuter (re-search-forward e-re nil t))
             (eInner (match-beginning 0))
             (eStr (match-string 0)) ;; for consistency
             (eMatches (progn (string-match e-re eStr) (match-data))) ;; for consistency
             (cStr (concat bStr eStr)) ;; combined string
             (cMatches (texfrag-combine-match-data bStr bMatches eStr eMatches))
             )
	(cl-assert eOuter nil "LaTeX fragment beginning at %d with %s not closed." bOuter bStr)
        (set-match-data cMatches)
        (list bOuter eOuter
              (concat (replace-match (nth 2 matchList) nil nil cStr)
                      (funcall texfrag-equation-filter (buffer-substring-no-properties bInner eInner))
                      (replace-match (nth 3 matchList) nil nil cStr))
	      matchList)))))

(defun texfrag-previous-frag-default (bound)
  "Search for the next LaTeX fragment in the region from `point' to BOUND.
See the documentation of `texfrag-previous-frag-function'
for further details about the argument and the return value."
  (let ((re-e (concat (mapconcat 'cadr texfrag-frag-alist "\\|"))))
    (when (re-search-backward re-e bound t)
      (let* ((eInner (match-beginning 0))
             (eOuter (match-end 0))
             (eStr (match-string 0))
             (matchList (cl-rassoc eStr texfrag-frag-alist :test (lambda (key candidate) (string-match (car candidate) key))))
	     (eMatches (match-data))
             (bOuter (re-search-backward (car matchList) nil t))
             (bInner (match-end 0))
	     (bStr (match-string 0))
	     (bMatches (progn (string-match (car matchList) bStr) (match-data)))
	     (cMatches (texfrag-combine-match-data bStr bMatches eStr eMatches))
	     (cStr (concat bStr eStr))
	     )
	(cl-assert bOuter nil "LaTeX fragment ending at %d with %s has no start string." bOuter eStr)
	(set-match-data cMatches)
        (list bOuter eOuter
              (concat (replace-match (nth 2 matchList) nil nil cStr)
                      (funcall texfrag-equation-filter (buffer-substring-no-properties bInner eInner))
                      (replace-match (nth 3 matchList) nil nil cStr))
	      matchList)))))

(defun texfrag-header-default ()
  "Just return the value of the variable `texfrag-header-default'."
  texfrag-header-default)

(defun texfrag-reduce-any (binop pred &rest args)
  "Reduce via BINOP all PRED satisfying ARGS.
Returns nil if none of the args are numbers."
  (let (ret)
    (mapc (lambda (x)
	    (when (funcall pred x)
	      (setq ret (or (and (funcall pred ret)
				 (funcall binop x ret))
			    x))))
	  args)
    ret))

(defun texfrag-comment-start-position ()
  "The position of the start of the current comment, or nil."
  ;; Partial copy from:
  ;; http://emacs.stackexchange.com/questions/21812/if-i-am-in-a-comment-including-empty-lines-how-do-i-get-to-the-beginning-of-t
  (save-excursion
    (let ((res nil))
      (while (progn
               (skip-chars-backward " \t\n")
               (let ((state (syntax-ppss)))
                 (if (nth 4 state)
                     (let ((start (nth 8 state)))
                       (setq res start)
                       (goto-char start)
                       t)
                   nil))))
      res)))

(defun texfrag-comment-end-position ()
  "The position of the end of the current comment, or nil."
  (save-excursion
    (let ((res nil))
      (while (progn
               (if (and
		    (> (skip-chars-forward " \t\n") 0)
		    (null (eobp)))
		   (forward-char))
               (let ((state (syntax-ppss)))
                 (if (nth 4 state)
		     (progn
		       (parse-partial-sexp (point) (point-max) nil nil state 'syntax-table)
		       (setq res (point))
		       (null (eobp)))
                   nil))))
      res)))

(defun texfrag-search-forward-fragment (&optional bound)
  "Search for the next LaTeX fragment in the region from `point' to BOUND.
list with the entries as in `texfrag-next-frag-function':
;(b e str)
b: beginning of LaTeX fragment
e: end of LaTeX fragment
str: LaTeX equation to be inserted in the target LaTeX file"
  (let (found
	e)
    (if texfrag-comments-only
        (while (and (or (null bound) (< (point) bound))
		    (or
		     (setq e (texfrag-comment-end-position)) ;; test whether we are already in a comment
		     (comment-search-forward bound t))
		    (null (setq found (funcall texfrag-next-frag-function (texfrag-reduce-any #'min #'numberp bound (or e (texfrag-comment-end-position)))))))
	  (comment-search-forward bound t))
      (setq found (funcall texfrag-next-frag-function bound)))
    (when found (goto-char (nth 1 found)))
    found))

(defun texfrag-search-backward-fragment (&optional bound)
  "Search backward for next LaTeX fragment in region from `point' to BOUND.
See `texfrag-search-forward-fragment' for further details."
  (let (found
	b)
    (if texfrag-comments-only
        (while (and (or
		     (setq b (texfrag-comment-start-position)) ;; test whether we are already in a comment
		     (comment-search-backward bound t))
		    (null (setq found (funcall texfrag-previous-frag-function (or b (texfrag-comment-start-position))))))
	  (comment-search-backward bound t))
      (setq found (funcall texfrag-previous-frag-function bound)))
    (when found (goto-char (car found)))
    found))

(defvar-local texfrag-LaTeX-file nil
  "Used instead of `buffer-file-name' for function `texfrag-LaTeX-file' when non-nil.
Can be modified in the major mode hook.")
(setq-default TeXFrag-LaTeX-file nil)

(defun texfrag-file-name-option (opt)
  "Get file name from option OPT.
OPT can be a file name string or a Lisp form
that generates the file name string."
  (if (stringp opt)
      opt
    (eval opt)))

(defun texfrag-file-name-escape (name &optional re)
  "Replace chars in NAME that are not ASCII and do not match RE.
RE is a regular expression defaulting to \"[^<>:\"/\\|?*]\".
Relevant characters are replaced by _hex-number."
  (unless re
    (setq re "[^<>:\"/\\|?*]"))
  (mapconcat (lambda (char)
               (let ((str (char-to-string char)))
                 (if (and (/= char ?_)
                          (> char #x20)
                          (< char #x80)
                          (string-match re str))
                     str
                   (format "_%0.2X" char))))
               name ""))

(defun texfrag-LaTeX-file (&optional absolute mkdir)
  "Return name of LaTeX file corresponding to the current buffers source file.
Return the absolute file name if ABSOLUTE is non-nil.
Create the directory of the LaTeX file (inclusive parent directories)
if it does not exist yet and MKDIR is non-nil."
  (let* ((default-directory (or (and texfrag-LaTeX-file (file-name-directory texfrag-LaTeX-file))
                                default-directory))
         (subdir (directory-file-name texfrag-subdir))
         (file (and texfrag-LaTeX-file (file-name-nondirectory texfrag-LaTeX-file)))
         (tex-file
          (texfrag-file-name-escape
           (concat
            (or (and
                 file
                 (null (string-empty-p file))
                 file)
                (file-name-nondirectory
                 (or (buffer-file-name)
                     (buffer-name)))) "." TeX-default-extension)))
         (tex-path (concat subdir "/" tex-file)))
    (when absolute
      (setq tex-path (expand-file-name tex-path)))
    (unless (and mkdir (file-directory-p subdir))
      (mkdir subdir t))
    tex-path))

(defvar-local texfrag-source-buffer nil
  "`texfrag-region' generates a LaTeX-buffer.
This variable is the link back from the LaTeX-buffer to the source buffer.")

(defun texfrag-after-tex ()
  "Buffer-local hook function for `texfrag-after-preview-hook'.
It is used in LaTeX buffers generated by texfrag."
  (cl-loop for ol being the overlays ;; ol is an overlay in the LaTeX buffer
	   if (eq (overlay-get ol 'category) 'preview-overlay)
	   do
	   (when-let ((ol-src (get-text-property (overlay-start ol) 'texfrag-src))
		      (buf-src (overlay-buffer ol-src))
		      (b-src (overlay-start ol-src))
		      (e-src (overlay-end ol-src))
		      (str-src (with-current-buffer buf-src
				 (buffer-substring-no-properties b-src e-src))))
	     ;; The user might change the source string while LaTeX is running.
	     ;; We don't generate overlays for those LaTeX fragments.
	     (if (string= str-src (overlay-get ol-src 'texfrag-string))
		 (move-overlay ol b-src e-src buf-src)
	       (delete-overlay ol))
	     (delete-overlay ol-src))
	   ))

(defvar texfrag-after-preview-hook nil
  "Buffer-local hook run after the preview images have been created.
\(i.e., after `preview-parse-messages').
The current buffer is the one stored in variable `TeX-command-buffer'.")

(defun texfrag-after-preview (&rest _)
  "Run hooks from `texfrag-after-preview-hook' as after-advice for `preview-parse-messages'."
  (with-current-buffer TeX-command-buffer
    (run-hooks 'texfrag-after-preview-hook)))

(advice-add #'preview-parse-messages :after #'texfrag-after-preview)

(defun texfrag-clearout-region (b e)
  "Clear out all texfrag overlays within region from B to E."
  (cl-loop for ol the overlays from b to e
	   if (or (overlay-get ol 'texfrag-string)
		  (equal (overlay-get ol 'category) 'preview-overlay))
	   do (delete-overlay ol)))

(defun texfrag-region (b e)
  "Collect all LaTeX fragments in region from B to E in the latex target file."
  (interactive "r")
  (cl-declare (special auto-insert-alist auto-insert))
  (let ((tex-path (texfrag-LaTeX-file t t))
	(src-buf (current-buffer))
        (coding-sys (or coding-system-for-write buffer-file-coding-system))
	tex-buf
	found
	found-str) ; only non-nil if there are LaTeX-fragments in the document
    (let (auto-insert-alist auto-insert)
      (setq tex-buf (find-file-noselect tex-path)))
    (texfrag-clearout-region b e)
    (save-excursion
      (goto-char b)
      (while (setq found (texfrag-search-forward-fragment e))
	(let* ((found-b (nth 0 found))
	       (found-e (nth 1 found))
	       (ol (make-overlay found-b found-e)))
	  (unless found-str ; only modify the buffer there if at least one LaTeX fragment
	    (with-current-buffer tex-buf
	      (setq buffer-file-coding-system coding-sys)
	      (delete-region (point-min) (point-max))))
	  (setq found-str (buffer-substring-no-properties found-b found-e))
	  (overlay-put ol 'texfrag-string found-str)
	  (with-current-buffer tex-buf
	    (insert "\n" (propertize (nth 2 found) 'texfrag-src ol))))))
    ;;; end of tex-buf:
    (when found-str ;; avoid error messages if no LaTeX fragments were found
      (with-current-buffer tex-buf
	(insert "\n\\end{document}\n%%Local Variables:\n%%TeX-master: t\n%%End:\n")
	(goto-char (point-min))
	(insert (funcall texfrag-header-function))
	(setq-local texfrag-source-buffer src-buf)
	(save-buffer)
	(let (TeX-mode-hook LaTeX-mode-hook)
	  (TeX-latex-mode)
	  (add-hook 'texfrag-after-preview-hook #'texfrag-after-tex t t)
	  (let ((preview-auto-cache-preamble t))
	    (preview-document)
	    ))))))

(defvar-local texfrag-preview-region-function nil
  "A function registered here will override the behavior of `preview-region'.
The function is called with two arguments the beginning and the end of the region to be processed.
It defaults to the original function.")

(defun texfrag-document ()
  "Process LaTeX fragments in the whole document."
  (interactive)
  (funcall texfrag-preview-region-function (point-min) (point-max)))

(defvar texfrag-submap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") #'texfrag-document)
    (define-key map (kbd "C-p") #'preview-at-point)
    (define-key map (kbd "C-l") #'texfrag-show-log)
    map)
  "Keymap of command `texfrag-mode' to be bound to some prefix-key.")

(advice-add #'preview-region :around #'texfrag-preview-region-ad)

(defvar texfrag-mode-map (make-sparse-keymap)
  "Menu for command `texfrag-mode'.")

(easy-menu-define nil texfrag-mode-map "Generating or removing previews with texfrag."
  '("TeX"
    ["Turn off texfrag mode" (texfrag-mode -1) t]
    ["Help for texfrag mode" (describe-function 'texfrag-mode) t]
    "--"
    "Generate previews"
    ["at point" preview-at-point t]
    ["for region" texfrag-region t]
    ["for document" texfrag-document t]
    "--"
    "Remove previews"
    ["at point" preview-clearout-at-point t]
    ["from region" preview-clearout t]
    ["from document" preview-clearout-document t]
    "--"
    "Log"
    ["Show log file" texfrag-show-log t]
    ))

(defun texfrag-set-prefix (prefix)
  "In texfrag mode set the prefix key sequence buffer-locally to PREFIX.
Example:
\(texfrag-set-prefix (kbd \"<C-f12>\"))"
  (define-key texfrag-mode-map texfrag-prefix nil)
  (setq texfrag-prefix prefix)
  (define-key texfrag-mode-map texfrag-prefix texfrag-submap))

(defun texfrag-find-setup-function ()
  "Find the setup function associated to current buffer's major mode."
  (let ((supported-modes (apply 'append (mapcar 'cdr texfrag-setup-alist)))
	(mode major-mode))
    (while
	(and
	 (null (member mode supported-modes))
	 (setq mode (get mode 'derived-mode-parent))))
    (and
     mode
     (car-safe (cl-rassoc-if (lambda (modes) (member mode modes)) texfrag-setup-alist)))))

;;;###autoload
(define-minor-mode texfrag-mode
  "Preview LaTeX fragments in current buffer with the help of the
`preview' package."
  nil
  " TeX"
  nil
  (let ((setup-function (texfrag-find-setup-function)))
    (when setup-function
      (funcall setup-function)))
  (if texfrag-mode
      (progn
        (unless texfrag-preview-region-function
          (setq texfrag-preview-region-function #'texfrag-region))
	(define-key texfrag-mode-map texfrag-prefix texfrag-submap)
	(LaTeX-preview-setup)
	(preview-mode-setup)
	(when texfrag-preview-buffer-at-start
	  (preview-buffer)))
    (preview-clearout-document)))
	
(defun texfrag-global-mode-fun ()
  "Helper function for command `texfrag-global-mode'.
Switch texfrag mode on if the major mode of the current buffer supports it."
  (when (texfrag-find-setup-function)
    (texfrag-mode)))

;;;###autoload
(define-global-minor-mode texfrag-global-mode texfrag-mode
  texfrag-global-mode-fun
  :group 'texfrag
  :require 'texfrag)

(defun texfrag-preview-region-ad (oldfun b e)
  "Around advice for `preview-region'.
It overrides the behavior of `preview-region' with the function
registered at `texfrag-preview-region-function'.
The original `preview-region' function is passed through argument OLDFUN.
B and E are the boundaries of the region to be processed."
  (let ((inhibit-read-only t)) ; we are just putting text properties
					; e.g., required for pdf-annot.
    (if (and
	 texfrag-mode
	 texfrag-preview-region-function)
	(funcall texfrag-preview-region-function b e)
      (funcall oldfun b e))))

(defun texfrag-show-log ()
  "Show log-file of last preview process of current buffer."
  (interactive)
  (TeX-recenter-output-buffer nil))

(defun texfrag-fix-display-math (&optional b e)
  "Insert line breaks around displayed math environments in region from B to E."
  (save-excursion
    (let ((end-marker (make-marker))
	  (eq-end-marker (make-marker)))
      (unwind-protect
          (progn
            (set-marker end-marker (or e (point-max)))
            (goto-char (or b (point-min)))
            (let (math)
              (while (setq math (funcall texfrag-next-frag-function end-marker))
		(when (eq 'display (nth 4 (nth 3 math)))
		  (set-marker eq-end-marker (nth 1 math))
		  (goto-char eq-end-marker)
		  (unless (looking-at "[[:space:]]*$")
		    (insert "\n"))
		  (goto-char (nth 0 math))
		  (unless (looking-back "^[[:space:]]*" (line-beginning-position))
		    (insert "\n"))
		  (goto-char eq-end-marker)
		  ))))
	(set-marker end-marker nil)))))

(defun texfrag-MathJax-filter (str)
  "`texfrag-equation-filter' filtering STR for `texfrag-MathJax-mode'.
Replaces &amp; with &, &lt; with <, and &gt; with >."
  (setq str (replace-regexp-in-string "&amp;" "&" str))
  (setq str (replace-regexp-in-string "&lt;" "<" str))
  (replace-regexp-in-string "&gt;" ">" str))

(defvar texfrag-MathJax-alist
  '(("\\$\\$" "\\$\\$" "$$" "$$")
    ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]")
    ("\\\\(" "\\\\)" "\\\\(" "\\\\)")
    ("\\\\begin{\\([a-z*]+\\)}" "\\\\end{\\1}" "\\\\begin{\\2}" "\\\\end{\\2}"))
  "Value for `texfrag-frag-alist' in modes with MathJax-like syntax.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog-mode

(defun texfrag-prog ()
  "Texfrag setup for `prog-mode'."
  ;; That is just a dummy since the default values are appropriate for prog-mode.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode

(defun texfrag-org ()
  "Texfrag setup for `org-mode'."
  (setq texfrag-frag-alist
	'(("\\$" "\\$" "$" "$")
	  ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]"))
	texfrag-comments-only nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trac-wiki-mode

(defun texfrag-trac-wiki ()
  "Preview TeX-fragments in MathJax html-pages."
  (setq texfrag-comments-only nil
        texfrag-frag-alist '(("^{{{\n#!latex" "^}}}" "" "")
                             ("\\$" "\\$" "$" "$")
                             ("\\\\(" "\\\\)" "\\\\(" "\\\\)")
                             ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]")
                             ("\\\\(" "\\\\)" "\\\\(" "\\\\)")
                             ("\\\\begin{\\([a-z*]+\\)}" "\\\\end{\\1}" "\\\\begin{\\2}" "\\\\end{\\2}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-mode

(defun texfrag-html-region-function (b e)
  "Special `texfrag-region' for region from B to E in html mode.
It differs from `texfrag-region' by skipping the text from buffer-beginning up to <body>."
  (save-excursion
    (goto-char b)
    (unless (search-backward "<body" nil t)
      (goto-char b)
      (setq b (search-forward "<body" e t))))
  (when b
    (texfrag-region b e)))

(defun texfrag-html ()
  "Texfrag setup for `html-mode'."
  (interactive)
  (setq texfrag-comments-only nil
        texfrag-equation-filter #'texfrag-MathJax-filter
        texfrag-frag-alist texfrag-LaTeX-frag-alist
        texfrag-preview-region-function #'texfrag-html-region-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eww-mode

(defun texfrag-eww-file-name ()
  "Retrieve file name from ‘eww-data’."
  (texfrag-file-name-option texfrag-eww-default-file-name))

(defun texfrag-eww ()
  "Texfrag setup for `eww-mode'."
  (if texfrag-mode
      (progn
	(setq texfrag-comments-only nil
	      texfrag-equation-filter #'texfrag-MathJax-filter
	      texfrag-frag-alist '(("\\$\\$" "\\$\\$" "$$" "$$")
				   ("\\$" "\\$" "$" "$")
				   ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]")
				   ("\\\\(" "\\\\)" "\\\\(" "\\\\)")
				   ("\\\\begin{\\([a-z*]+\\)}" "\\\\end{\\1}" "\\\\begin{\\2}" "\\\\end{\\2}")))
	(add-hook 'eww-after-render-hook #'texfrag-eww-set-LaTeX-file nil t))
    (remove-hook 'eww-after-render-hook #'texfrag-eww-set-LaTeX-file t)))

(defun texfrag-eww-set-LaTeX-file ()
  "Render LaTeX fragments in the current eww buffer.
Set variable `texfrag-LaTeX-file' to the file name of the current eww window."
  (setq texfrag-LaTeX-file (texfrag-eww-file-name))
  (when texfrag-LaTeX-file
    (texfrag-region (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sx-question-mode

(defun texfrag-sx-after-print (&rest _args)
  "Set LaTeX formulas after printing of the stackexchange question is done.
Can be used for advice of `sx-question-mode--print-question'
or as `sx-question-mode-after-print-hook'."
  (when texfrag-mode ;;< if sx.el works with global advice
    (texfrag-fix-display-math)
    (texfrag-document)))

(declare-function sx-question-mode--print-question
		  "sx-question-print.el")

(defun texfrag-sx ()
  "Texfrag setup for `sx-question-mode'."
  (setq
   texfrag-comments-only nil
   texfrag-frag-alist texfrag-LaTeX-frag-alist)
  (if texfrag-mode
      (if (special-variable-p 'sx-question-mode-after-print-hook)
	  (add-hook 'sx-question-mode-after-print-hook #'texfrag-sx-after-print nil t)
	(advice-add #'sx-question-mode--print-question :after #'texfrag-sx-after-print)
	;;> Eventually the advice should completely be replaced by the hook.
	)
    (if (special-variable-p 'sx-question-mode-after-print-hook)
	(remove-hook 'sx-question-mode-after-print-hook #'texfrag-sx-after-print t)
      (advice-remove #'sx-question-mode--print-question #'texfrag-sx-after-print))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'texfrag)
;;; texfrag.el ends here
