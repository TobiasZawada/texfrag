;;; TeXfrag.el --- preview LaTeX fragments in alien major modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: tex, languages, wp

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

;; Enable LaTeX-preview in non-LaTeX modes.
;; Strategy:
;; - collect all header contents and latex fragments from the source buffer
;; - prepare a LaTeX document from this information
;; - let preview do its stuff in the LaTeX document
;; - move the preview overlays from the LaTeX document to the source buffer
;;
;; The defaults are adapted to doxygen.
;; For the support of LaTeX fragments in doxygen comments put the following
;; into your init file:
;;
;; (require 'TeXfrag)
;; (add-hook 'prog-mode-hook #'TeXfrag-mode-on)
;;
;; The default prefix-key sequence for TeXfrag-mode is the same as for preview-mode, i.e., C-c C-p.
;; You can change the default prefix-key sequence by customizing TeXfrag-prefix.
;; If you want to modify the prefix key sequence just for one major mode use
;; `TeXfrag-set-prefix' in the major mode hook.
;;
;; You can adapt the LaTeX-header to your needs by buffer-locally setting
;; the variable `TeXfrag-header-function' to a function without arguments
;; that returns the LaTeX header as a string. Inspect the definition of
;; `TeXfrag-header-default' as an example.
;;
;; The easiest way to adapt the LaTeX fragment syntax of some major mode
;; is to set `TeXfrag-frag-alist' in the mode hook of that major mode.
;; For `org-mode' the function `TeXfrag-org-mode-hook-function'
;; can be used as minimal implementation of such a hook function.
;; Install it via
;; (add-hook 'org-mode-hook #'TeXfrag-org-mode-hook-function)
;; if you like. Note that this function only handles the most primitive
;; syntax for LaTeX fragments in org-mode buffers, i.e., $...$ and \[\].
;;
;; For more complicated cases you can install your own
;; parsers of LaTeX fragments in the variable
;; `TeXfrag-next-frag-function' (see the documentation of that variable).
;;
;;; Code:

(defgroup TeXfrag nil "Preview LaTeX fragments in buffers with non-LaTeX major modes."
  :group 'preview)

(defcustom TeXfrag-header-default
  "\\documentclass{article}
\\usepackage{amsmath,amsfonts}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\begin{document}"
  "LaTeX header inserted by the function `TeXfrag-header-default' into the LaTeX buffer."
  :group 'TeXfrag
  :type 'string)

(defcustom TeXfrag-subdir "TeXfrag"
  "Name of the sub-directory for the preview data."
  :group 'TeXfrag
  :type 'string)

(defcustom TeXfrag-prefix (kbd "C-c C-p")
  "Prefix for TeXfrag keys.
Defaults to the prefix of preview-mode \"C-c C-p\".
If there is a collision with the major mode you can change this prefix in the major mode's hook function."
  :type 'key-sequence
  :group 'TeXfrag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'newcomment)
(require 'tex-site nil t)
(require 'preview nil t)
(require 'cl-lib)

(defvar TeXfrag-header-function #'TeXfrag-header-default
  "Function that collects all LaTeX header contents from the current buffer.")

(defvar TeXfrag-tail-function "\n\\end{document}"
  "String with the LaTeX tail or Function that collects all preview-LaTeX tail contents from the current buffer.")

(defvar TeXfrag-next-frag-function #'TeXfrag-next-frag-default
  "Function that searches for the next LaTeX fragment starting at point.
It is called with:

(funcall TeXfrag-next-frag-function BOUND)

It returns nil if there is no LaTeX fragment within the region.
Otherwise it returns a list
(b e eqn)
with
b: beginning of equation region
e: end of equation region
eqn: equation text inclusive delimiters, e.g., $ or \\begin{align*}...\\end{align*} (it is not necessarily equal to (buffer-substring b e))")

(defvar TeXfrag-previous-frag-function #'TeXfrag-previous-frag-default
  "Function that searches for the previous LaTeX fragment starting at point.
It is called with:

(funcall TeXfrag-next-frag-function BOUND)

It returns nil if there is no LaTeX fragment within the region.
Otherwise it returns a list
(b e eqn)
with
b: beginning of equation region
e: end of equation region
eqn: equation text inclusive delimiters, e.g., $ or \\begin{align*}...\\end{align*} (it is not necessarily equal to (buffer-substring b e))")

(defvar-local TeXfrag-comments-only t
  "Only collect LaTeX fragments from comments.
Modify this variable in the major mode hook.")

(make-variable-buffer-local 'TeXfrag-comments-only)

(defvar-local TeXfrag-frag-alist
  '(("\\\\f\\$" "\\\\f\\$" "$" "$")
    ("\\\\f\\[" "\\\\f\\]" "\\[" "\\]")
    ("\\\\f{\\([a-z]+[*]?\\)}{" "\\\\f}" "\\\\begin{\\2}" "\\\\end{\\2}") ;; e.g., \f{align*}{ some formula \f}
    )
  "Regular expressions for the beginning and the end of formulas.
Override the default in the hook for the major mode.
The default works for some LaTeX fragments in doxygen.

The value is a list of equation-filters.
Each equation-filter is a list of four strings.

The first two strings are regular expressions and match the beginning and the end of an equation in the original buffer.
The last two strings are the beginning and the end of the corresponding equation in the LaTeX buffer.

Capturing groups can be used in the first two regular expressions. These groups can be referred to in the last two strings.
The indexes for the captures are determined as match for the combined regular expression
\\(beginning regexp\\)\\(end regexp\\).
")

(defvar-local TeXfrag-equation-filter #'identity
  "Filter function transforming the equation text from the original buffer into the equation text in the LaTeX buffer.")

(defun TeXfrag-org-mode-hook-function ()
  "If you want to use TeXfrag in org-mode use this function as follows:
 (add-hook 'org-mode-hook #'TeXfrag-org-hook-function)"
  (setq TeXfrag-frag-alist
	'(("\\$" "\\$" "$" "$")
	  ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]"))
	TeXfrag-comments-only nil)
  (TeXfrag-mode 1))

(defun TeXfrag-combine-regexps (re-template data &optional str)
  "Return a regular expression constructed from the RE-TEMPLATE,
the previous match-data DATA with corresponding string STR if this was a `string-match'.
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

(defun TeXfrag-combine-match-data (&rest args)
  "Combines the match data of multiple `string-match' commands as if it was one `string-match' command.
Let str_1, str_2, ... be strings and re_1, re_2, ... regular expressions.
Set matches_k to the `match-data' of (`string-match' re_k str_k) for k=1,2,....
(TeXfrag-combine-match-data str_1 matches_1 str_2 matches_2 ...)
returns the result of
(string-match \"\\\\(re_1\\\\)\\\\(re_2\\\\)...\" \"str_1str_2...\")

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

(defun TeXfrag-next-frag-default (bound)
  "Search for the next LaTeX fragment.
See the documentation of `TeXfrag-next-frag-function' for further details about the argument and the return value."
  (let ((re-b (concat (mapconcat 'car TeXfrag-frag-alist "\\|"))))
    (when (re-search-forward re-b bound t)
      (let* ((bOuter (match-beginning 0))
             (bInner (point))
             (bStr (match-string 0))
             (matchList (cl-assoc bStr TeXfrag-frag-alist :test (lambda (key candidate) (string-match candidate key))))
             (bMatches (match-data))
	     (e-re (TeXfrag-combine-regexps (nth 1 matchList) bMatches bStr))
             (eOuter (re-search-forward e-re nil t))
             (eInner (match-beginning 0))
             (eStr (match-string 0)) ;; for consistency
             (eMatches (progn (string-match e-re eStr) (match-data))) ;; for consistency
             (cStr (concat bStr eStr)) ;; combined string
             (cMatches (TeXfrag-combine-match-data bStr bMatches eStr eMatches))
             )
	(cl-assert eOuter nil "LaTeX fragment beginning at %d with %s not closed." bOuter bStr)
        (set-match-data cMatches)
        (list bOuter eOuter
              (concat (replace-match (nth 2 matchList) nil nil cStr)
                      (funcall TeXfrag-equation-filter (buffer-substring-no-properties bInner eInner))
                      (replace-match (nth 3 matchList) nil nil cStr)))))))

(defun TeXfrag-previous-frag-default (bound)
  "Search for the next LaTeX fragment.
See the documentation of `TeXfrag-previous-frag-function' for further details about the argument and the return value."
  (let ((re-e (concat (mapconcat 'cadr TeXfrag-frag-alist "\\|"))))
    (when (re-search-backward re-e bound t)
      (let* ((eInner (match-beginning 0))
             (eOuter (match-end 0))
             (eStr (match-string 0))
             (matchList (cl-rassoc eStr TeXfrag-frag-alist :test (lambda (key candidate) (string-match (car candidate) key))))
	     (eMatches (match-data))
             (bOuter (re-search-backward (car matchList) nil t))
             (bInner (match-end 0))
	     (bStr (match-string 0))
	     (bMatches (progn (string-match (car matchList) bStr) (match-data)))
	     (cMatches (TeXfrag-combine-match-data bStr bMatches eStr eMatches))
	     (cStr (concat bStr eStr))
	     )
	(cl-assert bOuter nil "LaTeX fragment ending at %d with %s has no start string." bOuter eStr)
	(set-match-data cMatches)
        (list bOuter eOuter
              (concat (replace-match (nth 2 matchList) nil nil cStr)
                      (funcall TeXfrag-equation-filter (buffer-substring-no-properties bInner eInner))
                      (replace-match (nth 3 matchList) nil nil cStr)))))))

(defun TeXfrag-header-default ()
  "Just return the value of the variable `TeXfrag-header-default'."
  TeXfrag-header-default)

(defun TeXfrag-reduce-any (binop pred &rest args)
  "Reduce via BINOP all args that satisfy PRED.
Returns `nil' if none of the args are numbers."
  (let (ret)
    (mapc (lambda (x)
	    (when (funcall pred x)
	      (setq ret (or (and (funcall pred ret)
				 (funcall binop x ret))
			    x))))
	  args)
    ret))

(defun TeXfrag-comment-start-position ()
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

(defun TeXfrag-comment-end-position ()
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
		       t)
                   nil))))
      res)))

(defun TeXfrag-search-forward-fragment (&optional bound)
  "Search forward for the next LaTeX fragment.
list with the entries as in `TeXfrag-next-frag-function':
;(b e str)
b: beginning of LaTeX fragment
e: end of LaTeX fragment
str: LaTeX equation to be inserted in the target LaTeX file"
  (let (found
	e)
    (if TeXfrag-comments-only
        (while (and (or (null bound) (< (point) bound))
		    (or
		     (setq e (TeXfrag-comment-end-position)) ;; test whether we are already in a comment
		     (comment-search-forward bound t))
		    (null (setq found (funcall TeXfrag-next-frag-function (TeXfrag-reduce-any #'min #'numberp bound (or e (TeXfrag-comment-end-position)))))))
	  (comment-search-forward bound t))
      (setq found (funcall TeXfrag-next-frag-function bound)))
    (when found (goto-char (nth 1 found)))
    found))

(defun TeXfrag-search-backward-fragment (&optional bound)
  "Search backward for the next LaTeX fragment.
See `TeXfrag-search-forward-fragment' for further details."
  (let (found
	b)
    (if TeXfrag-comments-only
        (while (and (or
		     (setq b (TeXfrag-comment-start-position)) ;; test whether we are already in a comment
		     (comment-search-backward bound t))
		    (null (setq found (funcall TeXfrag-previous-frag-function (or b (TeXfrag-comment-start-position))))))
	  (comment-search-backward bound t))
      (setq found (funcall TeXfrag-previous-frag-function bound)))
    (when found (goto-char (car found)))
    found))

(defun TeXfrag-LaTeX-file (&optional absolute mkdir)
  "Return name of LaTeX file corresponding to the current buffers source file.
Return the absolute file name if ABSOLUTE is non-nil.
Create the directory of the LaTeX file (inclusive parent directories)
if it does not exist yet and MKDIR is non-nil."
  (let* ((dir (directory-file-name TeXfrag-subdir))
         (tex-file (concat (file-name-nondirectory (buffer-file-name)) "." TeX-default-extension))
         (tex-path (concat dir "/" tex-file)))
    (when absolute
      (setq tex-path (expand-file-name tex-path)))
    (unless (and mkdir (file-directory-p dir))
      (mkdir dir t))
    tex-path))

(defvar-local TeXfrag-source-buffer nil
  "`TeXfrag-region' generates a LaTeX-buffer.
This variable is the link back from the LaTeX-buffer to the source buffer.")

(defun TeXfrag-after-tex ()
  "Buffer-locally installed hook function at `TeXfrag-after-preview-hook'
in LaTeX buffers generated by TeXfrag."
  (cl-loop for ol being the overlays ;; ol is an overlay in the LaTeX buffer
	   if (eq (overlay-get ol 'category) 'preview-overlay)
	   do 
	   (when-let ((ol-src (get-text-property (overlay-start ol) 'TeXfrag-src))
		      (buf-src (overlay-buffer ol-src))
		      (b-src (overlay-start ol-src))
		      (e-src (overlay-end ol-src))
		      (str-src (with-current-buffer buf-src
				 (buffer-substring-no-properties b-src e-src))))
	     ;; The user might change the source string while LaTeX is running.
	     ;; We don't generate overlays for those LaTeX fragments.
	     (if (string= str-src (overlay-get ol-src 'TeXfrag-string))
		 (move-overlay ol b-src e-src buf-src)
	       (delete-overlay ol))
	     (delete-overlay ol-src))
	   ))

(defvar TeXfrag-after-preview-hook nil
  "Buffer-local hook variable for functions run after the preview images have been created.
(i.e., after `preview-parse-messages').
The current buffer is `TeX-command-buffer'.")

(defun TeXfrag-after-preview (&rest _)
  "After-advice for `preview-parse-messages' that runs the hooks from `TeXfrag-after-preview-hook'."
  (with-current-buffer TeX-command-buffer
    (run-hooks 'TeXfrag-after-preview-hook)))

(advice-add #'preview-parse-messages :after #'TeXfrag-after-preview)

(defun TeXfrag-clearout-region (b e)
  "Clear out all TeXfrag overlays within region from B to E."
  (cl-loop for ol the overlays from b to e
	   if (or (overlay-get ol 'TeXfrag-string)
		  (equal (overlay-get ol 'category) 'preview-overlay))
	   do (delete-overlay ol)))

(defun TeXfrag-region (b e)
  "Collect all LaTeX fragments in region from B to E
in the latex target file."
  (interactive "r")
  (cl-declare (special auto-insert-alist auto-insert))
  (let ((tex-path (TeXfrag-LaTeX-file nil t))
	(src-buf (current-buffer))
	tex-buf 
	found)
    (let (auto-insert-alist auto-insert)
      (setq tex-buf (find-file-noselect tex-path))
      (with-current-buffer tex-buf
        (delete-region (point-min) (point-max))
        (insert (funcall TeXfrag-header-function))
        ))
    (TeXfrag-clearout-region b e)
    (save-excursion
      (goto-char b)
      (while (setq found (TeXfrag-search-forward-fragment e))
	(let* ((found-b (nth 0 found))
	       (found-e (nth 1 found))
	       (found-str (buffer-substring-no-properties found-b found-e))
	       (ol (make-overlay found-b found-e)))
	  (overlay-put ol 'TeXfrag-string found-str)
	  (with-current-buffer tex-buf
	    (insert "\n" (propertize (nth 2 found) 'TeXfrag-src ol))))))
    ;;; end of tex-buf:
    (with-current-buffer tex-buf
      (insert "\n\\end{document}\n%%Local Variables:\n%%TeX-master: t\n%%End:\n")
      (setq-local TeXfrag-source-buffer src-buf)
      (save-buffer)
      (normal-mode)
      (add-hook 'TeXfrag-after-preview-hook #'TeXfrag-after-tex t t)
      (let ((preview-auto-cache-preamble t))
	(preview-document)))))

(defun TeXfrag-document ()
  "Process LaTeX fragments in the whole document."
  (interactive)
  (funcall TeXfrag-preview-region-function (point-min) (point-max)))

(defvar TeXfrag-submap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") #'TeXfrag-document)
    (define-key map (kbd "C-p") #'preview-at-point)
    map)
  "Keymap of TeXfrag-mode to be bound to some prefix-key.")

(defvar-local TeXfrag-preview-region-function nil
  "A function registered here will override the behavior of `preview-region'.
The function is called with two arguments the beginning and the end of the region to be processed.
It defaults to the original function.")

(defun TeXfrag-preview-region-ad (oldfun b e)
  "Around advice for `preview-region'.
It overrides the behavior of `preview-region' with the function registered at `TeXfrag-preview-region-function'."
  (if (and
       TeXfrag-mode
       TeXfrag-preview-region-function)
      (funcall TeXfrag-preview-region-function b e)
    (funcall oldfun b e)))

(advice-add #'preview-region :around #'TeXfrag-preview-region-ad)

(defvar TeXfrag-mode-map (make-sparse-keymap)
  "Menu for `TeXfrag-mode'.")

(easy-menu-define nil TeXfrag-mode-map "Generating or removing previews with TeXfrag."
  '("TeX"
    ["Turn off TeXfrag mode" (TeXfrag-mode -1) t]
    ["Help for TeXfrag mode" (describe-function 'TeXfrag-mode) t]
    "--"
    "Generate previews"
    ["at point" preview-at-point t]
    ["for region" TeXfrag-region t]
    ["for document" TeXfrag-document t]
    "--"
    "Remove previews"
    ["at point" preview-clearout-at-point t]
    ["from region" preview-clearout t]
    ["from document" preview-clearout-document t]
    ))

(defun TeXfrag-set-prefix (prefix)
  "Set the prefix key sequence for `TeXfrag-mode' buffer-locally to PREFIX.
Example:
 (TeXfrag-set-prefix (kbd \"<C-f12>\"))"
  (define-key TeXfrag-mode-map TeXfrag-prefix nil)
  (setq TeXfrag-prefix prefix)
  (define-key TeXfrag-mode-map TeXfrag-prefix TeXfrag-submap))

(define-minor-mode TeXfrag-mode
  "Preview LaTeX fragments in current buffer with the help of the
  `preview' package."
  nil
  " TeX"
  nil
  (if TeXfrag-mode
      (progn
        (unless TeXfrag-preview-region-function
          (setq TeXfrag-preview-region-function #'TeXfrag-region))
	(define-key TeXfrag-mode-map TeXfrag-prefix TeXfrag-submap)
	(LaTeX-preview-setup)
	(preview-mode-setup))))

(defun TeXfrag-mode-on ()
  "Unconditionally switch TeXfrag-mode on."
  (TeXfrag-mode 1))

(defun TeXfrag-MathJax-filter (str)
  "`TeXfrag-equation-filter' for `TeXfrag-MathJax-mode'.
Replaces &amp; with &, &lt; with <, and &gt; with >."
  (setq str (replace-regexp-in-string "&amp;" "&" str))
  (setq str (replace-regexp-in-string "&lt;" "<" str))
  (replace-regexp-in-string "&gt;" ">" str))

(defun TeXfrag-MathJax-hook ()
  "Preview TeX-fragments in MathJax html-pages."
  (setq TeXfrag-comments-only nil
        TeXfrag-equation-filter #'TeXfrag-MathJax-filter
        TeXfrag-frag-alist '(("$$" "$$" "$$" "$$")
                             ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]")
                             ("\\\\(" "\\\\)" "\\\\(" "\\\\)")
                             ("\\\\begin{\\([a-z*]+\\)}" "\\\\end{\\1}" "\\\\begin{\\2}" "\\\\end{\\2}")))
  (TeXfrag-mode 1))

(defun TeXfrag-MadCap-region-function (b e)
  "Like `TeXfrag-region' (which see) but skip the text from buffer-beginning up to <body>."
  (save-excursion
    (goto-char b)
    (unless (search-backward "<body" nil t)
      (goto-char b)
      (setq b (search-forward "<body" e t))))
  (when b
    (TeXfrag-region b e)))

(defun TeXfrag-MadCap ()
  "Preview TeX-fragments in MadCap pages.
Can be used in `html-mode-hook'."
  (interactive)
  (setq TeXfrag-comments-only nil
        TeXfrag-equation-filter #'TeXfrag-MathJax-filter
        TeXfrag-frag-alist '(("\\$\\$" "\\$\\$" "$$" "$$")
                             ("\\$" "\\$" "$" "$")
                             ("\\\\\\[" "\\\\\\]" "\\\\[" "\\\\]")
                             ("\\\\(" "\\\\)" "\\\\(" "\\\\)")
                             ("\\\\begin{\\([a-z*]+\\)}" "\\\\end{\\1}" "\\\\begin{\\2}" "\\\\end{\\2}"))
        TeXfrag-preview-region-function #'TeXfrag-MadCap-region-function)
  (TeXfrag-mode 1))

(add-hook 'html-mode-hook #'TeXfrag-MadCap)

(provide 'TeXfrag)
;;; TeXfrag.el ends here