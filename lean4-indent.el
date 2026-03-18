;;; lean4-indent.el --- Indentation functions for lean4-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides Lean 4 indentation commands for `lean4-mode'.
;; It is experimental and intended primarily for my personal use.
;; Much of the current implementation was developed with Codex against
;; a growing test suite.  Expect rough edges.
;;
;; To use it, call `lean4-indent-setup-buffer' in a `lean4-mode-hook':
;;
;;   (add-hook 'lean4-mode-hook
;;             (lambda ()
;;               (lean4-indent-setup-buffer)))
;;
;; Optional bindings:
;;
;;   (with-eval-after-load 'lean4-mode
;;     (define-key lean4-mode-map (kbd "C-c <") #'lean4-indent-shift-left)
;;     (define-key lean4-mode-map (kbd "C-c >") #'lean4-indent-shift-right))
;;
;; In Lean buffers, C-j inserts an indented newline and RET inserts a plain
;; newline.  The shift commands are available as
;; `lean4-indent-shift-left' and `lean4-indent-shift-right'; the optional
;; snippet above binds them to C-c < and C-c >.

;;; Code:

(require 'subr-x)

;;;; Shift commands

(defun lean4-indent-shift-left (start end &optional count)
  "Shift lines in region START..END left by COUNT columns.

COUNT defaults to `lean4-indent-offset'. If the region is not active, shift the
current line. Signal an error if any nonblank line would be shifted past column
0."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (setq count (if count (prefix-numeric-value count) lean4-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (when (and (< (current-indentation) count)
                     (not (looking-at-p "[ \t]*$")))
            (user-error "Can't shift all lines enough"))
          (forward-line 1))
        (indent-rigidly start end (- count))))))

(defun lean4-indent-shift-right (start end &optional count)
  "Shift lines in region START..END right by COUNT columns.

COUNT defaults to `lean4-indent-offset'. If the region is not active, shift the
current line."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count) lean4-indent-offset))
    (indent-rigidly start end count)))

;;;; Main indentation function

(defgroup lean4-indent nil
  "Lean 4 indentation."
  :group 'languages)

(defcustom lean4-indent-offset 2
  "Indentation step for Lean 4."
  :type 'integer
  :safe #'integerp)

(defvar lean4-indent--region-top-level-contexts nil
  "Dynamically bound top-level context cache for `lean4-indent-region-function'.")

(defvar lean4-indent--region-line-contexts nil
  "Dynamically bound per-line context cache for `lean4-indent-region-function'.")

(defvar lean4-indent--preserve-tactic-region-indentation nil
  "Dynamically non-nil when region indentation should preserve tactic layout.")

;;;###autoload
(defun lean4-indent-setup-buffer ()
  "Install Lean 4 line and region indentation functions in the current buffer."
  (interactive)
  (setq-local indent-line-function #'lean4-indent-line-function)
  (setq-local indent-region-function #'lean4-indent-region-function))

(defconst lean4-indent--ops
  '("+" "-" "*" "/" "•" "≤" "≥" "≠" "∧" "∨" "↔" "→" "↦" "<;>"
    "<=" ">=" "=>" "->" "≅" "≃")
  "Operators that trigger line continuation when they end a line.")

(defconst lean4-indent--simp-like-keywords
  '("simp" "simp_rw" "simp?" "simpa" "rwa")
  "Keywords treated as simp-like tactics.")

(defconst lean4-indent--top-level-anchors
  '("attribute" "add_decl_doc" "compile_inductive" "set_option" "open" "universe" "variable"
    "@[" "scoped["
    "namespace" "section" "public section")
  "Non-declaration top-level anchors that snap to column 0 when not nested.")

(defconst lean4-indent--top-level-anchors-re
  (concat "\\`[ \t]*\\(?:"
          (mapconcat (lambda (anchor)
                       (let ((quoted (regexp-quote anchor)))
                         (if (string-match-p "[[:word:]]\\'" anchor)
                             (concat quoted "\\_>")
                           quoted)))
                     lean4-indent--top-level-anchors
                     "\\|")
          "\\)")
  "Regex matching a top-level anchor at the start of a line.")

(defconst lean4-indent--re-blank "\\`[ \t]*\\'")
(defconst lean4-indent--re-label-colon "^[ \t]*[A-Z][A-Z0-9_]*:[ \t]*$")
(defconst lean4-indent--re-macro-rules "^[ \t]*\\(?:scoped[ \t]+\\)?macro_rules\\_>")
(defconst lean4-indent--re-starts-paren "\\(?:[(]\\|{\\|\\[\\)")
(defconst lean4-indent--re-starts-branch "|\\(?:\\s-\\|$\\)")
(defconst lean4-indent--re-starts-focus "·")
(defconst lean4-indent--re-starts-namespace "namespace\\_>")
(defconst lean4-indent--re-starts-section "section\\_>")
(defconst lean4-indent--re-starts-public-section "public[ \\t]+section\\_>")
(defconst lean4-indent--re-starts-mutual "mutual\\_>")
(defconst lean4-indent--re-starts-end "\\_<end\\_>")
(defconst lean4-indent--re-starts-where "\\_<where\\_>")
(defconst lean4-indent--re-starts-let "\\_<let\\_>")
(defconst lean4-indent--re-starts-calc "\\_<calc\\_>")
(defconst lean4-indent--re-classical-exact-fun
  "\\_<classical\\_>\\s-+\\_<exact\\_>\\s-+\\_<fun\\_>")
(defconst lean4-indent--calc-relops
  '("=" "≤" "<" "≥" ">" "≠" "≃" "≅" "≈" "≡" "↔")
  "Relation operators that can start a calc step.")
(defconst lean4-indent--re-starts-calc-step
  (concat "\\s-*_[ \t]*\\(?:" (regexp-opt lean4-indent--calc-relops t) "\\)"))
(defconst lean4-indent--re-starts-closing "[])}⟩]")
(defconst lean4-indent--re-ends-by "\\_<by\\_>")
(defconst lean4-indent--re-ends-where "\\_<where\\_>")
(defconst lean4-indent--re-ends-calc "\\_<calc\\_>")
(defconst lean4-indent--re-ends-coloneq-by ":=\\s-*\\_<by\\_>")
(defconst lean4-indent--re-colon ":")
(defconst lean4-indent--re-coloneq ":=")
(defconst lean4-indent--re-ends-fat-arrow "=>")
(defconst lean4-indent--re-ends-do "\\_<do\\_>\\s-*\\'")
(defconst lean4-indent--re-ends-then "\\_<then\\_>\\s-*\\'")
(defconst lean4-indent--re-ends-else "\\_<else\\_>\\s-*\\'")
(defconst lean4-indent--re-ends-fun-arrow "↦")
(defconst lean4-indent--term-continuation-ops
  '("<;>" ";" "*")
  "Operators that continue the same term on the next line.")
(defconst lean4-indent--re-ends-term-continuation
  (concat "\\(?:" (regexp-opt lean4-indent--term-continuation-ops t) "\\)"))
(defconst lean4-indent--re-ends-equals "=")
(defconst lean4-indent--re-ends-comma ",")
(defconst lean4-indent--re-ends-termination "\\_<termination_by\\_>\\s-*\\'")
(defconst lean4-indent--re-ends-decreasing "\\_<decreasing_by\\_>\\s-*\\'")
(defconst lean4-indent--re-ends-at-star "\\_<at\\_>\\s-*\\*$")
(defconst lean4-indent--re-starts-simp-like
  (concat "\\_<" (regexp-opt lean4-indent--simp-like-keywords t) "\\_>"))
(defconst lean4-indent--re-starts-exact "\\_<exact\\_>")
(defconst lean4-indent--re-from "\\_<from\\_>")
(defconst lean4-indent--re-with "\\_<with\\_>")
(defconst lean4-indent--re-with-anchor
  "\\_<\\(match\\|nomatch\\|cases\\|cases'\\|induction\\)\\_>.*\\_<with\\_>\\s-*\\(?:--.*\\)?$")
(defconst lean4-indent--re-have-suffices "\\_<\\(have\\|suffices\\)\\_>")
(defconst lean4-indent--re-ends-angle-close "⟩\\s-*$")

(defconst lean4-indent--char-left-angle (string-to-char "⟨")
  "Lean angle-bracket open character as a codepoint.")

(defconst lean4-indent--char-right-angle (string-to-char "⟩")
  "Lean angle-bracket close character as a codepoint.")
(defconst lean4-indent--re-fun-by
  ":=\\s-*\\_<fun\\_>.*↦\\s-*\\_<by\\_>\\s-*$")

(defconst lean4-indent--ops-regexp
  (concat "\\(?:" (regexp-opt lean4-indent--ops t) "\\)\\s-*$")
  "Regexp matching end-of-line operator continuations.")

(defun lean4-indent--line-text (pos)
  "Return the text of the line at POS without properties."
  (save-excursion
    (goto-char pos)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun lean4-indent--line-text-no-comment (pos)
  "Return line text at POS with any trailing line comment stripped."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (cut eol))
      (goto-char bol)
      (while (re-search-forward "--" eol t)
        (let ((ppss (syntax-ppss (match-beginning 0))))
          (unless (or (nth 3 ppss) (nth 4 ppss))
            (setq cut (match-beginning 0))
            (goto-char eol))))
      (buffer-substring-no-properties bol cut))))

(defun lean4-indent--line-blank-p (text)
  "Return non-nil if TEXT is blank."
  (string-match-p lean4-indent--re-blank text))

(defun lean4-indent--line-indent (pos)
  "Return indentation of line at POS."
  (save-excursion
    (goto-char pos)
    (current-indentation)))

(defun lean4-indent--prev-nonblank ()
  "Return position of previous nonblank line or nil."
  (save-excursion
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (unless (lean4-indent--line-blank-p text)
            (setq found (point)))))
      found)))

;;; Paren helpers
(defun lean4-indent--open-paren-pos (&optional pos)
  "Return position of innermost open paren for line at POS, or nil."
  (save-excursion
    (goto-char (or pos (point)))
    (back-to-indentation)
    (let* ((ppss (syntax-ppss))
           (open (nth 1 ppss)))
      open)))

(defun lean4-indent--open-paren-col (&optional pos)
  "Return column of innermost open paren for line at POS, or nil."
  (let ((open (lean4-indent--open-paren-pos pos)))
    (when open
      (save-excursion
        (goto-char open)
        (current-column)))))

(defun lean4-indent--line-closes-paren-p (pos)
  "Return non-nil if the line at POS closes any paren/bracket/brace."
  (save-excursion
    (goto-char pos)
    (let ((start-depth (car (syntax-ppss (line-beginning-position))))
          (end-depth (car (syntax-ppss (line-end-position)))))
      (< end-depth start-depth))))

(defun lean4-indent--line-opens-paren-p (pos)
  "Return non-nil if the line at POS opens any paren/bracket/brace."
  (save-excursion
    (goto-char pos)
    (let ((start-depth (car (syntax-ppss (line-beginning-position))))
          (end-depth (car (syntax-ppss (line-end-position)))))
      (> end-depth start-depth))))

(defun lean4-indent--line-starts-with-paren-and-closes-p (pos)
  "Return non-nil if line at POS starts with a paren and closes it on the same line."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (when (looking-at-p lean4-indent--re-starts-paren)
      (condition-case nil
          (let ((end (scan-sexps (point) 1)))
            (and end (<= end (line-end-position))))
        (error nil)))))

(defun lean4-indent--line-contains-balanced-bracket-p (pos)
  "Return non-nil if the line at POS contains a balanced [...] on the same line."
  (save-excursion
    (goto-char pos)
    (if (lean4-indent--comment-line-p pos)
        nil
      (let ((bol (line-beginning-position))
            (eol (line-end-position))
            (found nil))
        (goto-char bol)
        (while (and (not found) (re-search-forward "\\[" eol t))
          (let* ((start (match-beginning 0))
                 (ppss (save-excursion (syntax-ppss start))))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (condition-case nil
                  (let ((end (scan-sexps start 1)))
                    (when (and end (<= end eol))
                      (setq found t)))
                (scan-error nil)))))
        found))))

(defun lean4-indent--open-paren-col-at-eol (pos)
  "Return column of innermost open paren at end of line POS, or nil."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (let* ((ppss (syntax-ppss))
           (open (nth 1 ppss)))
      (when open
        (save-excursion
          (goto-char open)
          (current-column))))))

(defun lean4-indent--open-paren-pos-at-eol (pos)
  "Return position of innermost open paren at end of line POS, or nil."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (nth 1 (syntax-ppss))))

(defun lean4-indent--last-unmatched-open-brace-pos (pos)
  "Return the position of the last unmatched `{` on line POS, or nil."
  (save-excursion
    (goto-char pos)
    (let ((stack nil)
          (end (line-end-position)))
      (while (< (point) end)
        (let* ((ppss (syntax-ppss (point)))
               (in-str (nth 3 ppss))
               (in-com (nth 4 ppss))
               (ch (char-after)))
          (unless (or in-str in-com)
            (cond
             ((eq ch ?{) (push (point) stack))
             ((eq ch ?}) (when stack (pop stack))))))
        (forward-char 1))
      (car stack))))

(defun lean4-indent--matching-open-delimiter-pos-at-eol (pos)
  "Return the matching opener for the last delimiter on line POS, or nil."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (skip-chars-backward " \t")
    (when (> (point) (line-beginning-position))
      (condition-case nil
          (scan-sexps (point) -1)
        (error nil)))))

(defun lean4-indent--starts-with-p (text regex)
  "Return non-nil if TEXT starts with REGEX after indentation."
  (string-match-p (concat "\\`[ \t]*" regex) text))

(defun lean4-indent--ends-with-p (text regex)
  "Return non-nil if TEXT ends with REGEX."
  (string-match-p (concat regex "\\s-*$") text))

(defun lean4-indent--ends-with-keyword-p (text regex)
  "Return non-nil if TEXT ends with REGEX, allowing trailing line comments."
  (string-match-p (concat regex "\\s-*\\(?:--.*\\)?$") text))

(defun lean4-indent--line-ends-with-colon-p (text)
  (and (lean4-indent--ends-with-keyword-p text lean4-indent--re-colon)
       (not (lean4-indent--ends-with-keyword-p text lean4-indent--re-coloneq))))

(defun lean4-indent--line-ends-with-coloneq-by-p (text)
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-coloneq-by))

(defun lean4-indent--line-ends-with-coloneq-p (text)
  (and (lean4-indent--ends-with-keyword-p text lean4-indent--re-coloneq)
       (not (lean4-indent--line-ends-with-coloneq-by-p text))))

(defun lean4-indent--line-ends-with-by-p (text)
  (and (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-by)
       (not (lean4-indent--line-ends-with-coloneq-by-p text))))

(defun lean4-indent--line-ends-with-fat-arrow-p (text)
  "Return non-nil if TEXT ends with =>, allowing a trailing line comment."
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-fat-arrow))

(defun lean4-indent--line-ends-with-do-p (text)
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-do))

(defun lean4-indent--line-ends-with-then-p (text)
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-then))

(defun lean4-indent--line-ends-with-else-p (text)
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-else))

(defun lean4-indent--line-ends-with-fun-arrow-p (text)
  (lean4-indent--ends-with-p text lean4-indent--re-ends-fun-arrow))

(defun lean4-indent--line-ends-with-term-continuation-p (text)
  (lean4-indent--ends-with-p text lean4-indent--re-ends-term-continuation))

(defun lean4-indent--line-ends-with-semicolon-p (text)
  (lean4-indent--ends-with-p text ";"))

(defun lean4-indent--line-ends-with-at-star-p (text)
  (string-match-p lean4-indent--re-ends-at-star text))

(defun lean4-indent--comment-line-p (&optional pos)
  "Return t if POS is on a comment line (line or block)."
  (save-excursion
    (goto-char (or pos (point)))
    (let* ((bol (line-beginning-position))
           (ppss (syntax-ppss bol))
           (text (lean4-indent--line-text bol))
           (trim (string-trim-left text)))
      (or (nth 4 ppss)
          (string-prefix-p "--" trim)
          (string-prefix-p "/-" trim)))))

(defun lean4-indent--prev-have-suffices-p (pos limit-indent)
  "Return t if a prior line contains have/suffices before POS with indent < LIMIT-INDENT."
  (save-excursion
    (goto-char pos)
    (catch 'found
      (while (not (bobp))
        (forward-line -1)
        (let* ((text (lean4-indent--line-text (point)))
               (indent (lean4-indent--line-indent (point))))
          (unless (lean4-indent--line-blank-p text)
            (when (and (< indent limit-indent)
                       (string-match-p lean4-indent--re-have-suffices text))
              (throw 'found t))
            (when (lean4-indent--line-top-level-anchor-p text)
              (throw 'found nil)))))
      nil)))

(defun lean4-indent--line-starts-with-calc-step-p (text)
  (string-match-p (concat "\\`" lean4-indent--re-starts-calc-step) text))

(defun lean4-indent--line-starts-with-closing-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-closing))

(defun lean4-indent--line-ends-with-where-p (text)
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-where))

(defun lean4-indent--line-ends-with-calc-p (text)
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-calc))

(defun lean4-indent--line-ends-with-equals-p (text)
  (and (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-equals)
       (not (lean4-indent--ends-with-keyword-p text lean4-indent--re-coloneq))))

(defun lean4-indent--line-ends-with-comma-p (text)
  (lean4-indent--ends-with-p text lean4-indent--re-ends-comma))

(defun lean4-indent--line-body-intro-kind (text)
  "Classify how TEXT introduces a following indented body.

Return a symbol such as `colon', `coloneq', `by', or
`bare-have-suffices', or nil if TEXT does not introduce a body."
  (cond
   ((lean4-indent--line-ends-with-colon-p text) 'colon)
   ((lean4-indent--line-ends-with-coloneq-by-p text) 'coloneq-by)
   ((lean4-indent--line-ends-with-coloneq-p text) 'coloneq)
   ((lean4-indent--line-ends-with-by-p text) 'by)
   ((lean4-indent--line-ends-with-fat-arrow-p text) 'fat-arrow)
   ((lean4-indent--line-ends-with-fun-arrow-p text) 'fun-arrow)
   ((lean4-indent--line-ends-with-do-p text) 'do)
   ((lean4-indent--line-ends-with-then-p text) 'then)
   ((lean4-indent--line-ends-with-else-p text) 'else)
   ((lean4-indent--line-ends-with-calc-p text) 'calc)
   ((lean4-indent--line-ends-with-equals-p text) 'equals)
   ((lean4-indent--line-ends-with-where-p text) 'where)
   ((string-match-p lean4-indent--re-ends-termination text) 'termination)
   ((string-match-p lean4-indent--re-ends-decreasing text) 'decreasing)
   ((string-match-p "\\`[ \t]*\\_<\\(?:have\\|suffices\\)\\_>\\s-*\\'" text)
    'bare-have-suffices)
   ((lean4-indent--ends-with-p text "{") 'open-brace)
   (t nil)))

(defun lean4-indent--line-ends-with-op-p (text)
  (string-match-p lean4-indent--ops-regexp (string-trim-right text)))

(defun lean4-indent--operator-continuation-p (text)
  "Return non-nil if TEXT ends with an operator and does not trigger other indent rules."
  (and (lean4-indent--line-ends-with-op-p text)
       (not (lean4-indent--line-body-intro-kind text))))

(defun lean4-indent--line-top-level-declaration-head-p (text)
  "Return non-nil if TEXT starts a top-level declaration header."
  (string-match-p
   "\\`[ \t]*\\(?:\\_<\\(?:protected\\|private\\|noncomputable\\|unsafe\\|partial\\)\\_>\\s-+\\)*\\_<\\(?:def\\|instance\\|partial_fixpoint\\|theorem\\|lemma\\|example\\|structure\\|inductive\\|class\\|abbrev\\|macro\\|syntax\\|notation\\)\\_>"
   text))

(defun lean4-indent--line-top-level-anchor-p (text)
  (or (lean4-indent--line-top-level-declaration-head-p text)
      (string-match-p lean4-indent--top-level-anchors-re text)))

(defun lean4-indent--line-top-level-binder-head-kind (text)
  "Classify a top-level binder head line TEXT.

Return `declaration' for ordinary wrapped declaration headers, `variable' for
wrapped `variable' lines, or nil if TEXT is neither."
  (cond
   ((lean4-indent--line-top-level-declaration-head-p text) 'declaration)
   ((string-match-p "\\`[ \t]*\\_<variable\\_>" text) 'variable)
   (t nil)))

(defun lean4-indent--line-starts-with-paren-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-paren))

(defun lean4-indent--line-starts-with-fun-form-p (text)
  "Return non-nil if TEXT starts with a `fun` form, possibly after `(`.

This also treats `classical exact fun` as a fun form."
  (string-match-p
   "\\`[ \t]*\\(?:([ \t]*\\)*\\(?:\\_<classical\\_>\\s-+\\_<exact\\_>\\s-+\\)?\\_<fun\\_>"
   text))

(defun lean4-indent--line-application-head-kind (text)
  "Classify TEXT when it is a plain term head that may take later arguments.

Return `atom' for a single identifier-like head, `application' for an
unparenthesized application head, or nil otherwise."
  (let* ((trim (string-trim text))
         (body-intro-kind (lean4-indent--line-body-intro-kind trim)))
    (and (not body-intro-kind)
         (not (string-match-p lean4-indent--top-level-anchors-re trim))
         (not (string-match-p
               "\\`\\_<\\(?:fun\\|let\\|calc\\|where\\|end\\|if\\|match\\|do\\|have\\|suffices\\)\\_>"
               trim))
         (not (lean4-indent--branch-line-p trim))
         (not (lean4-indent--focus-dot-line-p trim))
         (not (lean4-indent--line-starts-with-closing-p trim))
         (cond
          ((string-match-p "\\`[[:word:]_.']+\\'" trim)
           'atom)
          ((and (string-match-p "\\`[[:word:]_.']+\\(?:\\s-+.+\\)\\'" trim)
                (not (lean4-indent--line-ends-with-comma-p trim))
                (not (lean4-indent--line-ends-with-term-continuation-p trim)))
           'application)
          (t nil)))))

(defun lean4-indent--body-intro-indent (kind pos indent step)
  "Return indent of the first body line introduced by KIND.
POS is the position of the line introducing the body, at indentation
INDENT."
  (pcase kind
    ('coloneq (+ indent step))
    ((or 'fat-arrow 'fun-arrow 'then 'else) (+ indent step))
    ('equals
     (if (and pos (lean4-indent--in-calc-block-p pos))
         (+ indent (* 2 step))
       (+ indent step)))
    (_ nil)))

(defun lean4-indent--line-starts-structured-term-p (text)
  "Return non-nil if TEXT can begin the next structured piece of a term."
  (let ((trim (string-trim-left text)))
    (or (string-match-p "\\`fun\\_>" trim)
        (string-match-p "\\`by\\_>" trim)
        (string-match-p "\\`if\\_>" trim)
        (string-match-p "\\`match\\_>" trim)
        (string-match-p "\\`let\\_>" trim)
        (string-match-p "\\`have\\_>" trim)
        (string-match-p "\\`show\\_>" trim)
        (string-match-p "\\`calc\\_>" trim)
        (string-match-p "\\`do\\_>" trim)
        (lean4-indent--line-starts-with-paren-p trim)
        (string-match-p "\\`⟨" trim))))

(defun lean4-indent--line-starts-with-relop-p (text)
  "Return non-nil if TEXT starts with a relation operator."
  (string-match-p "\\`[ \t]*\\(?:=\\|≤\\|≥\\|↔\\|≠\\)" text))

(defun lean4-indent--show-body-column (text)
  "Return the body column for a `show` line TEXT, or nil."
  (when (string-match "\\`[ \t]*show\\s-+" text)
    (match-end 0)))

(defun lean4-indent--tactic-term-tail-head-kind (text)
  "Classify the term tail on a tactic line TEXT, or return nil.

This recognizes tactic commands whose first argument is an ordinary
term and reuses `lean4-indent--line-application-head-kind' on that
tail."
  (let ((trim (string-trim-left text)))
    (when (string-match
           "\\`\\(?:exact\\|refine\\|apply\\)\\_>\\s-+\\(.+\\)\\'"
           trim)
      (lean4-indent--line-application-head-kind (match-string 1 trim)))))

(defun lean4-indent--bare-tactic-term-intro-line-p (text)
  "Return non-nil if TEXT is a bare `exact'/`refine'/`apply' line."
  (string-match-p "\\`[ \t]*\\(?:exact\\|refine\\|apply\\)\\_>[ \t]*\\'" text))

(defun lean4-indent--line-leading-angle-paren-col (text)
  "Return the column of the leading '(' in a line starting with `⟨(`, or nil."
  (when (string-match "\\`[ \t]*⟨(" text)
    (1- (match-end 0))))

(defun lean4-indent--next-significant-line-top-level-anchor-p ()
  "Return non-nil if the next nonblank, noncomment line is a top-level anchor."
  (save-excursion
    (let ((found nil)
          (done nil))
      (while (and (not done) (not (eobp)))
        (forward-line 1)
        (let ((text (lean4-indent--line-text (point))))
          (cond
           ((or (lean4-indent--line-blank-p text)
                (lean4-indent--comment-line-p (point))))
           (t
            (setq done t)
            (setq found (lean4-indent--line-top-level-anchor-p text))))))
      found)))

(defun lean4-indent--next-significant-noncomment-line-top-level-anchor-p ()
  "Return non-nil if the next significant non-comment line is top-level."
  (save-excursion
    (let ((found nil)
          (done nil))
      (while (and (not done) (not (eobp)))
        (forward-line 1)
        (let ((text (lean4-indent--line-text (point))))
          (cond
           ((lean4-indent--line-blank-p text))
           ((lean4-indent--comment-line-p (point)))
           (t
            (setq done t)
            (setq found (lean4-indent--line-top-level-anchor-p text))))))
      found)))

(defun lean4-indent--branch-line-p (text)
  "Return non-nil if TEXT is a branch line."
  (lean4-indent--starts-with-p text lean4-indent--re-starts-branch))

(defun lean4-indent--focus-dot-line-p (text)
  "Return non-nil if TEXT starts with a focus dot."
  (lean4-indent--starts-with-p text lean4-indent--re-starts-focus))

(defun lean4-indent--macro-rules-line-p (text)
  "Return non-nil if TEXT is a macro_rules line."
  (string-match-p lean4-indent--re-macro-rules text))

(defun lean4-indent--label-colon-line-p (text)
  "Return non-nil if TEXT is an all-caps label ending in a colon."
  (let ((case-fold-search nil))
    (string-match-p lean4-indent--re-label-colon text)))

(defun lean4-indent--line-is-bare-sorry-p (text)
  (let ((trim (string-trim text)))
    (and (string= trim "sorry")
         (not (string-match-p lean4-indent--re-coloneq text))
         (not (string-match-p lean4-indent--re-from text)))))

(defun lean4-indent--line-unmatched-angle-p (pos)
  "Return non-nil if the line at POS has more '⟨' than '⟩', ignoring strings/comments."
  (save-excursion
    (goto-char pos)
    (let ((open 0)
          (close 0)
          (end (line-end-position)))
      (while (< (point) end)
        (let* ((ppss (syntax-ppss (point)))
               (in-str (nth 3 ppss))
               (in-com (nth 4 ppss))
               (ch (char-after)))
          (unless (or in-str in-com)
            (cond
             ((eq ch lean4-indent--char-left-angle) (setq open (1+ open)))
             ((eq ch lean4-indent--char-right-angle) (setq close (1+ close))))))
        (forward-char 1))
      (> open close))))

(defun lean4-indent--leading-paren-count (text)
  "Return the number of leading '(' after indentation in TEXT."
  (let* ((trim (string-trim-left text))
         (i 0)
         (count 0))
    (while (and (< i (length trim)) (= (aref trim i) ?\())
      (setq count (1+ count))
      (setq i (1+ i)))
    count))

(defun lean4-indent--last-unmatched-open-paren-col (pos)
  "Return the column of the outermost unmatched '(' on the line at POS."
  (save-excursion
    (goto-char pos)
    (let ((stack nil)
          (end (line-end-position)))
      (while (< (point) end)
        (let* ((ppss (syntax-ppss (point)))
               (in-str (nth 3 ppss))
               (in-com (nth 4 ppss))
               (ch (char-after)))
          (unless (or in-str in-com)
            (cond
             ((eq ch ?\() (push (point) stack))
             ((eq ch ?\)) (when stack (pop stack))))))
        (forward-char 1))
      (let ((outer (car (last stack))))
        (when outer
          (goto-char outer)
          (current-column))))))

(defun lean4-indent--colon-before-paren-p (pos)
  "Return non-nil if a ':' appears before any '(' on the line at POS, ignoring strings/comments."
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((found-colon nil)
            (found-paren nil)
            (end (line-end-position)))
        (while (and (< (point) end) (not found-paren))
          (let* ((ppss (syntax-ppss (point)))
                 (in-str (nth 3 ppss))
                 (in-com (nth 4 ppss))
                 (ch (char-after)))
            (unless (or in-str in-com)
              (cond
               ((and (eq ch ?:)
                     (not (eq (char-after (1+ (point))) ?=))
                     (not (eq (char-after (1+ (point))) ?:))
                     (not (eq (char-before (point)) ?:)))
                (setq found-colon t))
               ((eq ch ?\() (setq found-paren t)))))
          (forward-char 1))
        (and found-colon (not found-paren))))))

(defun lean4-indent--line-has-outer-coloneq-p (pos)
  "Return non-nil if line at POS contains a `:=` at delimiter depth 0."
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((found nil)
            (end (line-end-position)))
        (while (and (< (point) end) (not found))
          (let* ((ppss (syntax-ppss (point)))
                 (depth (car ppss))
                 (in-str (nth 3 ppss))
                 (in-com (nth 4 ppss)))
            (when (and (= depth 0)
                       (not in-str)
                       (not in-com)
                       (eq (char-after) ?:)
                       (eq (char-after (1+ (point))) ?=))
              (setq found t)))
          (forward-char 1))
        found))))

(defun lean4-indent--scan-prev-paren-lines (start-pos prev-indent dedent-indent)
  "Scan backward from START-POS for earlier paren-led lines.

PREV-INDENT is the indentation of the immediately previous nonblank line.
DEDENT-INDENT is the target upper bound used for `_)`-style dedents.

Return a plist with:

- `:block-min-indent' for the consecutive preceding paren-led block,
- `:sibling-indent' for the nearest earlier paren-led line indented less than
  PREV-INDENT,
- `:dedent-indent' for the nearest earlier paren-led line indented at or below
  DEDENT-INDENT."
  (save-excursion
    (goto-char start-pos)
    (let ((block-min-indent nil)
          (sibling-indent nil)
          (found-dedent-indent nil)
          (in-leading-block t))
      (while (and (not (bobp))
                  (or in-leading-block
                      (not sibling-indent)
                      (not found-dedent-indent)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (cond
           ((lean4-indent--line-blank-p text) nil)
           ((lean4-indent--line-starts-with-paren-p text)
            (let ((indent (lean4-indent--line-indent (point))))
              (when in-leading-block
                (setq block-min-indent
                      (if block-min-indent
                          (min block-min-indent indent)
                        indent)))
              (when (and (not sibling-indent)
                         (< indent prev-indent))
                (setq sibling-indent indent))
              (when (and (not found-dedent-indent)
                         (<= indent dedent-indent))
                (setq found-dedent-indent indent))))
           (t
            (setq in-leading-block nil)))))
      (list :block-min-indent block-min-indent
            :sibling-indent sibling-indent
            :dedent-indent found-dedent-indent))))

(defun lean4-indent--open-delimited-body-indent (open-paren-pos open-paren-col step)
  "Return body indentation inside a delimited term.

OPEN-PAREN-POS and OPEN-PAREN-COL describe the opener, and STEP is the current
indentation step.  If the opener shares its line with earlier text, indent from
the line; otherwise indent from the delimiter column."
  (let* ((open-line-indent
          (and open-paren-pos
               (save-excursion
                 (goto-char open-paren-pos)
                 (current-indentation))))
         (open-line-prefix
          (and open-paren-pos
               (save-excursion
                 (goto-char open-paren-pos)
                 (buffer-substring-no-properties
                  (line-beginning-position) open-paren-pos))))
         (open-line-has-real-text
          (and open-line-prefix
               (string-match-p "[^ \t(\\[{⟨]" open-line-prefix)))
         (open-line-has-only-delimiters
          (and open-line-prefix
               (string-match-p "[^ \t]" open-line-prefix)
               (not open-line-has-real-text))))
    (cond
     ((and open-line-has-only-delimiters open-paren-col)
      (+ open-paren-col step))
     (open-line-has-real-text
      (+ open-line-indent step))
     (t
      (+ open-paren-col step)))))

;;; Anchor helpers
(defun lean4-indent--find-anchor (prev-pos prev-indent)
  "Find nearest prior nonblank line with indent < PREV-INDENT.
Return (anchor-pos . anchor-indent), or nil if none."
  (let ((context (lean4-indent--region-line-context prev-pos)))
    (if context
        (let ((anchor-pos (plist-get context :anchor-pos))
              (anchor-indent (plist-get context :anchor-indent)))
          (and anchor-pos
               (< anchor-indent prev-indent)
               (cons anchor-pos anchor-indent)))
      (save-excursion
        (goto-char prev-pos)
        (let ((found nil))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (let ((text (lean4-indent--line-text (point))))
              (unless (lean4-indent--line-blank-p text)
                (let ((indent (lean4-indent--line-indent (point))))
                  (when (< indent prev-indent)
                    (setq found (cons (point) indent)))))))
          found)))))

(defun lean4-indent--anchor-parent-indent (anchor-pos anchor-indent step)
  "Compute the parent indentation from ANCHOR-POS/ANCHOR-INDENT.
If the anchor itself is a continuation line, dedent one level.
Otherwise return anchor-indent + step."
  (if (not anchor-pos)
      step
    (let* ((anchor2 (lean4-indent--find-anchor anchor-pos anchor-indent))
           (anchor2-indent (if anchor2 (cdr anchor2) 0)))
      (if (= anchor-indent (+ anchor2-indent (* 2 step)))
          (max 0 (- anchor-indent step))
        (+ anchor-indent step)))))

(defun lean4-indent--find-enclosing-body-intro-anchor (prev-pos prev-indent kinds)
  "Find nearest enclosing anchor whose body-intro kind is in KINDS.
Search upward from PREV-POS/PREV-INDENT through the anchor chain.
Return (anchor-pos . anchor-indent), or nil if none matches."
  (let ((anchor (and prev-pos (lean4-indent--find-anchor prev-pos prev-indent)))
        (found nil))
    (while (and anchor (not found))
      (let* ((anchor-pos (car anchor))
             (anchor-indent (cdr anchor))
             (anchor-text-no-comment
              (if (and anchor-pos (not (lean4-indent--comment-line-p anchor-pos)))
                  (lean4-indent--line-text-no-comment anchor-pos)
                ""))
             (anchor-kind
              (and anchor-pos
                   (lean4-indent--line-body-intro-kind anchor-text-no-comment))))
        (if (memq anchor-kind kinds)
            (setq found anchor)
          (setq anchor (and anchor-pos
                            (lean4-indent--find-anchor anchor-pos anchor-indent))))))
    found))

(defun lean4-indent--find-with-indent (start-pos limit-indent)
  "Find indent of nearest preceding match/cases/induction `with` line.
Only consider lines with indentation <= LIMIT-INDENT when LIMIT-INDENT is non-nil."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let* ((text (lean4-indent--line-text (point)))
               (indent (lean4-indent--line-indent (point))))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-p (point)))
            (when (and (or (not limit-indent) (<= indent limit-indent))
                       (lean4-indent--line-ends-with-with-p (point)))
              (setq found indent)))))
      found)))

(defun lean4-indent--line-has-with-p (pos)
  "Return non-nil if line at POS contains `with` outside strings/comments."
  (save-excursion
    (goto-char pos)
    (let ((eol (line-end-position))
          (found nil))
      (while (and (not found) (re-search-forward lean4-indent--re-with eol t))
        (let ((ppss (syntax-ppss (match-beginning 0))))
          (unless (or (nth 3 ppss) (nth 4 ppss))
            (setq found t))))
      found)))

(defun lean4-indent--line-ends-with-with-p (pos)
  "Return non-nil if POS ends a match/cases/induction `with` line."
  (let* ((text (lean4-indent--line-text-no-comment pos)))
    (and (string-match-p lean4-indent--re-with-anchor text)
         (lean4-indent--line-has-with-p pos))))

;;; Calc helpers
(defun lean4-indent--in-calc-block-p (start-pos)
  "Return non-nil if START-POS is inside a calc block."
  (let ((context (lean4-indent--region-line-context start-pos)))
    (if context
        (plist-get context :calc-indent)
      (save-excursion
        (goto-char start-pos)
        (let ((found nil)
              (start-indent (current-indentation)))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (let* ((text (lean4-indent--line-text (point)))
                   (indent (lean4-indent--line-indent (point))))
              (unless (or (lean4-indent--line-blank-p text)
                          (lean4-indent--comment-line-p (point)))
                (when (and (< indent start-indent)
                           (lean4-indent--starts-with-p text lean4-indent--re-starts-calc))
                  (setq found t))
                (when (< indent start-indent)
                  (setq start-indent indent)))))
          found)))))

(defun lean4-indent--find-calc-step-indent (start-pos)
  "Return indentation of nearest calc step line above START-POS, or nil."
  (let ((context (lean4-indent--region-line-context start-pos)))
    (if context
        (plist-get context :calc-step-indent)
      (save-excursion
        (goto-char start-pos)
        (let ((step-indent (current-indentation))
              (found nil)
              (done nil))
          (while (and (not found) (not done) (not (bobp)))
            (forward-line -1)
            (let ((text (lean4-indent--line-text (point)))
                  (indent (lean4-indent--line-indent (point))))
              (cond
               ((lean4-indent--line-blank-p text) nil)
               ((lean4-indent--starts-with-p text lean4-indent--re-starts-calc)
                (setq done t))
               ((and (<= indent step-indent)
                     (or (lean4-indent--line-starts-with-calc-step-p text)
                         (lean4-indent--line-ends-with-equals-p text)))
                (setq found (lean4-indent--line-indent (point)))))))
          found)))))

(defun lean4-indent--calc-block-body-indent (start-pos base-indent step)
  "Return indent for a term body inside a surrounding calc block, or nil."
  (when (and start-pos (lean4-indent--in-calc-block-p start-pos))
    (let ((calc-indent (lean4-indent--find-calc-step-indent start-pos)))
      (if calc-indent (+ calc-indent step) (+ base-indent step)))))

(defun lean4-indent--find-top-level-anchor (start-pos)
  "Return position of nearest top-level anchor before START-POS, or nil."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (unless (lean4-indent--comment-line-p (point))
            (when (lean4-indent--line-top-level-anchor-p text)
              (setq found (point))))))
      found)))

(defun lean4-indent--top-level-anchor-body-indent (start-pos step)
  "Return body indentation under the nearest top-level anchor before START-POS."
  (let ((top (and start-pos (lean4-indent--find-top-level-anchor start-pos))))
    (and top (+ (lean4-indent--line-indent top) step))))

(defun lean4-indent--top-level-declaration-body-intro-from (top start-pos)
  "Return top-level body-intro info for declaration TOP around START-POS.

For wrapped declarations, scan forward from the top-level declaration head to
the first line that introduces the declaration body, such as `:=', `:= by', or
`where'.  Return a plist with keys `:pos' and `:kind', or nil when START-POS is
not inside such a declaration."
  (when top
    (let ((top-text (lean4-indent--line-text top)))
      (when (lean4-indent--line-top-level-declaration-head-p top-text)
        (save-excursion
          (goto-char top)
          (let ((fallback-kind nil)
                (fallback-pos nil))
            (while (<= (point) start-pos)
              (let ((text (lean4-indent--line-text (point))))
                (unless (or (lean4-indent--line-blank-p text)
                            (lean4-indent--comment-line-p (point)))
                  (when (> (point) top)
                    (when (lean4-indent--line-top-level-anchor-p text)
                      (setq fallback-kind nil
                            fallback-pos nil)
                      (goto-char (1+ start-pos))))
                  (let ((kind (lean4-indent--line-body-intro-kind
                               (lean4-indent--line-text-no-comment (point)))))
                    (cond
                     ((memq kind '(coloneq-by coloneq by where))
                      (setq fallback-kind kind
                            fallback-pos (point))
                      (goto-char (1+ start-pos)))
                     ((and kind (not fallback-kind))
                      (setq fallback-kind kind
                            fallback-pos (point)))))))
              (forward-line 1))
            (and fallback-kind
                 (list :pos fallback-pos :kind fallback-kind))))))))

(defun lean4-indent--top-level-declaration-body-intro-kind-from (top start-pos)
  "Return the body-intro kind for a top-level declaration TOP around START-POS."
  (plist-get (lean4-indent--top-level-declaration-body-intro-from top start-pos)
             :kind))

(defun lean4-indent--top-level-context (start-pos step)
  "Return top-level context for START-POS as a plist, or nil."
  (or (and start-pos
           lean4-indent--region-top-level-contexts
           (let ((index (1- (line-number-at-pos start-pos t))))
             (and (>= index 0)
                  (< index (length lean4-indent--region-top-level-contexts))
                  (aref lean4-indent--region-top-level-contexts index))))
      (let ((top (and start-pos (lean4-indent--find-top-level-anchor start-pos))))
        (when top
          (let ((body-intro
                 (lean4-indent--top-level-declaration-body-intro-from top start-pos)))
            (list :pos top
                  :body-indent (+ (lean4-indent--line-indent top) step)
                  :body-intro-pos (plist-get body-intro :pos)
                  :body-intro-kind (plist-get body-intro :kind)))))))

(defun lean4-indent--top-level-declaration-body-intro-kind (start-pos)
  "Return the body-intro kind for the nearest enclosing top-level declaration."
  (plist-get (lean4-indent--top-level-context start-pos lean4-indent-offset)
             :body-intro-kind))

(defun lean4-indent--region-line-context (pos)
  "Return cached per-line context for POS, or nil when unavailable."
  (and pos
       lean4-indent--region-line-contexts
       (let ((index (1- (line-number-at-pos pos t))))
         (and (>= index 0)
              (< index (length lean4-indent--region-line-contexts))
              (aref lean4-indent--region-line-contexts index)))))

(defun lean4-indent--build-top-level-context-cache (step)
  "Build a per-line top-level context cache using indentation STEP."
  (save-excursion
    (goto-char (point-min))
    (let* ((line-count (line-number-at-pos (point-max) t))
           (contexts (make-vector (max 1 line-count) nil))
           (current-top-pos nil)
           (current-top-is-decl nil)
           (current-body-intro-pos nil)
           (current-body-intro-kind nil)
           (current-body-intro-final-p nil)
           (current-body-indent nil))
      (while (not (eobp))
        (let* ((pos (point))
               (text (lean4-indent--line-text pos)))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-p pos))
            (when (lean4-indent--line-top-level-anchor-p text)
              (setq current-top-pos (copy-marker pos)
                    current-top-is-decl (lean4-indent--line-top-level-declaration-head-p text)
                    current-body-intro-pos nil
                    current-body-intro-kind nil
                    current-body-intro-final-p nil
                    current-body-indent (+ (lean4-indent--line-indent pos) step)))
            (when (and current-top-pos current-top-is-decl
                       (not current-body-intro-final-p))
              (let ((kind (lean4-indent--line-body-intro-kind
                           (lean4-indent--line-text-no-comment pos))))
                (cond
                 ((memq kind '(coloneq-by coloneq by where))
                  (setq current-body-intro-pos (copy-marker pos)
                        current-body-intro-kind kind
                        current-body-intro-final-p t))
                 ((and kind (not current-body-intro-kind))
                  (setq current-body-intro-pos (copy-marker pos)
                        current-body-intro-kind kind))))))
          (aset contexts
                (1- (line-number-at-pos pos t))
                (and current-top-pos
                     (list :pos current-top-pos
                           :body-indent current-body-indent
                           :body-intro-pos current-body-intro-pos
                           :body-intro-kind current-body-intro-kind))))
        (forward-line 1))
      contexts)))

(defun lean4-indent--build-region-line-context-cache ()
  "Build a per-line cache for enclosing `mutual' and `calc' context."
  (save-excursion
    (goto-char (point-min))
    (let* ((line-count (line-number-at-pos (point-max) t))
           (contexts (make-vector (max 1 line-count) nil))
           (block-stack nil)
           (calc-stack nil)
           (indent-stack nil))
      (while (not (eobp))
        (let* ((pos (point))
               (text (lean4-indent--line-text pos))
               (nonblank (not (lean4-indent--line-blank-p text)))
               (significant (and nonblank
                                 (not (lean4-indent--comment-line-p pos))))
               (indent (and nonblank (lean4-indent--line-indent pos))))
          (when nonblank
            (while (and indent-stack
                        (>= (cdar indent-stack) indent))
              (pop indent-stack)))
          (when significant
            (while (and calc-stack
                        (<= indent (plist-get (car calc-stack) :indent)))
              (pop calc-stack)))
          (aset contexts
                (1- (line-number-at-pos pos t))
                (list :mutual-indent
                      (catch 'mutual
                        (dolist (entry block-stack)
                          (when (eq (car entry) 'mutual)
                            (throw 'mutual (cdr entry))))
                        nil)
                      :anchor-pos (caar indent-stack)
                      :anchor-indent (cdar indent-stack)
                      :calc-indent (plist-get (car calc-stack) :indent)
                      :calc-step-indent (plist-get (car calc-stack) :last-step-indent)))
          (when nonblank
            (push (cons (copy-marker pos) indent) indent-stack))
          (when significant
            (when (and calc-stack
                       (> indent (plist-get (car calc-stack) :indent))
                       (or (lean4-indent--line-starts-with-calc-step-p text)
                           (lean4-indent--line-ends-with-equals-p text)))
              (setf (plist-get (car calc-stack) :last-step-indent) indent))
            (cond
             ((lean4-indent--starts-with-p text lean4-indent--re-starts-end)
              (when block-stack
                (pop block-stack)))
             (t
              (when (lean4-indent--starts-with-p text lean4-indent--re-starts-mutual)
                (push (cons 'mutual indent) block-stack))
              (when (lean4-indent--starts-with-p text lean4-indent--re-starts-public-section)
                (push (cons 'public-section indent) block-stack))
              (when (lean4-indent--starts-with-p text lean4-indent--re-starts-section)
                (push (cons 'section indent) block-stack))
              (when (lean4-indent--starts-with-p text lean4-indent--re-starts-namespace)
                (push (cons 'namespace indent) block-stack))
              (when (lean4-indent--starts-with-p text lean4-indent--re-starts-calc)
                (push (list :indent indent :last-step-indent nil) calc-stack)))))
          (forward-line 1)))
      contexts)))

(defun lean4-indent--find-end-anchor-indent (start-pos)
  "Return indentation for an `end` line based on the nearest opener."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil)
          (depth 0))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-p (point)))
            (cond
             ((lean4-indent--starts-with-p text lean4-indent--re-starts-end)
              (setq depth (1+ depth)))
             ((or (lean4-indent--starts-with-p text lean4-indent--re-starts-namespace)
                  (lean4-indent--starts-with-p text lean4-indent--re-starts-section)
                  (lean4-indent--starts-with-p text lean4-indent--re-starts-public-section)
                  (lean4-indent--starts-with-p text lean4-indent--re-starts-mutual))
              (if (> depth 0)
                  (setq depth (1- depth))
                (setq found (lean4-indent--line-indent (point)))))))))
      (or found 0))))

(defun lean4-indent--prev-noncomment (start-pos)
  "Return position of previous nonblank, non-comment line before START-POS."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-p (point)))
            (setq found (point)))))
      found)))

(defun lean4-indent--inside-namespace-or-section-p (start-pos)
  "Return non-nil if START-POS is inside a namespace/section block."
  (save-excursion
    (goto-char start-pos)
    (let ((depth 0)
          (found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-p (point)))
            (cond
             ((lean4-indent--starts-with-p text lean4-indent--re-starts-end)
              (setq depth (1+ depth)))
             ((or (lean4-indent--starts-with-p text lean4-indent--re-starts-namespace)
                  (lean4-indent--starts-with-p text lean4-indent--re-starts-section)
                  (lean4-indent--starts-with-p text lean4-indent--re-starts-public-section)
                  (lean4-indent--starts-with-p text lean4-indent--re-starts-mutual))
              (if (> depth 0)
                  (setq depth (1- depth))
                (setq found t)))))))
      found)))

(defun lean4-indent--find-mutual-indent (start-pos)
  "Return the indentation of the nearest enclosing `mutual`, or nil."
  (let ((context (lean4-indent--region-line-context start-pos)))
    (if context
        (plist-get context :mutual-indent)
      (save-excursion
        (goto-char start-pos)
        (let ((found nil)
              (depth 0))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (let ((text (lean4-indent--line-text (point))))
              (unless (or (lean4-indent--line-blank-p text)
                          (lean4-indent--comment-line-p (point)))
                (cond
                 ((lean4-indent--starts-with-p text lean4-indent--re-starts-end)
                  (setq depth (1+ depth)))
                 ((lean4-indent--starts-with-p text lean4-indent--re-starts-mutual)
                  (if (> depth 0)
                      (setq depth (1- depth))
                    (setq found (lean4-indent--line-indent (point)))))))))
          found)))))

(defun lean4-indent--compute-indent ()
  "Compute indentation for current line."
  (let* ((step lean4-indent-offset)
         (current-text (lean4-indent--line-text (point)))
         (prev-pos (lean4-indent--prev-nonblank))
         (prev-text (if prev-pos (lean4-indent--line-text prev-pos) ""))
         (prev-text-no-comment (if (and prev-pos (not (lean4-indent--comment-line-p prev-pos)))
                                   (lean4-indent--line-text-no-comment prev-pos)
                                 ""))
         (prev-indent (if prev-pos (lean4-indent--line-indent prev-pos) 0))
         (current-trim (string-trim current-text))
         (prev-noncomment (and prev-pos (lean4-indent--prev-noncomment prev-pos)))
         (prev-noncomment-text (if prev-noncomment (lean4-indent--line-text prev-noncomment) ""))
         (prev-noncomment-indent (if prev-noncomment (lean4-indent--line-indent prev-noncomment) 0))
         (mutual-indent (lean4-indent--find-mutual-indent (point)))
         (anchor (and prev-pos (lean4-indent--find-anchor prev-pos prev-indent)))
         (anchor-pos (car-safe anchor))
         (anchor-text (if anchor-pos (lean4-indent--line-text anchor-pos) ""))
         (anchor-text-no-comment (if (and anchor-pos (not (lean4-indent--comment-line-p anchor-pos)))
                                     (lean4-indent--line-text-no-comment anchor-pos)
                                   ""))
         (anchor-indent (if anchor (cdr anchor) 0))
         (anchor-body-intro-kind (and anchor-pos
                                      (lean4-indent--line-body-intro-kind
                                       anchor-text-no-comment)))
         (anchor-term-body-indent
          (and anchor-body-intro-kind
               (lean4-indent--body-intro-indent anchor-body-intro-kind
                                                anchor-pos anchor-indent step)))
         (anchor-by-block-p (memq anchor-body-intro-kind '(by coloneq-by)))
         (anchor-starts-calc (lean4-indent--starts-with-p anchor-text lean4-indent--re-starts-calc))
         (prev-continuation-p (and anchor-pos (= prev-indent (+ anchor-indent (* 2 step)))))
         (anchor2 (and anchor-pos (lean4-indent--find-anchor anchor-pos anchor-indent)))
         (anchor2-indent (if anchor2 (cdr anchor2) 0))
         (anchor-continuation-p (and anchor2 (= anchor-indent (+ anchor2-indent (* 2 step)))))
         (prev-shallow-continuation-p
          (and anchor-pos
               anchor-continuation-p
               (= prev-indent (+ anchor-indent step))))
         (parent-indent (lean4-indent--anchor-parent-indent anchor-pos anchor-indent step))
         (prev-comment-p (and prev-pos (lean4-indent--comment-line-p prev-pos)))
         (current-comment-p (lean4-indent--comment-line-p (line-beginning-position)))
         (open-paren-pos (lean4-indent--open-paren-pos (point)))
         (open-paren-col (and open-paren-pos (save-excursion
                                               (goto-char open-paren-pos)
                                               (current-column))))
         (open-paren-char (and open-paren-pos (char-after open-paren-pos)))
         (open-paren-col-prev (and prev-pos (lean4-indent--open-paren-col-at-eol prev-pos)))
         (open-paren-pos-prev (and prev-pos (lean4-indent--open-paren-pos-at-eol prev-pos)))
         (open-paren-char-prev (and open-paren-pos-prev (char-after open-paren-pos-prev)))
         (prev-unmatched-brace-pos
          (and prev-pos (lean4-indent--last-unmatched-open-brace-pos prev-pos)))
         (prev-unmatched-brace-col
          (and prev-unmatched-brace-pos
               (save-excursion
                 (goto-char prev-unmatched-brace-pos)
                 (current-column))))
         (prev-unmatched-brace-body-indent
          (and prev-unmatched-brace-pos prev-unmatched-brace-col
               (lean4-indent--open-delimited-body-indent
                prev-unmatched-brace-pos prev-unmatched-brace-col step)))
         (starts-with-branch (lean4-indent--branch-line-p current-text))
         (starts-with-focus (lean4-indent--focus-dot-line-p current-text))
         (current-is-underscore-paren (string= current-trim "_)"))
         (starts-with-paren (or (lean4-indent--line-starts-with-paren-p current-text)
                                current-is-underscore-paren))
         (starts-with-closing (lean4-indent--line-starts-with-closing-p current-text))
         (current-body-intro-kind
          (lean4-indent--line-body-intro-kind
           (lean4-indent--line-text-no-comment (point))))
         (current-plain-body-line-p
          (and (not starts-with-paren)
               (not starts-with-branch)
               (not starts-with-focus)
               (not starts-with-closing)))
         (prev-starts-with-paren (lean4-indent--line-starts-with-paren-p prev-text))
         (prev-starts-with-focus (lean4-indent--focus-dot-line-p prev-text))
         (prev-starts-with-calc-step (lean4-indent--line-starts-with-calc-step-p prev-text))
         (prev-body-intro-kind (lean4-indent--line-body-intro-kind prev-text-no-comment))
         (prev-term-continuation-p
          (and (lean4-indent--line-ends-with-term-continuation-p prev-text-no-comment)
               (not (lean4-indent--line-ends-with-at-star-p prev-text-no-comment))))
         (prev-line-ends-with-op (lean4-indent--line-ends-with-op-p prev-text-no-comment))
         (prev-line-ends-with-comma (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
         (prev-line-starts-with-let (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-let))
         (prev-line-has-outer-coloneq (and prev-pos
                                           (lean4-indent--line-has-outer-coloneq-p prev-pos)))
         (prevprev-pos (and prev-pos (save-excursion
                                       (goto-char prev-pos)
                                       (lean4-indent--prev-nonblank))))
         (prevprev-text (if prevprev-pos (lean4-indent--line-text prevprev-pos) ""))
         (prevprev-indent (if prevprev-pos (lean4-indent--line-indent prevprev-pos) 0))
         (prevprev-body-intro-kind
          (and prevprev-pos
               (lean4-indent--line-body-intro-kind
                (lean4-indent--line-text-no-comment prevprev-pos))))
         (prev-label-colon (lean4-indent--label-colon-line-p prev-text))
         (prev-unmatched-angle (and prev-pos (lean4-indent--line-unmatched-angle-p prev-pos)))
         (prev-ends-with-brace (and prev-pos (string-match-p "[)}⟩]\\s-*$" prev-text)))
         (prev-ends-with-bracket (and prev-pos (string-match-p "\\]\\s-*$" prev-text)))
         (prev-closes-bracket-with-at (and prev-pos (string-match-p "\\]\\s-+at\\_>" prev-text)))
         (prev-closes-paren (and prev-pos (lean4-indent--line-closes-paren-p prev-pos)))
         (prev-starts-with-paren-closed
          (and prev-pos (lean4-indent--line-starts-with-paren-and-closes-p prev-pos)))
         (top-level-context (and prev-pos
                                 (lean4-indent--top-level-context prev-pos step)))
         (top-level-body-intro-pos
          (plist-get top-level-context :body-intro-pos))
         (top-level-body-intro-kind
          (plist-get top-level-context :body-intro-kind))
         (nested-by-candidate-p
          (and prev-pos
               anchor-pos
               (> anchor-indent 0)
               (= prev-indent (+ anchor-indent step))
               anchor-by-block-p))
         (prev-calc-body-indent (lean4-indent--calc-block-body-indent prev-pos prev-indent step))
         (prev-top-level-body-indent (plist-get top-level-context :body-indent))
         (prev-have-suffices-p (string-match-p lean4-indent--re-have-suffices prev-text))
         (prev-coloneq-by-top-level-body-indent
          (and prev-top-level-body-indent
               (eq prev-pos top-level-body-intro-pos)
               (not prev-have-suffices-p)
               (not (and prev-pos
                         (lean4-indent--prev-have-suffices-p prev-pos prev-indent)))
               prev-top-level-body-indent))
         (prev-coloneq-top-level-body-indent
          (and (eq prev-pos top-level-body-intro-pos)
               (not prev-have-suffices-p)
               (not prev-starts-with-calc-step)
               (not anchor-starts-calc)
               (not (eq top-level-body-intro-kind 'coloneq-by))
               (not (eq anchor-body-intro-kind 'where))
               (or (and anchor-pos
                        (lean4-indent--line-top-level-anchor-p anchor-text)
                        (+ anchor-indent step))
                   prev-top-level-body-indent))))
    (cond
     ;; 0) `end` lines align with their opener.
     ((lean4-indent--starts-with-p current-text "\\_<end\\_>")
      (if prev-pos
          (lean4-indent--find-end-anchor-indent (point))
        0))
     ;; 1) Keep comment indentation as written.
     (current-comment-p
      (current-indentation))
     ;; 1.5) Lines starting with := continue previous field alignment.
     ((lean4-indent--starts-with-p current-text ":=")
      prev-indent)
     ;; 1.75) A `let ...;` line continues the let-chain/body at the same indent.
     ((and prev-line-starts-with-let
           (lean4-indent--line-ends-with-semicolon-p prev-text-no-comment)
           (not starts-with-branch)
           (not starts-with-focus))
      prev-indent)
     ;; 2) Branch lines
     (starts-with-branch
      (let* ((with-indent (and prev-pos (lean4-indent--find-with-indent
                                         prev-pos
                                         (or anchor2-indent anchor-indent prev-indent)))))
        (cond
         ((and prev-pos (lean4-indent--macro-rules-line-p prev-text))
          (+ prev-indent step))
         ((and prev-noncomment (lean4-indent--macro-rules-line-p prev-noncomment-text))
          (+ prev-noncomment-indent step))
         ((eq prev-body-intro-kind 'where)
          (+ prev-indent step))
         (with-indent with-indent)
         (t prev-indent))))
     ;; `mutual` bodies indent one level.
     ((and prev-pos
           (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-mutual))
      (+ prev-indent step))
     ;; 2.5) Top-level anchors inside mutual indent one step.
     ((and mutual-indent (lean4-indent--line-top-level-anchor-p current-text))
      (+ mutual-indent step))
     ;; 3) Top-level snap
     ((lean4-indent--line-top-level-anchor-p current-text)
      0)
     ;; 3.5) `where` aligns with its declaration anchor.
     ((and (lean4-indent--starts-with-p current-text lean4-indent--re-starts-where) anchor-pos)
      anchor-indent)
     ;; Continuation of wrapped top-level declaration/variable binders.
     ((and prev-pos
           (lean4-indent--line-top-level-binder-head-kind prev-text)
           (not prev-line-has-outer-coloneq)
           (not (memq prev-body-intro-kind '(colon coloneq coloneq-by by where))))
      (+ prev-indent
         (pcase (lean4-indent--line-top-level-binder-head-kind prev-text)
           ('variable step)
           (_ (* 2 step)))))
     ;; Continuation of a wrapped declaration statement across relation lines.
     ((and anchor-pos
           (lean4-indent--line-top-level-declaration-head-p anchor-text)
           (not prev-line-has-outer-coloneq)
           (lean4-indent--line-starts-with-relop-p current-text)
           (= prev-indent (+ anchor-indent (* 2 step))))
      (+ prev-indent (* 2 step)))
     ;; Continuation after type-ascription without := or trailing colon
     ((and (lean4-indent--colon-before-paren-p prev-pos)
           (not (memq prev-body-intro-kind '(colon coloneq coloneq-by)))
           (not (string-match-p lean4-indent--re-from prev-text))
           starts-with-paren)
      (+ prev-indent (* 2 step)))
     ;; 4) After colon lines
     ((eq prev-body-intro-kind 'colon)
      (cond
       (prev-continuation-p prev-indent)
       ((or prev-label-colon
            (string-match-p "\\`[ \t]*\\_<\\(?:have\\|suffices\\|let\\)\\_>"
                            prev-text-no-comment))
        (+ prev-indent step))
       (t (+ prev-indent (* 2 step)))))
     ;; 4.25) Local let/have lines with an inline := term keep continuation aligned.
     ((and (eq prev-body-intro-kind 'coloneq)
           (string-match-p
            "\\`[ \t]*\\(?:let\\|have\\)\\_>.*:=\\s-*\\S-+"
            prev-text-no-comment)
           (not (lean4-indent--starts-with-p current-text ":=")))
      prev-indent)
     ;; 4.5) Focus-dot lines that open a block should indent one step.
     ((and prev-starts-with-focus
           (memq prev-body-intro-kind '(calc by coloneq-by coloneq)))
      (+ prev-indent step))
     ;; 5-7) Lines following a body-introducing previous line.
     ((memq prev-body-intro-kind
            '(bare-have-suffices coloneq-by coloneq by fat-arrow fun-arrow do
              then else calc equals termination decreasing open-brace where))
      (pcase prev-body-intro-kind
        ('bare-have-suffices
         (+ prev-indent step))
        ('where
         (or prev-top-level-body-indent
             (if (and anchor-pos
                      (lean4-indent--line-top-level-declaration-head-p anchor-text))
                 (+ anchor-indent step)
               (+ prev-indent step))))
        ('coloneq-by
         (cond
          (prev-calc-body-indent)
          (anchor-starts-calc
           (+ prev-indent step))
          ;; If this := by closes a declaration, indent relative to that declaration line.
          (prev-coloneq-by-top-level-body-indent)
          ((or prev-continuation-p prev-shallow-continuation-p) parent-indent)
          (prev-starts-with-paren
           (if (and anchor
                    (>= (- prev-indent anchor-indent) (* 3 step)))
               anchor-indent
             parent-indent))
          (t (+ prev-indent step))))
        ('coloneq
         (if (lean4-indent--starts-with-p current-text ":=")
             prev-indent
           (cond
            ((and prev-unmatched-brace-pos
                  (string-match-p ":=\\s-*$" prev-text-no-comment)
                  current-plain-body-line-p)
             (+ prev-indent (* 2 step)))
            ((and prev-top-level-body-indent
                  (= prev-pos top-level-body-intro-pos)
                  (> prev-indent prev-top-level-body-indent)
                  (not (lean4-indent--line-blank-p current-text)))
             prev-top-level-body-indent)
            (prev-coloneq-top-level-body-indent)
            ((and prev-starts-with-paren
                  (not (lean4-indent--line-starts-with-paren-and-closes-p prev-pos)))
             (+ prev-indent step))
            (prev-calc-body-indent)
            (prev-starts-with-calc-step
             (+ prev-indent step))
            (anchor-starts-calc
             (+ anchor-indent step))
            (prev-continuation-p
             (max 0 (- prev-indent step)))
            (t (+ prev-indent step)))))
        ('equals
         (if (and prev-continuation-p
                  anchor-pos
                  (string-match-p lean4-indent--re-have-suffices
                                  (lean4-indent--line-text anchor-pos)))
             prev-indent
           (if (and prev-pos (lean4-indent--in-calc-block-p prev-pos))
               (+ prev-indent (* 2 step))
             (+ prev-indent step))))
        ('by
         (cond
          ((and anchor-pos
                (lean4-indent--show-body-column anchor-text-no-comment)
                (= prev-indent
                   (lean4-indent--show-body-column anchor-text-no-comment)))
           (+ anchor-indent step))
          ((string-match-p lean4-indent--re-fun-by prev-text)
           (if (< parent-indent prev-indent)
               parent-indent
             (+ prev-indent step)))
          (t
           (+ prev-indent step))))
        ('fun-arrow
         (cond
          ((string-match-p lean4-indent--re-classical-exact-fun prev-text)
           (+ prev-indent (* 2 step)))
          ((and (lean4-indent--line-starts-with-fun-form-p prev-text)
                (string-match-p "\\`[ \t]*\\_<\\(?:have\\|suffices\\)\\_>"
                                anchor-text-no-comment)
                (= prev-indent (+ anchor-indent step))
                (not (lean4-indent--line-blank-p current-text)))
           prev-indent)
          ((and (lean4-indent--line-starts-with-fun-form-p prev-text)
                prev-top-level-body-indent
                (= prev-indent prev-top-level-body-indent)
                (memq top-level-body-intro-kind '(coloneq coloneq-by))
                (not (lean4-indent--line-blank-p current-text)))
           prev-indent)
          ((and anchor-pos
                (lean4-indent--line-top-level-declaration-head-p anchor-text)
                (= prev-indent (+ anchor-indent (* 2 step)))
                (not (lean4-indent--line-blank-p current-text)))
           parent-indent)
          ((and (lean4-indent--line-starts-with-fun-form-p prev-text)
                anchor-pos
                (= prev-indent (+ anchor-indent step))
                (eq anchor-body-intro-kind 'fun-arrow)
                (lean4-indent--line-starts-with-fun-form-p anchor-text)
                (not (lean4-indent--line-blank-p current-text)))
           prev-indent)
          ((and (lean4-indent--line-starts-with-fun-form-p prev-text)
                (lean4-indent--line-blank-p current-text))
           prev-indent)
          (t (+ prev-indent step))))
        ('fat-arrow
         (cond
          ((and (string-match-p "\\`[ \t]*\\_<\\(?:have\\|suffices\\)\\_>"
                                anchor-text-no-comment)
                (= prev-indent (+ anchor-indent step))
                (not (lean4-indent--line-blank-p current-text)))
           prev-indent)
          ((and prev-top-level-body-indent
                (lean4-indent--line-starts-with-fun-form-p prev-text)
                (= prev-indent prev-top-level-body-indent)
                (memq top-level-body-intro-kind '(coloneq coloneq-by))
                (not (lean4-indent--line-blank-p current-text)))
           prev-indent)
          (t (+ prev-indent step))))
        (_
         (+ prev-indent step))))
     ;; 7.4) Structure field bodies and siblings inside an open `{ ... }` literal.
     ((and prev-pos
           current-plain-body-line-p
           prev-unmatched-brace-body-indent
           (string-match-p ":=" prev-text-no-comment))
      (if (string-match-p ":=\\s-*$" prev-text-no-comment)
          (+ prev-unmatched-brace-body-indent step)
        (if (or (lean4-indent--line-blank-p current-text)
                (string-match-p ":=" (lean4-indent--line-text-no-comment (point))))
            prev-unmatched-brace-body-indent
          (+ prev-unmatched-brace-body-indent step))))
     ;; 7.5) Continue a simple head term when an enclosing anchor already expects one term.
     ((and prev-top-level-body-indent
           (eq top-level-body-intro-kind 'colon)
           (= prev-indent (+ prev-top-level-body-indent step))
           (memq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 '(atom application))
           starts-with-paren)
      (+ prev-indent step))
     ((and anchor-term-body-indent
           (= prev-indent anchor-term-body-indent)
           (memq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 '(atom application))
           (not prev-closes-paren)
           (not (and starts-with-paren
                     prev-pos
                     prev-closes-paren))
           (lean4-indent--line-starts-structured-term-p current-text))
      (+ prev-indent step))
     ;; 7.55) Continue sibling parenthesized arguments after an inline closed argument.
     ((and starts-with-paren
           (memq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 '(application))
           (string-match-p "[])}⟩]\\s-*$" prev-text)
           (not prev-closes-paren)
           (not prev-line-ends-with-comma)
           (not prev-line-ends-with-op)
           (not prev-term-continuation-p))
      (+ prev-indent step))
     ;; 7.6) Continue the multiline term argument of an `exact`/`refine`/`apply` tactic.
     ((and (or anchor-by-block-p
               (and prev-top-level-body-indent
                    (= prev-indent prev-top-level-body-indent)))
           (memq (lean4-indent--tactic-term-tail-head-kind prev-text-no-comment)
                 '(atom application))
           (lean4-indent--line-starts-structured-term-p current-text))
      (+ prev-indent step))
     ;; 7.65) Bare `refine`/`exact`/`apply` start a multiline tactic term.
     ((and (or anchor-by-block-p
               (and prev-top-level-body-indent
                    (= prev-indent prev-top-level-body-indent)))
           (lean4-indent--bare-tactic-term-intro-line-p prev-text-no-comment)
           (not (lean4-indent--line-blank-p current-text)))
      (+ prev-indent step))
     ;; 7.7) Continue the multiline term introduced after a bare tactic line.
     ((and anchor-pos
           (= prev-indent (+ anchor-indent step))
           (lean4-indent--bare-tactic-term-intro-line-p anchor-text-no-comment)
           (memq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 '(atom application))
           (lean4-indent--line-starts-structured-term-p current-text))
      (+ prev-indent step))
     ;; 7.8) Local let/have values that started inline keep their continuation column.
     ((and (string-match-p
            "\\`[ \t]*\\(?:let\\|have\\)\\_>.*:=\\s-*\\S-+"
            prev-text-no-comment)
           (not starts-with-branch)
           (not starts-with-focus)
           (not (string-match-p ":=\\s-*{" prev-text-no-comment))
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma))
      (if (and starts-with-paren
               anchor-pos
               (memq anchor-body-intro-kind '(fat-arrow fun-arrow by coloneq-by))
               (string-match-p "\\`[ \t]*have\\_>" prev-text-no-comment))
          prev-indent
        (if (string-match-p "\\`[ \t]*have\\_>" prev-text-no-comment)
            (+ prev-indent step)
          prev-indent)))
     ;; 8) Anonymous literal ⟨…⟩
     ((and prev-unmatched-angle
           starts-with-paren
           (lean4-indent--line-leading-angle-paren-col prev-text))
      (lean4-indent--line-leading-angle-paren-col prev-text))
     ((and prev-unmatched-angle (not starts-with-closing))
      prev-indent)
     ;; 8.25) Lines inside ⟨ ... ⟩ literals
     ((and open-paren-col
           (eq open-paren-char lean4-indent--char-left-angle)
           current-plain-body-line-p
           (not (lean4-indent--starts-with-p current-text "⟩")))
      (lean4-indent--open-delimited-body-indent open-paren-pos open-paren-col step))
     ;; 8.5) Lines inside { ... } structure literals
     ((and open-paren-col
           (eq open-paren-char ?{)
           (not anchor-by-block-p)
           current-plain-body-line-p
           (not (lean4-indent--starts-with-p current-text "}")))
      (lean4-indent--open-delimited-body-indent open-paren-pos open-paren-col step))
     ;; 8.75) Closing delimiters align with opener
     ((and starts-with-closing open-paren-col)
      open-paren-col)
     ;; 9) Paren-led lines
     (starts-with-paren
      (let* ((leading-parens (lean4-indent--leading-paren-count prev-text))
             (first-non-paren-col (+ prev-indent leading-parens))
             (last-unmatched-col (and prev-pos (lean4-indent--last-unmatched-open-paren-col prev-pos)))
             (paren-scan (and prev-pos
                              (lean4-indent--scan-prev-paren-lines
                               prev-pos prev-indent (- prev-indent (* 2 step)))))
             (prev-paren-block-min-indent (plist-get paren-scan :block-min-indent))
             (prev-paren-sibling-indent (plist-get paren-scan :sibling-indent))
             (prev-paren-dedent-indent (plist-get paren-scan :dedent-indent)))
        (cond
         (prev-term-continuation-p
          (+ prev-indent step))
         (current-is-underscore-paren
          (or prev-paren-dedent-indent
              prev-paren-block-min-indent
              prev-paren-sibling-indent
              last-unmatched-col
              open-paren-col
              prev-indent))
         ((and prev-pos
               anchor-pos
               prev-closes-paren
               (lean4-indent--line-starts-with-paren-p anchor-text))
          anchor-indent)
         ((and (not open-paren-col)
               prev-pos
               prev-closes-paren
               (not prev-line-ends-with-comma))
          (or prev-paren-sibling-indent prev-indent))
         (prev-starts-with-paren-closed
          prev-indent)
         (prev-line-ends-with-comma
          (+ prev-indent step))
         ((>= leading-parens 2)
          first-non-paren-col)
         ((string-suffix-p "/" (string-trim-right prev-text))
          (+ prev-indent step))
         (prev-line-ends-with-op
          (+ prev-indent step))
         (last-unmatched-col
          (+ last-unmatched-col step))
         (open-paren-col
          (+ open-paren-col step))
         (t prev-indent))))
     ;; 10) Calc-step operator continuation (_ = ... *)
     ((and prev-line-ends-with-op
           (lean4-indent--line-starts-with-calc-step-p prev-text)
           (not prev-term-continuation-p))
      prev-indent)
     ;; 10.25) Term continuation operators (<;>, ;)
     (prev-term-continuation-p
      (if (and prev-pos (lean4-indent--line-starts-with-calc-step-p prev-text))
          (+ prev-indent (* 2 step))
        (+ prev-indent step)))
     ;; 10.4) Local let/have values that already started inline keep their continuation column.
     ((and (string-match-p
            "\\`[ \t]*\\(?:let\\|have\\)\\_>.*:=\\s-*\\S-+"
            prev-text-no-comment)
           (not starts-with-branch)
           (not starts-with-focus)
           (not (string-match-p ":=\\s-*{" prev-text-no-comment))
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma))
      (if (and starts-with-paren
               anchor-pos
               (memq anchor-body-intro-kind '(fat-arrow fun-arrow by coloneq-by))
               (string-match-p "\\`[ \t]*have\\_>" prev-text-no-comment))
          prev-indent
        (if (string-match-p "\\`[ \t]*have\\_>" prev-text-no-comment)
            (+ prev-indent step)
          prev-indent)))
     ;; 10.6) Continuation of a multiline `show` proposition aligns under `show`.
     ((and prev-pos
           (lean4-indent--show-body-column prev-text-no-comment)
           (lean4-indent--line-ends-with-op-p prev-text-no-comment))
      (lean4-indent--show-body-column prev-text-no-comment))
     ;; 10.5) Operator continuation
     ((lean4-indent--operator-continuation-p prev-text-no-comment)
      prev-indent)
     ;; 10.75) Arguments continue after an open paren on the previous line.
     ((and prev-pos
           open-paren-col-prev
           (lean4-indent--line-opens-paren-p prev-pos)
           current-plain-body-line-p)
      (+ prev-indent step))
     ;; 11) Focus dots
     (prev-starts-with-focus
      (+ prev-indent step))
     ;; 11.2) Continue multi-line bracketed lists started on previous line.
     ((and prev-pos
           prev-line-ends-with-comma
           open-paren-char-prev
           (memq open-paren-char-prev (list ?\( ?\[ lean4-indent--char-left-angle))
           current-plain-body-line-p)
      (+ prev-indent step))
     ;; 11.25) Dedent after closing a paren/bracket/brace line.
     ((and prev-pos
           anchor-pos
           (not open-paren-col)
           current-plain-body-line-p
           (not (string-match-p lean4-indent--re-ends-angle-close prev-text))
           (or (lean4-indent--line-starts-with-closing-p prev-text)
               prev-ends-with-brace
               prev-closes-bracket-with-at
               (and prev-ends-with-bracket
                    (not (string-match-p "\\]\\s-*$" current-text))))
           prev-closes-paren)
      (let* ((matching-open-pos
              (and prev-ends-with-brace
                   (lean4-indent--matching-open-delimiter-pos-at-eol prev-pos)))
             (matching-open-is-brace
              (and matching-open-pos
                   (eq (char-after matching-open-pos) ?{)))
             (matching-open-line-pos
              (and matching-open-is-brace
                   (save-excursion
                     (goto-char matching-open-pos)
                     (line-beginning-position))))
             (matching-open-line-indent
              (and matching-open-line-pos
                   (lean4-indent--line-indent matching-open-line-pos)))
             (matching-open-parent
              (and matching-open-line-pos
                   (lean4-indent--find-anchor
                    matching-open-line-pos matching-open-line-indent)))
             (anchor-text (lean4-indent--line-text anchor-pos))
             (anchor-parent (lean4-indent--find-anchor anchor-pos anchor-indent)))
        (cond
         (matching-open-parent
          (cdr matching-open-parent))
         ((lean4-indent--starts-with-p anchor-text "{")
          (if anchor-parent (cdr anchor-parent) 0))
         (t
          anchor-indent))))
     ;; 11.3) Dedent after a parenthesized continuation line belonging to inline let/have.
     ((and prev-pos
           anchor-pos
           prev-starts-with-paren-closed
           (string-match-p
            "\\`[ \t]*\\(?:let\\|have\\)\\_>.*:=\\s-*\\S-+"
            anchor-text-no-comment))
      anchor-indent)
     ;; 11.35) After a local `have`/`suffices := fun ... =>` body, dedent to the sibling column.
     ((and anchor-pos
           (string-match-p "\\`[ \t]*\\_<\\(?:have\\|suffices\\)\\_>"
                           anchor-text-no-comment)
           (= prev-indent (+ anchor-indent step))
           (= prevprev-indent prev-indent)
           (eq prevprev-body-intro-kind 'fat-arrow)
           current-plain-body-line-p
           (not prev-body-intro-kind)
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma))
      anchor-indent)
     ;; 11.4) Dedent after exact before a simp-like line in nested by blocks.
     ((and nested-by-candidate-p
           (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-exact)
           (lean4-indent--starts-with-p current-text lean4-indent--re-starts-simp-like))
      anchor-indent)
     ;; 11.5) Dedent after nested `by` blocks that close on the previous line.
     ((and nested-by-candidate-p
           (let ((prev-simp (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-simp-like))
                 (prevprev-simp (lean4-indent--starts-with-p prevprev-text lean4-indent--re-starts-simp-like)))
             (or prev-simp (and prevprev-simp prev-closes-paren)))
           (not (lean4-indent--line-blank-p current-text))
           current-plain-body-line-p
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma)
           (not (memq prev-body-intro-kind '(colon coloneq coloneq-by by)))
           (or prev-closes-paren
               (lean4-indent--line-contains-balanced-bracket-p prev-pos)))
      anchor-indent)
     ;; 11.75) Blank lines inside tactic blocks keep the surrounding tactic indent.
     ((and (lean4-indent--line-blank-p current-text)
           anchor-by-block-p
           (> prev-indent anchor-indent)
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma)
           (not prev-body-intro-kind))
      prev-indent)
     ;; 11.8) Blank lines after a first-level non-tactic declaration body snap back.
     ((and (lean4-indent--line-blank-p current-text)
           anchor-pos
           (lean4-indent--line-top-level-anchor-p anchor-text)
           (eq anchor-body-intro-kind 'coloneq)
           (= prev-indent (+ anchor-indent step))
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma)
           (not prev-body-intro-kind))
      anchor-indent)
     ;; 12) Fallback
     (t
      (let ((fallback-indent (if (and prev-noncomment (string-empty-p prev-text-no-comment))
                                 (if (> prev-indent 0) prev-indent prev-noncomment-indent)
                               prev-indent)))
        (if (lean4-indent--line-is-bare-sorry-p prev-text-no-comment)
            (max 0 (- fallback-indent step))
          fallback-indent))))))

(defun lean4-indent--cycle-indent (computed current)
  "Cycle indentation for repeated TAB presses."
  (let ((step lean4-indent-offset))
    (if (> current 0)
        (let ((next (- current step)))
          (if (< next 0) computed next))
      computed)))

(defun lean4-indent--acceptable-tactic-indent-p (computed current)
  "Return non-nil if CURRENT is acceptable inside a tactic block.

This is intentionally permissive only for non-TAB reindentation: any
indent at or beyond the enclosing `by`/`:= by` anchor is accepted.
Outside tactic blocks this returns nil."
  (let* ((current-text (lean4-indent--line-text (point)))
         (current-trim (string-trim current-text))
         (current-tactic-body-line-p
          (and (not (string-empty-p current-trim))
               (not (lean4-indent--line-top-level-anchor-p current-text))))
         (prev-pos (lean4-indent--prev-nonblank))
         (prev-indent (if prev-pos (lean4-indent--line-indent prev-pos) 0))
         (step lean4-indent-offset)
         (top-level-context (and prev-pos
                                 (lean4-indent--top-level-context prev-pos step)))
         (top-body-intro-kind (plist-get top-level-context :body-intro-kind))
         (anchor
          (lean4-indent--find-enclosing-body-intro-anchor
           prev-pos prev-indent '(by coloneq-by)))
         (anchor-pos (car-safe anchor))
         (anchor-indent (if anchor (cdr anchor) 0))
         (prev-top-level-body-indent (plist-get top-level-context :body-indent)))
    (or (and current-tactic-body-line-p
             anchor-pos
             (> current 0)
             (<= anchor-indent current))
        (and current-tactic-body-line-p
             (eq top-body-intro-kind 'coloneq-by)
             prev-top-level-body-indent
             (> current 0)
             (<= prev-top-level-body-indent current))
        (and current-tactic-body-line-p
             (lean4-indent--focus-dot-line-p current-text)
             prev-top-level-body-indent
             (= current prev-top-level-body-indent)))))

(defun lean4-indent--acceptable-region-body-indent-p (computed current)
  "Return non-nil if CURRENT should be preserved during region indentation."
  (let* ((prev-pos (lean4-indent--prev-nonblank))
         (step lean4-indent-offset)
         (top-level-context (and prev-pos
                                 (lean4-indent--top-level-context prev-pos step)))
         (body-indent (plist-get top-level-context :body-indent))
         (current-text (lean4-indent--line-text (point))))
    (and lean4-indent--preserve-tactic-region-indentation
         (> current 0)
         (not (lean4-indent--line-blank-p current-text))
         (not (lean4-indent--line-top-level-anchor-p current-text))
         body-indent
         (>= current body-indent))))

(defun lean4-indent-line-function ()
  "Indent current line according to Lean 4 rules."
  (interactive)
  (let* ((computed (lean4-indent--compute-indent))
         (current (current-indentation))
         (tabp (eq this-command 'indent-for-tab-command))
         (target (cond
                 ((and tabp (eq last-command 'indent-for-tab-command))
                   (lean4-indent--cycle-indent computed current))
                 ((and (not tabp)
                        (> current 0)
                        (or (and (lean4-indent--acceptable-tactic-indent-p computed current)
                                 (or (< current computed)
                                     lean4-indent--preserve-tactic-region-indentation))
                            (lean4-indent--acceptable-region-body-indent-p computed current)))
                   current)
                  (t computed)))
         (eolp (eolp)))
    (indent-line-to (max 0 target))
    (when eolp
      (end-of-line))))

(defun lean4-indent-region-function (start end)
  "Indent nonblank lines between START and END using Lean 4 rules.

This is a region-aware companion to `lean4-indent-line-function'.  It uses a
single forward pass to cache enclosing top-level declaration context, avoiding
repeated whole-declaration rescans near the end of large files."
  (let ((end-marker (copy-marker end))
        (lean4-indent--region-top-level-contexts
         (lean4-indent--build-top-level-context-cache lean4-indent-offset))
        (lean4-indent--region-line-contexts
         (lean4-indent--build-region-line-context-cache))
        (lean4-indent--preserve-tactic-region-indentation t))
    (save-excursion
      (goto-char start)
      (unless (bolp)
        (forward-line 1))
      (while (< (point) end-marker)
        (unless (or (looking-at-p "[ \t]*$")
                    (lean4-indent--comment-line-p (point))
                    (and (= (current-indentation) 0)
                         (lean4-indent--line-top-level-anchor-p
                          (lean4-indent--line-text (point)))))
          (funcall indent-line-function))
        (forward-line 1)))))

(provide 'lean4-indent)
;;; lean4-indent.el ends here
