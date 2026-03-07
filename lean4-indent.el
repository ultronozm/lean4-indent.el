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
;; To use it, set `indent-line-function' buffer-locally in a `lean4-mode-hook':
;;
;;   (add-hook 'lean4-mode-hook
;;             (lambda ()
;;               (setq-local indent-line-function
;;                           #'lean4-indent-line-function)))
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
(require 'seq)

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

(defconst lean4-indent--ops
  '("+" "-" "*" "/" "•" "≤" "≥" "≠" "∧" "∨" "↔" "→" "↦" "<;>"
    "<=" ">=" "=>" "->" "≅" "≃")
  "Operators that trigger line continuation when they end a line.")

(defconst lean4-indent--simp-like-keywords
  '("simp" "simp_rw" "simp?" "simpa" "rwa")
  "Keywords treated as simp-like tactics.")

(defconst lean4-indent--top-level-anchors
  '("attribute" "compile_inductive" "def" "instance" "partial_fixpoint"
    "theorem" "lemma" "example" "structure" "inductive" "class" "abbrev"
    "macro" "syntax" "notation" "set_option" "open" "universe" "@["
    "namespace" "section" "public section")
  "Top-level anchors that snap to column 0 when not nested.")

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
(defconst lean4-indent--re-starts-branch "|\\(?:[^>|]\\|$\\)")
(defconst lean4-indent--re-starts-focus "·")
(defconst lean4-indent--re-starts-namespace "namespace\\_>")
(defconst lean4-indent--re-starts-section "section\\_>")
(defconst lean4-indent--re-starts-public-section "public[ \\t]+section\\_>")
(defconst lean4-indent--re-starts-mutual "mutual\\_>")
(defconst lean4-indent--re-starts-end "\\_<end\\_>")
(defconst lean4-indent--re-starts-where "\\_<where\\_>")
(defconst lean4-indent--re-starts-fun "\\_<fun\\_>")
(defconst lean4-indent--re-starts-let "\\_<let\\_>")
(defconst lean4-indent--re-starts-calc "\\_<calc\\_>")
(defconst lean4-indent--re-classical-exact-fun
  "\\_<classical\\_>\\s-+\\_<exact\\_>\\s-+\\_<fun\\_>")
(defconst lean4-indent--re-starts-decl
  "\\_<\\(theorem\\|lemma\\|example\\|inductive\\|class\\|abbrev\\|macro\\|syntax\\|notation\\|set_option\\|open\\|universe\\)\\_>")
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
(defconst lean4-indent--re-angle-brackets "[⟨⟩]")
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

(defun lean4-indent--trim-left (text)
  "Trim leading whitespace from TEXT."
  (string-trim-left text))

(defun lean4-indent--trim-right (text)
  "Trim trailing whitespace from TEXT."
  (string-trim-right text))

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

(defun lean4-indent--inside-comment-p (pos)
  "Return non-nil if POS is inside a comment."
  (save-excursion
    (goto-char pos)
    (nth 4 (syntax-ppss))))

(defun lean4-indent--inside-string-p (pos)
  "Return non-nil if POS is inside a string."
  (save-excursion
    (goto-char pos)
    (nth 3 (syntax-ppss))))

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

(defun lean4-indent--open-paren-char (&optional pos)
  "Return character of innermost open paren for line at POS, or nil."
  (save-excursion
    (goto-char (or pos (point)))
    (back-to-indentation)
    (let* ((ppss (syntax-ppss))
           (open (nth 1 ppss)))
      (when open
        (char-after open)))))

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
            (when (string-match-p lean4-indent--re-starts-decl text)
              (throw 'found nil)))))
      nil)))

(defun lean4-indent--calc-rhs-start-col (pos)
  "Return column of first non-space after '=' on the line at POS, or nil."
  (save-excursion
    (goto-char pos)
    (let ((eol (line-end-position))
          (bol (line-beginning-position))
          (col nil))
      (while (and (not col) (search-forward "=" eol t))
        (let* ((eq-pos (point))
               (ppss (syntax-ppss (1- eq-pos)))
               (prev (char-before (1- eq-pos)))
               (next (char-after eq-pos)))
          (goto-char eq-pos)
          (when (and (not (nth 3 ppss)) (not (nth 4 ppss))
                     (not (memq prev '(?: ?< ?>)))
                     (not (memq next '(?= ?> ?<))))
            (skip-chars-forward " \t" eol)
            (setq col (string-width
                       (buffer-substring-no-properties bol (point)))))))
      col)))

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

(defun lean4-indent--line-ends-with-op-p (text)
  (string-match-p lean4-indent--ops-regexp (lean4-indent--trim-right text)))

(defun lean4-indent--operator-continuation-p (text)
  "Return non-nil if TEXT ends with an operator and does not trigger other indent rules."
  (and (lean4-indent--line-ends-with-op-p text)
       (not (lean4-indent--line-ends-with-colon-p text))
       (not (lean4-indent--line-ends-with-coloneq-p text))
       (not (lean4-indent--line-ends-with-coloneq-by-p text))
       (not (lean4-indent--line-ends-with-by-p text))
       (not (lean4-indent--line-ends-with-fat-arrow-p text))
       (not (lean4-indent--line-ends-with-fun-arrow-p text))
       (not (lean4-indent--line-ends-with-calc-p text))
       (not (lean4-indent--line-ends-with-where-p text))
       (not (lean4-indent--line-ends-with-equals-p text))))

(defun lean4-indent--line-label-colon-p (text)
  (let ((case-fold-search nil))
    (string-match-p lean4-indent--re-label-colon text)))

(defun lean4-indent--line-macro-rules-p (text)
  (string-match-p lean4-indent--re-macro-rules text))

(defun lean4-indent--line-top-level-anchor-p (text)
  (string-match-p lean4-indent--top-level-anchors-re text))

(defun lean4-indent--line-starts-with-paren-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-paren))

(defun lean4-indent--line-starts-with-branch-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-branch))

(defun lean4-indent--line-starts-with-focus-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-focus))

(defun lean4-indent--line-starts-with-where-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-where))

(defun lean4-indent--line-starts-with-let-p (text)
  (lean4-indent--starts-with-p text lean4-indent--re-starts-let))

(defun lean4-indent--simple-term-head-line-p (text)
  "Return non-nil if TEXT is a single identifier-like term head."
  (let ((trim (string-trim text)))
    (and (string-match-p "\\`[[:word:]_.']+\\'" trim)
         (not (string-match-p lean4-indent--top-level-anchors-re trim))
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-decl) trim))
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-fun) trim))
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-let) trim))
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-calc) trim))
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-where) trim))
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-end) trim)))))

(defun lean4-indent--plain-application-head-line-p (text)
  "Return non-nil if TEXT is a plain term application head."
  (let ((trim (string-trim text)))
    (and (string-match-p "\\`[[:word:]_.']+\\(?:\\s-+.+\\)\\'" trim)
         (not (string-match-p (concat "\\`" lean4-indent--re-starts-decl) trim))
         (not (string-match-p "\\`\\_<\\(?:fun\\|let\\|calc\\|where\\|end\\|if\\|match\\|do\\|have\\|suffices\\)\\_>" trim))
         (not (lean4-indent--line-ends-with-colon-p trim))
         (not (lean4-indent--line-ends-with-coloneq-p trim))
         (not (lean4-indent--line-ends-with-coloneq-by-p trim))
         (not (lean4-indent--line-ends-with-by-p trim))
         (not (lean4-indent--line-ends-with-fat-arrow-p trim))
         (not (lean4-indent--line-ends-with-fun-arrow-p trim))
         (not (lean4-indent--line-ends-with-do-p trim))
         (not (lean4-indent--line-ends-with-then-p trim))
         (not (lean4-indent--line-ends-with-else-p trim))
         (not (lean4-indent--line-ends-with-calc-p trim))
         (not (lean4-indent--line-ends-with-equals-p trim))
         (not (lean4-indent--line-ends-with-comma-p trim))
         (not (lean4-indent--line-ends-with-term-continuation-p trim))
         (not (lean4-indent--line-starts-with-branch-p trim))
         (not (lean4-indent--line-starts-with-focus-p trim))
         (not (lean4-indent--line-starts-with-closing-p trim)))))

(defun lean4-indent--term-body-intro-indent (pos text indent step)
  "Return indent of the first line in the term body introduced by TEXT.
POS is the position of the line containing TEXT, at indentation INDENT."
  (cond
   ((lean4-indent--line-ends-with-coloneq-p text)
    (+ indent step))
   ((or (lean4-indent--line-ends-with-fat-arrow-p text)
        (lean4-indent--line-ends-with-fun-arrow-p text)
        (lean4-indent--line-ends-with-then-p text)
        (lean4-indent--line-ends-with-else-p text))
    (+ indent step))
   ((lean4-indent--line-ends-with-equals-p text)
    (if (and pos (lean4-indent--in-calc-block-p pos))
        (+ indent (* 2 step))
      (+ indent step)))
   (t nil)))

(defun lean4-indent--term-continuation-line-p (text)
  "Return non-nil if TEXT starts a structured continuation of a term."
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

(defun lean4-indent--line-leading-angle-paren-col (text)
  "Return the column of the leading '(' in a line starting with `⟨(`, or nil."
  (when (string-match "\\`[ \t]*⟨(" text)
    (1- (match-end 0))))

(defun lean4-indent--branch-line-p (text)
  "Return non-nil if TEXT is a branch line."
  (lean4-indent--line-starts-with-branch-p text))

(defun lean4-indent--focus-dot-line-p (text)
  "Return non-nil if TEXT starts with a focus dot."
  (lean4-indent--line-starts-with-focus-p text))

(defun lean4-indent--macro-rules-line-p (text)
  "Return non-nil if TEXT is a macro_rules line."
  (lean4-indent--line-macro-rules-p text))

(defun lean4-indent--label-colon-line-p (text)
  "Return non-nil if TEXT is an all-caps label ending in a colon."
  (lean4-indent--line-label-colon-p text))

(defun lean4-indent--line-starts-with-simp-like-p (text)
  "Return non-nil if TEXT starts with a simp-like tactic keyword."
  (lean4-indent--starts-with-p text lean4-indent--re-starts-simp-like))

(defun lean4-indent--line-starts-with-exact-p (text)
  "Return non-nil if TEXT starts with exact."
  (lean4-indent--starts-with-p text lean4-indent--re-starts-exact))

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
               ((eq ch ?:) (setq found-colon t))
               ((eq ch ?\() (setq found-paren t)))))
          (forward-char 1))
        (and found-colon (not found-paren))))))

(defun lean4-indent--prev-paren-block-min-indent (start-pos)
  "Return minimum indent of consecutive previous lines starting with '('.
Stops at the first nonblank line that does not start with a paren."
  (save-excursion
    (goto-char start-pos)
    (let ((min-indent nil)
          (done nil))
      (while (and (not done) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (cond
           ((lean4-indent--line-blank-p text) nil)
           ((lean4-indent--line-starts-with-paren-p text)
            (let ((indent (lean4-indent--line-indent (point))))
              (setq min-indent (if min-indent (min min-indent indent) indent))))
           (t (setq done t)))))
      min-indent)))

(defun lean4-indent--find-prev-paren-dedent (start-pos max-indent)
  "Find the nearest previous line starting with '(' at or below MAX-INDENT."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (when (and (lean4-indent--line-starts-with-paren-p text)
                     (<= (lean4-indent--line-indent (point)) max-indent))
            (setq found (lean4-indent--line-indent (point))))))
      found)))

(defun lean4-indent--find-prev-paren-start-indent (start-pos prev-indent)
  "Find indent of a preceding line that starts with a paren and is less indented."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil))
      (while (not (bobp))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (when (and (lean4-indent--line-starts-with-paren-p text)
                     (< (lean4-indent--line-indent (point)) prev-indent))
            (let ((indent (lean4-indent--line-indent (point))))
              (when (or (not found) (< indent found))
                (setq found indent))))))
      found)))

;;; Anchor helpers
(defun lean4-indent--find-anchor (prev-pos prev-indent)
  "Find nearest prior nonblank line with indent < PREV-INDENT.
Return (anchor-pos . anchor-indent), or nil if none."
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
      found)))

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
      found)))

(defun lean4-indent--find-calc-step-indent (start-pos)
  "Return indentation of nearest calc step line above START-POS, or nil."
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
      found)))

(defun lean4-indent--find-top-level-anchor (start-pos)
  "Return position of nearest top-level anchor before START-POS, or nil."
  (save-excursion
    (goto-char start-pos)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point))))
          (unless (lean4-indent--comment-line-p (point))
            (when (or (lean4-indent--line-top-level-anchor-p text)
                      (lean4-indent--starts-with-p text lean4-indent--re-starts-decl))
              (setq found (point))))))
      found)))

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
      found)))

(defun lean4-indent--compute-indent ()
  "Compute indentation for current line."
  (let* ((step lean4-indent-offset)
         (current-text (lean4-indent--line-text (point)))
         (current-indent (current-indentation))
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
         (prev-text-op (if (string-empty-p prev-text-no-comment)
                           ""
                         prev-text-no-comment))
         (mutual-indent (lean4-indent--find-mutual-indent (point)))
         (anchor (and prev-pos (lean4-indent--find-anchor prev-pos prev-indent)))
         (anchor-pos (car-safe anchor))
         (anchor-text (if anchor-pos (lean4-indent--line-text anchor-pos) ""))
         (anchor-text-no-comment (if (and anchor-pos (not (lean4-indent--comment-line-p anchor-pos)))
                                     (lean4-indent--line-text-no-comment anchor-pos)
                                   ""))
         (anchor-indent (if anchor (cdr anchor) 0))
         (anchor-term-body-indent
          (and anchor-pos
               (lean4-indent--term-body-intro-indent anchor-pos anchor-text-no-comment
                                                     anchor-indent step)))
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
         (starts-with-branch (lean4-indent--branch-line-p current-text))
         (starts-with-focus (lean4-indent--focus-dot-line-p current-text))
         (current-is-underscore-paren (string= current-trim "_)"))
         (starts-with-paren (or (lean4-indent--line-starts-with-paren-p current-text)
                                current-is-underscore-paren))
         (starts-with-closing (lean4-indent--line-starts-with-closing-p current-text))
         (prev-starts-with-paren (lean4-indent--line-starts-with-paren-p prev-text))
         (prev-starts-with-focus (lean4-indent--focus-dot-line-p prev-text))
         (prev-line-ends-with-colon (lean4-indent--line-ends-with-colon-p prev-text-no-comment))
         (prev-line-ends-with-coloneq-by (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment))
         (prev-line-ends-with-coloneq (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment))
         (prev-line-ends-with-by (lean4-indent--line-ends-with-by-p prev-text-no-comment))
         (prev-line-ends-with-fat-arrow (lean4-indent--line-ends-with-fat-arrow-p prev-text-no-comment))
         (prev-line-ends-with-fun-arrow (lean4-indent--line-ends-with-fun-arrow-p prev-text-no-comment))
         (prev-line-ends-with-where (lean4-indent--line-ends-with-where-p prev-text-no-comment))
         (prev-line-ends-with-do (lean4-indent--line-ends-with-do-p prev-text-no-comment))
         (prev-line-ends-with-then (lean4-indent--line-ends-with-then-p prev-text-no-comment))
         (prev-line-ends-with-else (lean4-indent--line-ends-with-else-p prev-text-no-comment))
         (prev-line-ends-with-calc (lean4-indent--line-ends-with-calc-p prev-text-no-comment))
         (prev-line-ends-with-equals (lean4-indent--line-ends-with-equals-p prev-text-no-comment))
         (prev-line-ends-with-op (lean4-indent--line-ends-with-op-p prev-text-op))
         (prev-line-ends-with-comma (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
         (prev-line-starts-with-let (lean4-indent--line-starts-with-let-p prev-text))
         (prevprev-pos (and prev-pos (save-excursion
                                       (goto-char prev-pos)
                                       (lean4-indent--prev-nonblank))))
         (prevprev-text (if prevprev-pos (lean4-indent--line-text prevprev-pos) ""))
         (prev-label-colon (lean4-indent--label-colon-line-p prev-text))
         (prev-unmatched-angle (and prev-pos (lean4-indent--line-unmatched-angle-p prev-pos)))
         (prev-ends-with-brace (and prev-pos (string-match-p "[)}⟩]\\s-*$" prev-text)))
         (prev-ends-with-bracket (and prev-pos (string-match-p "\\]\\s-*$" prev-text)))
         (prev-closes-bracket-with-at (and prev-pos (string-match-p "\\]\\s-+at\\_>" prev-text)))
         (prev-starts-with-paren-closed
          (and prev-pos (lean4-indent--line-starts-with-paren-and-closes-p prev-pos))))
    (cond
     ;; 0) `end` lines align with their opener.
     ((lean4-indent--starts-with-p current-text "\\_<end\\_>")
      (if prev-pos
          (lean4-indent--find-end-anchor-indent (point))
        0))
     ;; 1) Comment continuation
     ((and prev-comment-p current-comment-p)
      prev-indent)
     ;; 1.5) Lines starting with := continue previous field alignment.
     ((lean4-indent--starts-with-p current-text ":=")
      prev-indent)
     ;; 1.75) A `let ...;` line continues the let-chain/body at the same indent.
     ((and prev-line-starts-with-let
           (lean4-indent--line-ends-with-semicolon-p prev-text-op)
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
         (prev-line-ends-with-where
          (+ prev-indent step))
         (with-indent with-indent)
         (t prev-indent))))
     ;; Namespace/section blocks keep one-level indent
     ((and prev-pos
           (or (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-namespace)
               (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-section)
               (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-public-section)
               (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-mutual)))
      (+ prev-indent step))
     ;; 2.5) Top-level anchors inside mutual indent one step.
     ((and mutual-indent (lean4-indent--line-top-level-anchor-p current-text))
      (+ mutual-indent step))
     ;; 3) Top-level snap
     ((and (lean4-indent--line-top-level-anchor-p current-text)
           (not (lean4-indent--inside-namespace-or-section-p (point))))
      0)
     ;; 3.5) `where` aligns with its declaration anchor.
     ((and (lean4-indent--line-starts-with-where-p current-text) anchor-pos)
      anchor-indent)
     ;; Continuation of top-level declaration binders before the colon
     ((and prev-pos
           (lean4-indent--line-top-level-anchor-p prev-text)
           (not prev-line-ends-with-colon)
           (not prev-line-ends-with-coloneq)
           (not prev-line-ends-with-coloneq-by)
           (not prev-line-ends-with-by)
           (not prev-line-ends-with-where))
      (+ prev-indent (* 2 step)))
     ;; Continuation of theorem/lemma/example binders before the colon
     ((and prev-pos
           (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-decl)
           (not prev-line-ends-with-colon)
           (not prev-line-ends-with-coloneq)
           (not prev-line-ends-with-coloneq-by)
           (not prev-line-ends-with-by)
           (not prev-line-ends-with-where))
      (+ prev-indent (* 2 step)))
     ;; Continuation after type-ascription without := or trailing colon
     ((and (lean4-indent--colon-before-paren-p prev-pos)
           (not prev-line-ends-with-colon)
           (not (string-match-p lean4-indent--re-coloneq prev-text))
           (not (string-match-p lean4-indent--re-from prev-text))
           starts-with-paren)
      (+ prev-indent (* 2 step)))
     ;; 4) After colon lines
     (prev-line-ends-with-colon
      (cond
       (prev-continuation-p prev-indent)
       (prev-label-colon (+ prev-indent step))
       (t (+ prev-indent (* 2 step)))))
     ;; 4.5) Focus-dot lines that open a block should indent one step.
     ((and prev-starts-with-focus
           (or prev-line-ends-with-calc
               prev-line-ends-with-by
               prev-line-ends-with-coloneq-by
               prev-line-ends-with-coloneq))
      (+ prev-indent step))
     ;; 5) After := by
     (prev-line-ends-with-coloneq-by
      (cond
       ((and prev-pos (lean4-indent--in-calc-block-p prev-pos))
        (let ((calc-indent (lean4-indent--find-calc-step-indent prev-pos)))
          (if calc-indent (+ calc-indent step) (+ prev-indent step))))
       ((lean4-indent--starts-with-p anchor-text lean4-indent--re-starts-calc)
        (+ prev-indent step))
       ;; If this := by closes a declaration, indent relative to that declaration line.
       ((let* ((top (and prev-pos (lean4-indent--find-top-level-anchor prev-pos)))
               (top-text (and top (lean4-indent--line-text top))))
          (and top
               (or (lean4-indent--line-top-level-anchor-p top-text)
                   (lean4-indent--starts-with-p top-text lean4-indent--re-starts-decl))
               (not (string-match-p lean4-indent--re-have-suffices prev-text))
               (not (and prev-pos
                         (lean4-indent--prev-have-suffices-p prev-pos prev-indent)))
               (+ (lean4-indent--line-indent top) step))))
       ((or prev-continuation-p prev-shallow-continuation-p) parent-indent)
       (prev-starts-with-paren
        (if (and anchor
                 (>= (- prev-indent anchor-indent) (* 3 step)))
            anchor-indent
          parent-indent))
       (t (+ prev-indent step))))
     ;; 6) After := (no by)
     (prev-line-ends-with-coloneq
      (if (lean4-indent--starts-with-p current-text ":=")
          prev-indent
        (cond
         ((and prev-starts-with-paren
               (not (lean4-indent--line-starts-with-paren-and-closes-p prev-pos)))
          (+ prev-indent step))
         ((and prev-pos (lean4-indent--in-calc-block-p prev-pos))
          (let ((calc-indent (lean4-indent--find-calc-step-indent prev-pos)))
            (if calc-indent (+ calc-indent step) (+ prev-indent step))))
         ((and anchor-pos
               (not (string-match-p lean4-indent--re-have-suffices prev-text))
               (not (lean4-indent--line-starts-with-calc-step-p prev-text))
               (not (lean4-indent--starts-with-p anchor-text lean4-indent--re-starts-calc))
               (or (lean4-indent--line-top-level-anchor-p anchor-text)
                   (lean4-indent--starts-with-p anchor-text lean4-indent--re-starts-decl)))
          (+ anchor-indent step))
         ((and (not (string-match-p lean4-indent--re-have-suffices prev-text))
               (not (lean4-indent--line-starts-with-calc-step-p prev-text))
               (not (and anchor-text
                         (lean4-indent--starts-with-p anchor-text lean4-indent--re-starts-calc)))
               (let ((top (and prev-pos (lean4-indent--find-top-level-anchor prev-pos))))
                 top))
          (let ((top (and prev-pos (lean4-indent--find-top-level-anchor prev-pos))))
            (+ (lean4-indent--line-indent top) step)))
         ((lean4-indent--line-starts-with-calc-step-p prev-text)
          (+ prev-indent step))
         ((lean4-indent--starts-with-p anchor-text lean4-indent--re-starts-calc)
          (+ anchor-indent step))
         (prev-continuation-p
          (max 0 (- prev-indent step)))
         (t (+ prev-indent step)))))
     ;; Equals in have/suffices continuations keeps indent
     ((and prev-line-ends-with-equals
           prev-continuation-p
           anchor-pos
           (string-match-p lean4-indent--re-have-suffices
                           (lean4-indent--line-text anchor-pos)))
      prev-indent)
     ;; 7) After by / => / ↦ / calc / termination_by / decreasing_by / "{" / "="
     ((or prev-line-ends-with-by
          prev-line-ends-with-fat-arrow
          prev-line-ends-with-fun-arrow
          prev-line-ends-with-do
          prev-line-ends-with-then
          prev-line-ends-with-else
          prev-line-ends-with-calc
          prev-line-ends-with-equals
          (string-match-p lean4-indent--re-ends-termination prev-text)
          (string-match-p lean4-indent--re-ends-decreasing prev-text)
          (lean4-indent--ends-with-p prev-text "{")
          prev-line-ends-with-where)
      (cond
       ((and prev-line-ends-with-by
             (string-match-p lean4-indent--re-fun-by prev-text))
        parent-indent)
       ((and prev-line-ends-with-equals
             prev-pos
             (lean4-indent--in-calc-block-p prev-pos))
        (+ prev-indent (* 2 step)))
       ((and prev-line-ends-with-fun-arrow
             (string-match-p lean4-indent--re-classical-exact-fun prev-text))
        (+ prev-indent (* 2 step)))
       ((and prev-line-ends-with-fun-arrow
             (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-fun)
             (lean4-indent--line-blank-p current-text))
        prev-indent)
       (t (+ prev-indent step))))
     ;; 7.5) Continue a simple head term when an enclosing anchor already expects one term.
     ((and anchor-term-body-indent
           (= prev-indent anchor-term-body-indent)
           (or (lean4-indent--simple-term-head-line-p prev-text-no-comment)
               (lean4-indent--plain-application-head-line-p prev-text-no-comment))
           (lean4-indent--term-continuation-line-p current-text))
      (+ prev-indent step))
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
           (not starts-with-paren)
           (not starts-with-branch)
           (not starts-with-focus)
           (not (lean4-indent--starts-with-p current-text "⟩")))
      (let* ((open-line-indent (and open-paren-pos
                                    (save-excursion
                                      (goto-char open-paren-pos)
                                      (current-indentation))))
             (open-line-prefix (and open-paren-pos
                                    (save-excursion
                                      (goto-char open-paren-pos)
                                      (buffer-substring-no-properties
                                       (line-beginning-position) open-paren-pos))))
             (open-line-has-text (and open-line-prefix
                                      (string-match-p "[^ \t]" open-line-prefix))))
        (if open-line-has-text
            (+ open-line-indent step)
          (+ open-paren-col step))))
     ;; 8.5) Lines inside { ... } structure literals
     ((and open-paren-col
           (eq open-paren-char ?{)
           (not starts-with-paren)
           (not starts-with-branch)
           (not starts-with-focus)
           (not (lean4-indent--starts-with-p current-text "}")))
      (let* ((open-line-indent (and open-paren-pos
                                    (save-excursion
                                      (goto-char open-paren-pos)
                                      (current-indentation))))
             (open-line-prefix (and open-paren-pos
                                    (save-excursion
                                      (goto-char open-paren-pos)
                                      (buffer-substring-no-properties
                                       (line-beginning-position) open-paren-pos))))
             (open-line-has-text (and open-line-prefix
                                      (string-match-p "[^ \t]" open-line-prefix))))
        (if open-line-has-text
            (+ open-line-indent step)
          (+ open-paren-col step))))
     ;; 8.75) Closing delimiters align with opener
     ((and starts-with-closing open-paren-col)
      open-paren-col)
     ;; 9) Paren-led lines
     (starts-with-paren
      (let* ((leading-parens (lean4-indent--leading-paren-count prev-text))
             (first-non-paren-col (+ prev-indent leading-parens))
             (last-unmatched-col (and prev-pos (lean4-indent--last-unmatched-open-paren-col prev-pos))))
        (cond
         ((and (lean4-indent--line-ends-with-term-continuation-p prev-text-op)
               (not (lean4-indent--line-ends-with-at-star-p prev-text-op)))
          (+ prev-indent step))
         (current-is-underscore-paren
          (or (and prev-pos (lean4-indent--find-prev-paren-dedent prev-pos (- prev-indent (* 2 step))))
              (and prev-pos (lean4-indent--prev-paren-block-min-indent prev-pos))
              (and prev-pos (lean4-indent--find-prev-paren-start-indent prev-pos prev-indent))
              last-unmatched-col
              open-paren-col
              prev-indent))
         ((and (not open-paren-col)
               prev-pos
               (lean4-indent--line-closes-paren-p prev-pos)
               (not prev-line-ends-with-comma))
          (or (lean4-indent--find-prev-paren-start-indent prev-pos prev-indent)
              prev-indent))
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
           (not (lean4-indent--line-ends-with-term-continuation-p prev-text-op)))
      prev-indent)
     ;; 10.25) Term continuation operators (<;>, ;)
     ((and (lean4-indent--line-ends-with-term-continuation-p prev-text-op)
           (not (lean4-indent--line-ends-with-at-star-p prev-text-op)))
      (if (and prev-pos (lean4-indent--line-starts-with-calc-step-p prev-text))
          (+ prev-indent (* 2 step))
        (+ prev-indent step)))
     ;; 10.5) Operator continuation
     ((lean4-indent--operator-continuation-p prev-text-op)
      prev-indent)
     ;; 10.75) Arguments continue after an open paren on the previous line.
     ((and prev-pos
           open-paren-col-prev
           (lean4-indent--line-opens-paren-p prev-pos)
           (not starts-with-paren)
           (not starts-with-branch)
           (not starts-with-focus))
      (+ prev-indent step))
     ;; 11) Focus dots
     (prev-starts-with-focus
      (+ prev-indent step))
     ;; 11.2) Continue multi-line bracketed lists started on previous line.
     ((and prev-pos
           prev-line-ends-with-comma
           open-paren-char-prev
           (memq open-paren-char-prev (list ?\( ?\[ lean4-indent--char-left-angle))
           (not starts-with-paren)
           (not starts-with-branch)
           (not starts-with-focus))
      (+ prev-indent step))
     ;; 11.25) Dedent after closing a paren/bracket/brace line.
     ((and prev-pos
           anchor-pos
           (not open-paren-col)
           (not starts-with-paren)
           (not starts-with-branch)
           (not starts-with-focus)
           (not (string-match-p lean4-indent--re-ends-angle-close prev-text))
           (or (lean4-indent--line-starts-with-closing-p prev-text)
               prev-ends-with-brace
               prev-closes-bracket-with-at
               (and prev-ends-with-bracket
                    (not (string-match-p "\\]\\s-*$" current-text))))
           (lean4-indent--line-closes-paren-p prev-pos))
      (let* ((anchor-text (lean4-indent--line-text anchor-pos))
             (anchor-parent (lean4-indent--find-anchor anchor-pos anchor-indent)))
        (if (lean4-indent--starts-with-p anchor-text "{")
            (if anchor-parent (cdr anchor-parent) 0)
          anchor-indent)))
     ;; 11.4) Dedent after exact before a simp-like line in nested by blocks.
     ((and prev-pos
           anchor-pos
           (> anchor-indent 0)
           (= prev-indent (+ anchor-indent step))
           (let ((anchor-text (lean4-indent--line-text anchor-pos)))
             (or (lean4-indent--line-ends-with-by-p anchor-text)
                 (lean4-indent--line-ends-with-coloneq-by-p anchor-text)))
           (lean4-indent--line-starts-with-exact-p prev-text)
           (lean4-indent--line-starts-with-simp-like-p current-text))
      anchor-indent)
     ;; 11.5) Dedent after nested `by` blocks that close on the previous line.
     ((and prev-pos
           anchor-pos
           (> anchor-indent 0)
           (= prev-indent (+ anchor-indent step))
           (let ((anchor-text (lean4-indent--line-text anchor-pos)))
             (or (lean4-indent--line-ends-with-by-p anchor-text)
                 (lean4-indent--line-ends-with-coloneq-by-p anchor-text)))
           (let ((prev-simp (lean4-indent--line-starts-with-simp-like-p prev-text))
                 (prevprev-simp (lean4-indent--line-starts-with-simp-like-p prevprev-text)))
             (or prev-simp (and prevprev-simp (lean4-indent--line-closes-paren-p prev-pos))))
           (not (lean4-indent--line-blank-p current-text))
           (not starts-with-paren)
           (not starts-with-branch)
           (not starts-with-focus)
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma)
           (not prev-line-ends-with-colon)
           (not prev-line-ends-with-coloneq)
           (not prev-line-ends-with-coloneq-by)
           (not prev-line-ends-with-by)
           (or (lean4-indent--line-closes-paren-p prev-pos)
               (lean4-indent--line-contains-balanced-bracket-p prev-pos)))
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

(defun lean4-indent-line-function ()
  "Indent current line according to Lean 4 rules."
  (interactive)
  (let* ((computed (lean4-indent--compute-indent))
         (current (current-indentation))
         (target (if (and (eq this-command 'indent-for-tab-command)
                          (eq last-command 'indent-for-tab-command))
                     (lean4-indent--cycle-indent computed current)
                   computed))
         (eolp (eolp)))
    (indent-line-to (max 0 target))
    (when eolp
      (end-of-line))))

(provide 'lean4-indent)
;;; lean4-indent.el ends here
