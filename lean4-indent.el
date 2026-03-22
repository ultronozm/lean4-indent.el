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
;;   (add-hook 'lean4-mode-hook #'lean4-indent-setup-buffer)
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

(defvar-local lean4-indent--buffer-context-cache-tick nil
  "Buffer modification tick for live indentation caches.")

(defvar-local lean4-indent--buffer-top-level-contexts nil
  "Buffer-local top-level context cache for live indentation.")

(defvar-local lean4-indent--buffer-line-contexts nil
  "Buffer-local per-line context cache for live indentation.")

(defvar-local lean4-indent--building-buffer-context-caches nil
  "Non-nil while live indentation caches are being rebuilt.")

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
    "<=" ">=" "=>" "->" "≅" "≃" "≪≫")
  "Operators that trigger line continuation when they end a line.")

(defconst lean4-indent--simp-like-keywords
  '("simp" "simp_rw" "simp?" "simpa" "rwa")
  "Keywords treated as simp-like tactics.")

(defconst lean4-indent--top-level-anchors
  '("attribute" "add_decl_doc" "compile_inductive" "initialize" "initialize_simps_projections"
    "add_aesop_rules"
    "grind_pattern" "irreducible_def" "proof_wanted" "set_option" "open" "universe" "variable"
    "extend_docs"
    "run_cmd"
    "library_note2"
    "export" "include" "omit" "unseal" "suppress_compilation" "unsuppress_compilation" "partial"
    "insert_to_additive_translation"
    "mk_iff_of_inductive_prop"
    "assert_not_exists"
    "elab_rules"
    "unif_hint"
    "private" "public" "protected" "unsafe" "meta"
    "termination_by" "decreasing_by"
    "#check" "#eval" "#guard_msgs"
    "alias" "noncomputable" "nonrec"
    "@[" "scoped["
    "namespace" "section" "public section" "mutual" "deriving")
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
(defconst lean4-indent--re-macro-rules
  "^[ \t]*\\(?:\\(?:local\\|scoped\\)[ \t]+\\)?macro_rules\\_>")
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
(defconst lean4-indent--re-ends-left-arrow "←")
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
  (let ((context (lean4-indent--region-line-context pos)))
    (if (and context (plist-member context :text-no-comment))
        (plist-get context :text-no-comment)
      (save-excursion
        (goto-char pos)
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (text (buffer-substring-no-properties bol eol))
               (prev-continued-string-line
                (and (not (bobp))
                     (save-excursion
                       (forward-line -1)
                       (string-match-p "\\\\\\s-*$"
                                       (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position))))
                     (string-match-p "\"" text)))
               (cut eol))
          (if prev-continued-string-line
              text
            (goto-char bol)
            (while (re-search-forward "--" eol t)
              (let ((ppss (syntax-ppss (match-beginning 0))))
                (unless (or (nth 3 ppss) (nth 4 ppss))
                  (setq cut (match-beginning 0))
                  (goto-char eol))))
            (buffer-substring-no-properties bol cut)))))))

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

(defun lean4-indent--next-nonblank ()
  "Return position of next nonblank line or nil."
  (save-excursion
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (forward-line 1)
        (when (and (not (eobp))
                   (let ((text (lean4-indent--line-text (point))))
                     (not (lean4-indent--line-blank-p text))))
          (setq found (point))))
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

(defun lean4-indent--line-ends-with-left-arrow-p (text)
  "Return non-nil if TEXT ends with ←, allowing a trailing line comment."
  (lean4-indent--ends-with-keyword-p text lean4-indent--re-ends-left-arrow))

(defun lean4-indent--line-starts-with-quantifier-and-ends-with-comma-p (text)
  "Return non-nil if TEXT starts with a quantifier binder and ends with a comma."
  (let ((trim (string-trim-left text)))
    (and (string-match-p "\\`\\(?:∀\\|∃\\|Σ\\|Π\\)" trim)
         (lean4-indent--line-ends-with-comma-p trim))))

(defun lean4-indent--qualified-apply-line-p (text)
  "Return non-nil if TEXT is an `apply` line headed by a qualified name."
  (string-match-p
   "\\`[ \t]*apply\\_>\\s-+\\(?:_root_\\.\\|[[:word:]_'.]+\\.\\)"
   text))

(defun lean4-indent--proof-with-line-p (text)
  "Return non-nil if TEXT starts a proof-local `with` continuation line."
  (string-match-p "\\`[ \t]*with\\_>" text))

(defun lean4-indent--proof-with-continuation-head-line-p (text)
  "Return non-nil if TEXT can continue with a proof-local `with` line."
  (and (string-match-p
        "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:cases\\|rcases\\|obtain\\)\\_>\\s-+\\S-"
        text)
       (not (string-match-p "\\_<with\\_>" text))))

(defun lean4-indent--bare-have-suffices-colon-line-p (text)
  "Return non-nil if TEXT is a `have`/`suffices` line ending at the colon."
  (string-match-p
   "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|suffices\\)\\_>.*:\\s-*\\'"
   text))

(defun lean4-indent--proof-at-target-line-p (text)
  "Return non-nil if TEXT is a standalone tactic target clause like `at h ⊢`."
  (string-match-p "\\`[ \t]*at\\_>.*⊢\\s-*\\'" text))

(defun lean4-indent--proof-standalone-at-line-p (text)
  "Return non-nil if TEXT is a standalone local `at ...` modifier line."
  (string-match-p "\\`[ \t]*at\\_>.*\\'" text))

(defun lean4-indent--ordinary-proof-tactic-line-p (text)
  "Return non-nil if TEXT starts a routine proof tactic command."
  (string-match-p
   "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:intro\\|rw\\|simp\\(?:_rw\\| only\\)?\\|unfold\\|cases\\|rcases\\|subst\\|replace\\|obtain\\|simpa\\|exact\\|refine\\|apply\\|have\\|let\\)\\_>"
   text))

(defun lean4-indent--filter-upwards-line-p (text)
  "Return non-nil if TEXT starts a `filter_upwards` line."
  (string-match-p "\\`[ \t]*filter_upwards\\_>" text))

(defun lean4-indent--proof-block-opener-line-p (text)
  "Return non-nil if TEXT opens a subordinate proof/tactic block."
  (or (string-match-p "\\`[ \t]*\\(?:all_goals\\|any_goals\\)\\_>\\s-*\\'" text)
      (string-match-p "\\`[ \t]*split_ifs\\_>.*\\_<with\\_>" text)))

(defun lean4-indent--inside-filter-upwards-bracket-block-p (start-pos)
  "Return non-nil if START-POS is within a multiline `filter_upwards [...]` block."
  (save-excursion
    (goto-char start-pos)
    (let ((start-indent (current-indentation))
          (found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((text (lean4-indent--line-text (point)))
              (indent (lean4-indent--line-indent (point))))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-p (point)))
            (when (and (< indent start-indent)
                       (lean4-indent--filter-upwards-line-p text))
              (setq found t))
            (when (< indent start-indent)
              (setq start-indent indent)))))
      found)))

(defun lean4-indent--operator-led-continuation-line-p (text)
  "Return non-nil if TEXT starts with a leading operator continuation."
  (string-match-p "\\`[ \t]*[+-]\\s-" text))

(defun lean4-indent--calc-relation-column (text)
  "Return the column of the first relation token after `calc' in TEXT.

This is used for blank-line indentation after inline or single-line `calc`
steps, where the next sibling step often aligns under the relation token from
the current line."
  (when (string-match "\\_<calc\\_>" text)
    (let ((search-pos (match-end 0))
          (found nil))
      (while (and (not found)
                  (string-match
                   "\\(?:=ᶠ\\[\\|=O\\[\\|=o\\[\\|=Θ\\[\\|~\\[\\|≤\\|≥\\|<\\|>\\|=\\)"
                   text search-pos))
        (let ((beg (match-beginning 0)))
          (if (and (eq (aref text beg) ?=)
                   (> beg 0)
                   (eq (aref text (1- beg)) ?:))
              (setq search-pos (1+ beg))
            (setq found beg))))
      found)))

(defun lean4-indent--inline-coloneq-by-line-p (text)
  "Return non-nil if TEXT contains an inline `:= by` body with following text."
  (string-match-p ":=\\s-*\\_<by\\_>\\s-+\\S-" text))

(defun lean4-indent--inline-coloneq-by-tail (text)
  "Return the trimmed inline proof tail after `:= by` in TEXT, or nil."
  (when (string-match ":=\\s-*\\_<by\\_>\\s-*\\(.*\\)\\'" text)
    (string-trim (match-string 1 text))))

(defun lean4-indent--inline-coloneq-by-tail-column (text)
  "Return the starting column of the inline proof tail after `:= by`, or nil."
  (when (string-match ":=\\s-*\\_<by\\_>\\s-*\\(\\S-\\)" text)
    (match-beginning 1)))

(defun lean4-indent--semicolon-bracket-tactic-column (text)
  "Return the start column of a bracketed tactic after `;' in TEXT, or nil.

This handles inline proof tails such as `intros; simp only [...]`."
  (when (string-match
         ";\\s-*\\(\\(?:simp\\s-+only\\)\\|rw\\|simp_rw\\)\\_>.*\\[[^]]*\\'"
         text)
    (match-beginning 1)))

(defun lean4-indent--simple-bare-head-after-outer-coloneq-p (text &optional pos)
  "Return non-nil when TEXT ends with a single bare head after an outer `:='.

This is the shape that typically opens named arguments on the following line,
for example `:= LawfulMonad.mk''.

It deliberately excludes already-complete simple terms like `:= 2' and wrapped
applications like `:= Foo bar', which should not be forced one step deeper on
a fresh newline.

When POS is non-nil, only a delimiter-depth-0 `:=' on the corresponding line
counts as the outer `:='."
  (let ((tail text))
    (when pos
      (save-excursion
        (goto-char pos)
        (let ((end (line-end-position))
              (outer-coloneq nil))
          (while (and (< (point) end) (not outer-coloneq))
            (let* ((ppss (syntax-ppss (point)))
                   (depth (car ppss))
                   (in-str (nth 3 ppss))
                   (in-com (nth 4 ppss)))
              (when (and (= depth 0)
                         (not in-str)
                         (not in-com)
                         (eq (char-after) ?:)
                         (eq (char-after (1+ (point))) ?=))
                (setq outer-coloneq (point))))
            (forward-char 1))
          (setq tail (and outer-coloneq
                          (buffer-substring-no-properties
                           (+ outer-coloneq 2) end))))))
    (and tail
         (string-match-p
          "\\`\\s-*[@[:alpha:]_][^ \t\n]*\\s-*\\'"
          tail))))

(defun lean4-indent--application-continues-after-inline-fun-p (text)
  "Return non-nil when TEXT keeps applying arguments after an inline `fun` term.

This distinguishes lines like `f (fun x => ...) a` from lines whose body ends
at the `fun` term itself."
  (and (string-match-p "\\_<fun\\_>" text)
       (string-match-p "[])}⟩]\\s-+\\S-" text)))

(defun lean4-indent--line-ends-with-term-continuation-p (text)
  (lean4-indent--ends-with-p text lean4-indent--re-ends-term-continuation))

(defun lean4-indent--line-ends-with-semicolon-p (text)
  (lean4-indent--ends-with-p text ";"))

(defun lean4-indent--line-ends-with-at-star-p (text)
  (string-match-p lean4-indent--re-ends-at-star text))

(defun lean4-indent--comment-line-p (&optional pos)
  "Return t if POS is on a comment line (line or block)."
  (let* ((pos (or pos (point)))
         (context (lean4-indent--region-line-context pos)))
    (if (and context (plist-member context :comment-line))
        (plist-get context :comment-line)
      (save-excursion
        (goto-char pos)
        (let* ((bol (line-beginning-position))
               (ppss (syntax-ppss bol))
               (text (lean4-indent--line-text bol))
               (trim (string-trim-left text)))
          (or (nth 4 ppss)
              (string-prefix-p "--" trim)
              (string-prefix-p "/-" trim)))))))

(defun lean4-indent--string-line-p (&optional pos)
  "Return t if POS is on a line that begins inside a string literal."
  (let* ((pos (or pos (point)))
         (context (lean4-indent--region-line-context pos)))
    (if (and context (plist-member context :string-line))
        (plist-get context :string-line)
      (save-excursion
        (goto-char pos)
        (nth 3 (syntax-ppss (line-beginning-position)))))))

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
            (when (lean4-indent--line-structural-top-level-anchor-p (point))
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

(defun lean4-indent--line-ends-with-opening-delimiter-p (text)
  "Return non-nil if TEXT ends with an opening delimiter, allowing a line comment."
  (string-match-p "[][({⟨]\\s-*\\(?:--.*\\)?$" text))

(defun lean4-indent--current-closing-delimiter-belongs-to-top-level-body-p (top-level-context)
  "Return non-nil if the current closing delimiter closes the outer top-level body.

TOP-LEVEL-CONTEXT is the enclosing declaration context returned by
`lean4-indent--top-level-context'."
  (and top-level-context
       (lean4-indent--line-starts-with-closing-p (lean4-indent--line-text (point)))
       (let ((open (lean4-indent--open-paren-pos (point))))
         (and open
              (= (save-excursion
                   (goto-char open)
                   (current-indentation))
                 (lean4-indent--line-indent (plist-get top-level-context :pos)))))))

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
   ((string-match-p "{\\s-*\\(?:--.*\\)?$" text) 'open-brace)
   (t nil)))

(defun lean4-indent--line-ends-with-op-p (text)
  (string-match-p lean4-indent--ops-regexp (string-trim-right text)))

(defun lean4-indent--operator-continuation-p (text)
  "Return non-nil if TEXT ends with an operator and does not trigger other indent rules."
  (and (lean4-indent--line-ends-with-op-p text)
       (not (lean4-indent--line-body-intro-kind text))))

(defun lean4-indent--line-top-level-declaration-head-p (text)
  "Return non-nil if TEXT starts a top-level declaration header."
  (let ((case-fold-search nil))
    (string-match-p
     "\\`[ \t]*\\(?:@\\[[^]\n]+\\]\\s-*\\)*\\(?:\\_<\\(?:scoped\\|local\\|protected\\|private\\|public\\|noncomputable\\|unsafe\\|partial\\|nonrec\\|meta\\)\\_>\\s-+\\)*\\_<\\(?:def\\|instance\\|partial_fixpoint\\|irreducible_def\\|simproc_decl\\|theorem\\|lemma\\|example\\|structure\\|inductive\\|class\\|abbrev\\|macro\\|syntax\\|notation\\|elab\\|register_option\\)\\_>"
     text)))

(defun lean4-indent--line-top-level-anchor-p (text)
  (let ((case-fold-search nil))
    (or (lean4-indent--line-top-level-declaration-head-p text)
        (lean4-indent--macro-rules-line-p text)
        (string-match-p "\\`[ \t]*#[[:alpha:]_][[:word:]_]*\\_>" text)
        (string-match-p
         "\\`[ \t]*\\(?:\\_<\\(?:local\\|scoped\\)\\_>\\s-+\\)?\\_<grind_pattern\\_>"
         text)
        (string-match-p
         "\\`[ \t]*\\(?:\\_<scoped\\_>\\s-*\\[[^]\n]+\\]\\s-+\\)?\\_<attribute\\_>"
         text)
        (string-match-p
         "\\`[ \t]*\\(?:\\_<\\(?:local\\|scoped\\)\\_>\\s-+\\)?\\(?:notation\\(?:[0-9]+\\)?\\|infixl?\\|infixr\\|prefix\\|postfix\\)\\(?:[:][^ \t\n]+\\)?\\(?:\\s-\\|$\\)"
         text)
        (string-match-p
         "\\`[ \t]*\\(?:\\_<meta\\_>\\s-+\\)?\\_<register_option\\_>"
         text)
        (string-match-p lean4-indent--top-level-anchors-re text))))

(defun lean4-indent--line-top-level-binder-head-kind (text)
  "Classify a top-level binder head line TEXT.

Return `declaration' for ordinary wrapped declaration headers, `variable' for
wrapped `variable' lines, or nil if TEXT is neither."
  (cond
   ((lean4-indent--line-top-level-declaration-head-p text) 'declaration)
   ((string-match-p "\\`[ \t]*\\_<proof_wanted\\_>" text) 'declaration)
   ((string-match-p "\\`[ \t]*\\_<variable\\_>" text) 'variable)
   (t nil)))

(defun lean4-indent--line-leading-binder-group-count (pos)
  "Return the number of leading top-level binder groups on the line at POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((count 0)
          end)
      (while (and (looking-at-p "[][()]")
                  (setq end (ignore-errors (scan-sexps (point) 1))))
        (setq count (1+ count))
        (goto-char end)
        (skip-chars-forward " \t"))
      count)))

(defun lean4-indent--line-top-level-wrappable-anchor-p (text)
  "Return non-nil if TEXT starts a wrapped top-level command header."
  (let ((case-fold-search nil))
    (or (string-match-p
         "\\`[ \t]*\\(?:\\_<\\(?:local\\|scoped\\)\\_>\\s-+\\)?\\(?:notation\\(?:[0-9]+\\)?\\|infixl?\\|infixr\\|prefix\\|postfix\\)\\(?:[:][^ \t\n]+\\)?\\(?:\\s-\\|$\\)"
         text)
        (string-match-p
         "\\`[ \t]*\\_<open\\_>"
         text)
        (string-match-p
         "\\`[ \t]*\\_<deriving\\_>\\s-+\\_<instance\\_>"
         text)
        (string-match-p
         "\\`[ \t]*\\_<proof_wanted\\_>"
         text)
        (string-match-p
         "\\`[ \t]*\\_<\\(?:syntax\\|macro\\|elab\\)\\_>"
         text))))

(defun lean4-indent--standalone-top-level-wrappable-prefix-p (&optional pos)
  "Return non-nil if POS is a standalone `local`/`scoped` prefix for a wrapped top-level command."
  (let* ((pos (or pos (point)))
         (text (lean4-indent--line-text pos)))
    (and (string-match-p "\\`[ \t]*\\_<\\(?:local\\|scoped\\)\\_>\\s-*\\'" text)
         (save-excursion
           (goto-char pos)
           (forward-line 1)
           (let ((next (if (and (not (eobp))
                                (not (lean4-indent--line-blank-p
                                      (lean4-indent--line-text (point)))))
                           (point)
                         (lean4-indent--next-nonblank))))
             (and next
                  (not (lean4-indent--comment-line-p next))
                  (let ((next-trim (string-trim-left (lean4-indent--line-text next))))
                    (or (string-prefix-p "notation" next-trim)
                        (string-prefix-p "infix" next-trim)
                        (string-prefix-p "prefix" next-trim)
                        (string-prefix-p "postfix" next-trim)))))))))

(defun lean4-indent--line-structural-top-level-anchor-p (&optional pos)
  "Return non-nil if line at POS is a real top-level anchor."
  (let ((pos (or pos (point))))
    (let ((context (lean4-indent--region-line-context pos)))
      (if (and context (plist-member context :structural-top-level-anchor))
          (plist-get context :structural-top-level-anchor)
        (and (save-excursion
               (goto-char pos)
               (let ((ppss (syntax-ppss (line-beginning-position))))
                 (and (= (car ppss) 0)
                      (not (nth 3 ppss))
                      (not (nth 4 ppss)))))
             (or (lean4-indent--line-top-level-anchor-p (lean4-indent--line-text pos))
                 (lean4-indent--standalone-top-level-wrappable-prefix-p pos)))))))

(defun lean4-indent--comment-line-cached-p (&optional pos)
  "Return t if POS is on a comment line, using region cache when available."
  (let* ((pos (or pos (point)))
         (context (lean4-indent--region-line-context pos)))
    (if (and context (plist-member context :comment-line))
        (plist-get context :comment-line)
      (lean4-indent--comment-line-p pos))))

(defun lean4-indent--inside-open-top-level-attribute-block-p ()
  "Return non-nil when point is on a continuation line of a top-level `@[` block."
  (let* ((context (lean4-indent--region-line-context (point))))
    (if (and context (plist-member context :open-top-level-attribute-block))
        (plist-get context :open-top-level-attribute-block)
      (let ((pos (lean4-indent--prev-nonblank))
            found)
        (while (and pos (not found))
          (let ((text (lean4-indent--line-text pos)))
            (cond
             ((lean4-indent--starts-with-p text "@\\[")
              (setq found (not (lean4-indent--line-contains-balanced-bracket-p pos))))
             ((lean4-indent--line-contains-balanced-bracket-p pos)
              (setq pos nil))
             (t
              (setq pos (save-excursion
                          (goto-char pos)
                          (forward-line -1)
                          (lean4-indent--prev-nonblank)))))))
        found))))

(defun lean4-indent--declaration-header-body-intro-kind (pos)
  "Return the declaration header body-intro kind for line at POS, or nil.

This prefers a real outer `:=`-style body intro over a type colon on wrapped
declaration headers, so lines like `... : T := do` classify as `do` rather
than `colon`. It also normalizes header lines like `foo := {' to `coloneq' so
top-level declaration context reflects the real declaration body, not just the
trailing delimiter."
  (when pos
    (let* ((text (lean4-indent--line-text pos))
           (has-coloneq (lean4-indent--line-has-outer-coloneq-p pos)))
      (cond
       ((and has-coloneq
             (lean4-indent--line-ends-with-coloneq-by-p text))
        'coloneq-by)
       ((and has-coloneq
             (lean4-indent--line-ends-with-by-p text))
        'coloneq-by)
       ((and has-coloneq
             (lean4-indent--line-ends-with-do-p text))
        'do)
       ((and has-coloneq
             (or (string-match-p "{\\s-*\\(?:--.*\\)?$" text)
                 (lean4-indent--line-ends-with-opening-delimiter-p text)))
        'coloneq)
       (has-coloneq
        'coloneq)
       (t
        (lean4-indent--line-body-intro-kind text))))))

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
         (body-intro-kind (lean4-indent--line-body-intro-kind trim))
         (head-re "@?[[:word:]_.']+\\(?:\\.[{][^}\n]+[}]\\)?"))
    (and (not body-intro-kind)
         (not (string-match-p lean4-indent--top-level-anchors-re trim))
         (not (string-match-p
               "\\`\\_<\\(?:fun\\|let\\|calc\\|where\\|end\\|if\\|match\\|do\\|have\\|suffices\\)\\_>"
               trim))
         (not (lean4-indent--branch-line-p trim))
         (not (lean4-indent--focus-dot-line-p trim))
         (not (lean4-indent--line-starts-with-closing-p trim))
         (cond
          ((string-match-p (concat "\\`" head-re "\\'") trim)
           'atom)
          ((and (string-match-p (concat "\\`" head-re "\\(?:\\s-+.+\\)\\'") trim)
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
           "\\`\\(?:·\\s-*\\)?\\(?:exact\\|refine\\|apply\\)\\_>\\s-+\\(.+\\)\\'"
           trim)
      (lean4-indent--line-application-head-kind (match-string 1 trim)))))

(defun lean4-indent--pipe-left-tail-head-kind (text &optional _pos)
  "Classify the outer `<|` tail in TEXT, or return nil.

Only a `<|` at delimiter depth 0 counts. This avoids treating inner
`<|` applications inside a completed parenthesized term as if they
were the live continuation for a following blank line."
  (let ((depth 0)
        (in-string nil)
        (idx 0)
        (len (length text))
        found)
    (while (and (< idx len) (not found))
      (let ((ch (aref text idx)))
        (cond
         ((eq ch ?\")
          (setq in-string (not in-string)))
         (in-string nil)
         ((memq ch '(?\( ?\[ ?\{ ?⟨))
          (setq depth (1+ depth)))
         ((and (> depth 0)
               (memq ch '(?\) ?\] ?\} ?⟩)))
          (setq depth (1- depth)))
         ((and (= depth 0)
               (= ch ?<)
               (< (1+ idx) len)
               (= (aref text (1+ idx)) ?|))
          (setq found
                (lean4-indent--line-application-head-kind
                 (substring text (+ idx 2)))))))
      (setq idx (1+ idx)))
    found))

(defun lean4-indent--calc-inline-expression-column (text)
  "Return the starting column of an inline `calc` expression in TEXT, or nil."
  (when (string-match "\\`[ \t]*calc\\s-+\\S-" text)
    (1- (match-end 0))))

(defun lean4-indent--embedded-calc-expression-column (text)
  "Return the starting column of an embedded `calc` expression in TEXT, or nil."
  (when (string-match "\\_<calc\\_>\\s-+\\S-" text)
    (1- (match-end 0))))

(defun lean4-indent--line-contains-calc-p (text)
  "Return non-nil if TEXT contains a real `calc` opener."
  (string-match-p "\\_<calc\\_>" text))

(defun lean4-indent--calc-relation-rhs-column (text)
  "Return the starting column of the RHS on a calc relation line TEXT, or nil."
  (when (string-match
         "\\`[ \t]*_\\s-*\\(?:=ᶠ\\[\\|=O\\[\\|=o\\[\\|=Θ\\[\\|~\\[\\|≤\\|≥\\|<\\|>\\|=\\)\\s-*\\S-"
         text)
    (1- (match-end 0))))

(defun lean4-indent--calc-relation-simple-rhs-p (text)
  "Return non-nil if TEXT has a simple one-token RHS on a calc relation line."
  (when (string-match
         "\\`[ \t]*_\\s-*\\(?:=ᶠ\\[\\|=O\\[\\|=o\\[\\|=Θ\\[\\|~\\[\\|≤\\|≥\\|<\\|>\\|=\\)\\s-*\\([^ \t].*?\\)\\s-*:=\\s-*\\_<by\\_>\\s-*\\'"
         text)
    (not (string-match-p "[ \t]" (match-string 1 text)))))

(defun lean4-indent--inequality-rhs-column (text)
  "Return the starting column of the RHS after the first inequality in TEXT."
  (let ((search-pos 0)
        (found nil))
    (while (and (not found)
                (string-match "\\(?:≤\\|≥\\|<\\|>\\)\\s-*\\S-" text search-pos))
      (let ((beg (match-beginning 0)))
        (if (and (> beg 0)
                 (eq (aref text (1- beg)) ?:))
            (setq search-pos (1+ beg))
          (setq found (1- (match-end 0))))))
    found))

(defun lean4-indent--inline-coloneq-by-starter-p (text)
  "Return non-nil if TEXT has an inline `:= by` proof tail that opens more proof."
  (let ((tail (lean4-indent--inline-coloneq-by-tail text)))
    (and tail
         (not (string-empty-p tail))
         (not (string-match-p "\\(?:;\\|<;>\\)" tail))
         (string-match-p
          "\\`\\(?:gcongr\\|rw\\|simp_rw\\|refine\\|apply\\|exact_mod_cast\\|show\\|have\\|let\\|constructor\\|aesop\\|omega\\|linarith\\|positivity\\|calc\\)\\_>"
          tail))))

(defun lean4-indent--inline-coloneq-by-complete-p (text)
  "Return non-nil if TEXT has an inline `:= by` body that looks complete."
  (let ((tail (lean4-indent--inline-coloneq-by-tail text)))
    (and tail
         (not (string-empty-p tail))
         (not (lean4-indent--inline-coloneq-by-starter-p text)))))

(defun lean4-indent--projection-application-tail-line-p (text)
  "Return non-nil when TEXT ends in a continued projection application.

This recognizes lines of the form `...).foo arg`, where a further
argument on the next line is a natural deep continuation."
  (string-match-p
   "[)}⟩]\\.[[:word:]_']+\\_>\\s-+\\S-.*\\s-*$"
   text))

(defun lean4-indent--projection-head-line-p (text)
  "Return non-nil when TEXT ends in a projection head like `...).foo'."
  (string-match-p
   "[)}⟩]\\.[[:word:]_']+\\s-*$"
   text))

(defun lean4-indent--bare-tactic-term-intro-line-p (text)
  "Return non-nil if TEXT is a bare `exact'/`refine'/`apply' line."
  (string-match-p "\\`[ \t]*\\(?:exact\\|refine\\|apply\\)\\_>[ \t]*\\'" text))

(defun lean4-indent--line-leading-angle-paren-col (text)
  "Return the column of the leading '(' in a line starting with `⟨(`, or nil."
  (when (string-match "\\`[ \t]*⟨(" text)
    (1- (match-end 0))))

(defun lean4-indent--paren-led-bare-head-line-p (text)
  "Return non-nil when TEXT is a parenthesized bare head awaiting arguments."
  (string-match-p "\\`[ \t]*\\(?:([ \t]*\\)+@?[[:word:]_.']+\\s-*\\'" text))

(defun lean4-indent--paren-led-application-tail-line-p (text)
  "Return non-nil when TEXT starts with a parenthesized head and one argument.

This recognizes shapes like `(cond x` where a following line is naturally a
deeper sibling argument."
  (string-match-p
   "\\`[ \t]*\\(?:([ \t]*\\)+@?[[:word:]_.']+\\_>\\s-+\\S-.*\\'"
   text))

(defun lean4-indent--paren-led-first-argument-column (text)
  "Return the first argument column for a parenthesized head line in TEXT.

This handles shapes like `((foo x` and `⟨((foo x`, where a following line can
continue with sibling arguments aligned under `x'."
  (let ((i 0)
        (len (length text))
        (paren-count 0))
    (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
      (setq i (1+ i)))
    (when (and (< i len) (eq (aref text i) lean4-indent--char-left-angle))
      (setq i (1+ i))
      (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
        (setq i (1+ i))))
    (while (and (< i len) (eq (aref text i) ?\())
      (setq paren-count (1+ paren-count))
      (setq i (1+ i))
      (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
        (setq i (1+ i))))
    (when (>= paren-count 2)
      (when (string-match "@?[[:word:]_.']+\\_>\\s-+" text i)
        (when (= (match-beginning 0) i)
          (match-end 0))))))

(defun lean4-indent--simple-parenthesized-application-before-coloneq-p (text)
  "Return non-nil if TEXT is a simple head with only `(...)` args before `:='.

This distinguishes wrapped declaration-result lines like
`foo (bar baz) :=', which can continue with a projection-style body on the
next line, from more complex carried terms like `@Computable ... fun ... :='."
  (let ((trim (string-trim text)))
    (and (not (string-match-p "\\_<fun\\_>" trim))
         (not (string-match-p "<|" trim))
         (string-match-p
          "\\`@?\\S-+\\s-*(.+)\\s-*:=\\s-*\\'"
          trim))))

(defun lean4-indent--leading-paren-count (text)
  "Return the number of leading `(' delimiters in TEXT after whitespace/`⟨'."
  (let ((i 0)
        (len (length text))
        (count 0))
    (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
      (setq i (1+ i)))
    (when (and (< i len) (eq (aref text i) lean4-indent--char-left-angle))
      (setq i (1+ i))
      (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
        (setq i (1+ i))))
    (while (and (< i len) (eq (aref text i) ?\())
      (setq count (1+ count))
      (setq i (1+ i))
      (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
        (setq i (1+ i))))
    count))

(defun lean4-indent--inline-open-paren-argument-column (text)
  "Return the first inline `(' column after real head text in TEXT.

Leading whitespace and opening delimiters do not count as real head text."
  (let ((i 0)
        (len (length text))
        (seen-real nil)
        (found nil))
    (while (and (< i len) (not found))
      (let ((ch (aref text i)))
        (cond
         ((memq ch '(?\s ?\t)))
         ((eq ch ?\()
          (when seen-real
            (setq found i)))
         ((eq ch lean4-indent--char-left-angle)
          (when seen-real
            (setq found i)))
         (t
          (setq seen-real t))))
      (setq i (1+ i)))
    found))

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
            (setq found (lean4-indent--line-structural-top-level-anchor-p (point)))))))
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
            (setq found (lean4-indent--line-structural-top-level-anchor-p (point)))))))
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

(defun lean4-indent--find-prev-delimited-sibling-indent (start-pos prev-indent)
  "Return indent of nearest earlier less-indented delimiter-led sibling line.

Scan backward from START-POS through nonblank lines.  When a line indented less
than PREV-INDENT starts with a delimiter-led term, return its indentation."
  (save-excursion
    (goto-char start-pos)
    (let ((ceiling-indent prev-indent)
          (found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let* ((text (lean4-indent--line-text (point)))
               (indent (lean4-indent--line-indent (point))))
          (unless (lean4-indent--line-blank-p text)
            (when (< indent ceiling-indent)
              (if (lean4-indent--line-starts-with-paren-p text)
                  (setq found indent)
                (setq ceiling-indent indent))))))
      found)))

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
                           (lean4-indent--line-contains-calc-p text))
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
               ((lean4-indent--line-contains-calc-p text)
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
            (when (lean4-indent--line-structural-top-level-anchor-p (point))
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
                    (when (lean4-indent--line-structural-top-level-anchor-p (point))
                      (setq fallback-kind nil
                            fallback-pos nil)
                      (goto-char (1+ start-pos))))
                  (let ((kind (if (= (point) top)
                                  (lean4-indent--declaration-header-body-intro-kind
                                   (point))
                                (lean4-indent--line-body-intro-kind
                                 (lean4-indent--line-text-no-comment (point))))))
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
          (let* ((top-text (lean4-indent--line-text top))
                 (top-kind (lean4-indent--line-top-level-binder-head-kind top-text))
                 (body-intro
                  (and (eq top-kind 'declaration)
                       (lean4-indent--top-level-declaration-body-intro-from top start-pos))))
            (list :pos top
                  :kind top-kind
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
       (let ((contexts lean4-indent--region-line-contexts))
         (and contexts
              (let ((index (1- (line-number-at-pos pos t))))
                (and (>= index 0)
                     (< index (length contexts))
                     (aref contexts index)))))))

(defun lean4-indent--build-top-level-context-cache (step)
  "Build a per-line top-level context cache using indentation STEP."
  (save-excursion
    (goto-char (point-min))
    (let* ((line-count (line-number-at-pos (point-max) t))
           (contexts (make-vector (max 1 line-count) nil))
           (current-top-pos nil)
           (current-top-kind nil)
           (current-body-intro-pos nil)
           (current-body-intro-kind nil)
           (current-body-intro-final-p nil)
           (current-body-indent nil))
      (while (not (eobp))
        (let* ((pos (point))
               (text (lean4-indent--line-text pos)))
          (unless (or (lean4-indent--line-blank-p text)
                      (lean4-indent--comment-line-cached-p pos))
            (when (lean4-indent--line-structural-top-level-anchor-p pos)
              (setq current-top-pos (copy-marker pos)
                    current-top-kind (lean4-indent--line-top-level-binder-head-kind text)
                    current-body-intro-pos nil
                    current-body-intro-kind nil
                    current-body-intro-final-p nil
                    current-body-indent (+ (lean4-indent--line-indent pos) step))
              (when (not current-top-kind)
                (let ((kind (lean4-indent--line-body-intro-kind text)))
                  (when kind
                    (setq current-body-intro-pos (copy-marker pos)
                          current-body-intro-kind kind
                          current-body-intro-final-p
                          (memq kind '(coloneq-by coloneq by where do termination decreasing)))))))
            (when (and current-top-pos (eq current-top-kind 'declaration)
                       (not current-body-intro-final-p))
              (let ((kind (lean4-indent--declaration-header-body-intro-kind pos)))
                (cond
                 ((memq kind '(coloneq-by coloneq by where))
                  (setq current-body-intro-pos (copy-marker pos)
                        current-body-intro-kind kind
                        current-body-intro-final-p t))
                 ((eq kind 'do)
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
                           :kind current-top-kind
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
           (indent-stack nil)
           (open-top-level-attribute-block nil))
      (while (not (eobp))
        (let* ((pos (point))
               (text (lean4-indent--line-text pos))
               (nonblank (not (lean4-indent--line-blank-p text)))
               (indent (and nonblank (lean4-indent--line-indent pos)))
               (trim (string-trim-left text))
               (ppss (syntax-ppss pos))
               (string-line (nth 3 ppss))
               (comment-line (or (nth 4 ppss)
                                 (string-prefix-p "--" trim)
                                 (string-prefix-p "/-" trim)))
               (structural-top-level-anchor
                (and (= (car ppss) 0)
                     (not (nth 3 ppss))
                     (not (nth 4 ppss))
                     (or (lean4-indent--line-top-level-anchor-p text)
                         (lean4-indent--standalone-top-level-wrappable-prefix-p pos))
                     (or (= indent 0)
                         (assoc 'mutual block-stack))))
               (significant (and nonblank
                                 (not comment-line))))
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
                      :open-top-level-attribute-block open-top-level-attribute-block
                      :block-indent (cdar block-stack)
                      :string-line string-line
                      :comment-line comment-line
                      :structural-top-level-anchor structural-top-level-anchor
                      :anchor-pos (caar indent-stack)
                      :anchor-indent (cdar indent-stack)
                      :calc-indent (plist-get (car calc-stack) :indent)
                      :calc-step-indent (plist-get (car calc-stack) :last-step-indent)))
          (when nonblank
            (push (cons (copy-marker pos) indent) indent-stack))
          (when (and nonblank
                     open-top-level-attribute-block
                     (lean4-indent--line-contains-balanced-bracket-p pos))
            (setq open-top-level-attribute-block nil))
          (when significant
            (when (and structural-top-level-anchor
                       (lean4-indent--starts-with-p text "@\\[")
                       (not (lean4-indent--line-contains-balanced-bracket-p pos)))
              (setq open-top-level-attribute-block t))
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
              (when (lean4-indent--line-contains-calc-p text)
                (push (list :indent indent :last-step-indent nil) calc-stack)))))
          (forward-line 1)))
      contexts)))

(defun lean4-indent--ensure-buffer-context-caches ()
  "Refresh buffer-local indentation caches when the buffer changed."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (and lean4-indent--buffer-context-cache-tick
                 (= lean4-indent--buffer-context-cache-tick tick)
                 lean4-indent--buffer-line-contexts
                 lean4-indent--buffer-top-level-contexts)
      (let* ((lean4-indent--building-buffer-context-caches t)
             (line-contexts (lean4-indent--build-region-line-context-cache))
             (top-level-contexts
              (let ((lean4-indent--region-line-contexts line-contexts))
                (lean4-indent--build-top-level-context-cache lean4-indent-offset))))
        (setq lean4-indent--buffer-line-contexts line-contexts
              lean4-indent--buffer-top-level-contexts top-level-contexts
              lean4-indent--buffer-context-cache-tick tick)))))

(defun lean4-indent--find-end-anchor-indent (start-pos)
  "Return indentation for an `end` line based on the nearest opener."
  (let* ((context (lean4-indent--region-line-context start-pos))
         (cached-indent (and context (plist-get context :block-indent))))
    (if cached-indent
        cached-indent
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
          (or found 0))))))

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
         (current-string-p (lean4-indent--string-line-p (line-beginning-position)))
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
         (top-level-pos (plist-get top-level-context :pos))
         (top-level-kind (plist-get top-level-context :kind))
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
               top-level-body-intro-pos
               (eq prev-pos top-level-body-intro-pos)
               (not prev-have-suffices-p)
               (not (and prev-pos
                         (lean4-indent--prev-have-suffices-p prev-pos prev-indent)))
               prev-top-level-body-indent))
         (prev-coloneq-top-level-body-indent
          (and top-level-body-intro-pos
               (eq prev-pos top-level-body-intro-pos)
               (not prev-have-suffices-p)
               (not prev-starts-with-calc-step)
               (not anchor-starts-calc)
               (not (eq top-level-body-intro-kind 'coloneq-by))
               (not (eq anchor-body-intro-kind 'where))
               (or (and anchor-pos
                        (lean4-indent--line-structural-top-level-anchor-p anchor-pos)
                        (+ anchor-indent step))
                   prev-top-level-body-indent))))
    (cond
     ;; 0) `end` lines align with their opener.
     ((lean4-indent--starts-with-p current-text "\\_<end\\_>")
      (if prev-pos
          (lean4-indent--find-end-anchor-indent (point))
        0))
     ;; 1) Keep comment/string indentation as written.
     ((or current-comment-p current-string-p)
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
         ((and top-level-pos
               (eq top-level-kind 'declaration)
               top-level-body-intro-pos
               (lean4-indent--branch-line-p
                (lean4-indent--line-text top-level-body-intro-pos))
               (> prev-indent (lean4-indent--line-indent top-level-pos)))
          (lean4-indent--line-indent top-level-pos))
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
     ((and mutual-indent (lean4-indent--line-structural-top-level-anchor-p (point)))
      (+ mutual-indent step))
     ;; 3) Top-level snap
     ((lean4-indent--line-structural-top-level-anchor-p (point))
      0)
     ;; 3.25) `deriving` after an inductive/structure body aligns with the declaration head.
     ((and (lean4-indent--starts-with-p current-text "\\_<deriving\\_>")
           top-level-context)
      (lean4-indent--line-indent top-level-pos))
     ;; 3.3) Later lines in wrapped top-level command headers align with the header anchor.
     ((and top-level-pos
           (not top-level-kind)
           (not top-level-body-intro-pos)
           (string-match-p
            "\\`[ \t]*\\_<deriving\\_>\\s-+\\_<instance\\_>"
            (lean4-indent--line-text top-level-pos))
           prev-pos
           (> prev-indent 0))
      (lean4-indent--line-indent top-level-pos))
     ;; 3.3) Zero-indent closing delimiters in top-level declaration bodies stay at declaration column.
     ((and top-level-context
           (memq top-level-body-intro-kind '(coloneq coloneq-by open-brace))
           (lean4-indent--current-closing-delimiter-belongs-to-top-level-body-p
            top-level-context))
      (lean4-indent--line-indent top-level-pos))
     ;; 3.5) `where` aligns with its declaration anchor.
     ((and (lean4-indent--starts-with-p current-text lean4-indent--re-starts-where) anchor-pos)
      anchor-indent)
     ;; 3.6) Equation-compiler branches of a top-level declaration align with the declaration head.
     ((and top-level-pos
           (eq top-level-kind 'declaration)
           (not top-level-body-intro-pos)
           starts-with-branch)
      (lean4-indent--line-indent top-level-pos))
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
     ;; 4.3) A bare application head immediately after a := / = line can keep taking arguments.
     ((and prev-pos
           prevprev-pos
           anchor-pos
           (> prev-indent anchor-indent)
           (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
               'atom)
           (not anchor-body-intro-kind)
           (memq prevprev-body-intro-kind '(coloneq equals))
           (< prev-indent prevprev-indent))
      (+ prev-indent step))
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
                  top-level-body-intro-pos
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
                  (lean4-indent--bare-have-suffices-colon-line-p
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
     ((and (lean4-indent--line-blank-p current-text)
           anchor-pos
           (lean4-indent--branch-line-p anchor-text)
           (> prev-indent anchor-indent)
           (not prev-line-ends-with-op)
           (not prev-line-ends-with-comma)
           (not prev-body-intro-kind))
      prev-indent)
     ;; 11.8) Blank lines after a first-level non-tactic declaration body snap back.
     ((and (lean4-indent--line-blank-p current-text)
           anchor-pos
           (lean4-indent--line-structural-top-level-anchor-p anchor-pos)
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
               (not (lean4-indent--line-structural-top-level-anchor-p (point)))))
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
         (not (lean4-indent--line-structural-top-level-anchor-p (point)))
         body-indent
         (>= current body-indent))))

(defun lean4-indent--stable-region-body-line-p ()
  "Return non-nil when `indent-region' will preserve the current line.

This mirrors the cheap region-only preservation path for completed top-level
declaration bodies, avoiding a full `lean4-indent--compute-indent' call for
lines that will remain unchanged anyway."
  (let* ((current (current-indentation))
         (current-text (lean4-indent--line-text (point)))
         (step lean4-indent-offset)
         (prev-pos (lean4-indent--prev-nonblank))
         (top-level-context (and prev-pos
                                 (lean4-indent--top-level-context prev-pos step)))
         (top-level-kind (plist-get top-level-context :kind))
         (body-indent (plist-get top-level-context :body-indent))
         (body-intro-pos (plist-get top-level-context :body-intro-pos))
         (body-intro-kind (plist-get top-level-context :body-intro-kind)))
    (and lean4-indent--preserve-tactic-region-indentation
         (not (lean4-indent--line-blank-p current-text))
         (not (lean4-indent--line-structural-top-level-anchor-p (point)))
         (or (and (> current 0)
                  body-indent
                  (>= current body-indent))
             (let* ((prev-text (if prev-pos (lean4-indent--line-text prev-pos) ""))
                    (prev-indent (if prev-pos (lean4-indent--line-indent prev-pos) 0))
                    (anchor (and prev-pos
                                 (lean4-indent--find-anchor prev-pos prev-indent)))
                    (anchor-pos (car-safe anchor))
                    (anchor-indent (if anchor (cdr anchor) 0))
                    (anchor-body-intro-kind
                     (and anchor-pos
                          (lean4-indent--line-body-intro-kind
                           (lean4-indent--line-text-no-comment anchor-pos))))
                    (top-level-declaration-header-continuation
                     (and (>= current 0)
                          (eq top-level-kind 'declaration)
                          (not body-intro-pos)
                          (or (> current 0)
                              (and prev-pos (> prev-indent 0)))))
                    (top-level-wrapped-anchor-continuation
                     (or
                      (and prev-pos
                           (lean4-indent--line-structural-top-level-anchor-p prev-pos)
                           (lean4-indent--line-top-level-wrappable-anchor-p prev-text)
                           (<= current (+ prev-indent step)))
                      (and top-level-context
                           (not top-level-kind)
                           (not body-intro-pos)
                           (string-match-p
                            "\\`[ \t]*\\_<deriving\\_>\\s-+\\_<instance\\_>"
                            (lean4-indent--line-text (plist-get top-level-context :pos)))
                           (or (> current 0)
                               (and prev-pos (> prev-indent 0)))
                           (<= current (+ prev-indent step)))))
                    (top-level-variable-continuation
                     (and (eq top-level-kind 'variable)
                          body-indent
                          (<= current body-indent)))
                    (top-level-anchor-continuation
                     (and prev-pos
                          (lean4-indent--line-structural-top-level-anchor-p prev-pos)
                          (memq (lean4-indent--line-body-intro-kind prev-text)
                                '(coloneq coloneq-by by where))
                          (<= current (+ prev-indent step))))
                    (shallow-first-top-level-body-line
                     (and (> current 0)
                          body-indent
                          body-intro-pos
                          (= prev-pos body-intro-pos)
                          (memq body-intro-kind '(coloneq coloneq-by by where))
                          (<= current body-indent)))
                    (zero-indent-first-top-level-body-line
                     (and (= current 0)
                          body-indent
                          body-intro-pos
                          (= prev-pos body-intro-pos)
                          (memq body-intro-kind '(coloneq coloneq-by by where))))
                    (zero-indent-top-level-tactic-body-line
                     (and (= current 0)
                          body-indent
                          (memq body-intro-kind '(by coloneq-by))
                          prev-pos
                          (> prev-indent 0)))
                    (zero-indent-top-level-focus-line
                     (and (= current 0)
                          body-indent
                          (memq body-intro-kind '(by coloneq-by termination decreasing))
                          (lean4-indent--focus-dot-line-p current-text)
                          prev-pos
                          (or (and body-intro-pos (= prev-pos body-intro-pos))
                              (lean4-indent--focus-dot-line-p prev-text))))
                    (zero-indent-top-level-attribute-continuation
                     (and (= current 0)
                          (lean4-indent--inside-open-top-level-attribute-block-p)))
                    (zero-indent-top-level-match-or-branch
                     (and (= current 0)
                          body-indent
                          (or (lean4-indent--starts-with-p current-text "\\_<match\\_>")
                              (and (lean4-indent--branch-line-p current-text)
                                   (or (and prev-pos body-intro-pos
                                            (= prev-pos body-intro-pos))
                                       (and (= prev-indent 0)
                                            (or (lean4-indent--starts-with-p prev-text "\\_<match\\_>")
                                                (lean4-indent--branch-line-p prev-text))))))))
                    (zero-indent-top-level-equation-branch
                     (and (= current 0)
                          (eq top-level-kind 'declaration)
                          (lean4-indent--branch-line-p current-text)
                          (or (not body-intro-pos)
                              (eq body-intro-kind 'colon)
                              (lean4-indent--branch-line-p
                               (lean4-indent--line-text body-intro-pos)))))
                    (zero-indent-top-level-macro-rules-branch
                     (and (= current 0)
                          prev-pos
                          (lean4-indent--branch-line-p current-text)
                          (lean4-indent--line-structural-top-level-anchor-p prev-pos)
                          (lean4-indent--macro-rules-line-p prev-text)))
                    (zero-indent-top-level-brace-body
                     (and (= current 0)
                          body-indent
                          (memq body-intro-kind '(coloneq coloneq-by))
                          (string-match-p "\\`[ \t]*[{}]" current-text)))
                    (zero-indent-top-level-closing-delimiter
                     (and (= current 0)
                          body-indent
                          (memq body-intro-kind '(coloneq coloneq-by open-brace))
                          (lean4-indent--current-closing-delimiter-belongs-to-top-level-body-p
                           top-level-context)))
                    (zero-indent-top-level-where-line
                     (and (= current 0)
                          body-indent
                          (memq body-intro-kind '(coloneq coloneq-by by do))
                          (lean4-indent--starts-with-p current-text "\\_<where\\_>")))
                    (zero-indent-where-field-body-line
                     (and (= current 0)
                          prev-pos
                          anchor-pos
                          (> prev-indent anchor-indent)
                          (eq anchor-body-intro-kind 'where)
                          (memq (lean4-indent--line-body-intro-kind
                                 (lean4-indent--line-text-no-comment prev-pos))
                                '(coloneq coloneq-by by do)))))
               (or top-level-declaration-header-continuation
                   top-level-wrapped-anchor-continuation
                   top-level-anchor-continuation
                   top-level-variable-continuation
                   shallow-first-top-level-body-line
                   zero-indent-first-top-level-body-line
                   zero-indent-top-level-tactic-body-line
                   zero-indent-top-level-focus-line
                   zero-indent-top-level-attribute-continuation
                   zero-indent-top-level-match-or-branch
                   zero-indent-top-level-equation-branch
                   zero-indent-top-level-macro-rules-branch
                   zero-indent-top-level-brace-body
                   zero-indent-top-level-closing-delimiter
                   zero-indent-top-level-where-line
                   zero-indent-where-field-body-line))))))

(defun lean4-indent--stable-region-shallow-top-level-anchor-line-p ()
  "Return non-nil when `indent-region' should preserve a shallow top-level anchor line."
  (let* ((current (current-indentation))
         (current-text (lean4-indent--line-text (point)))
         (prev-pos (lean4-indent--prev-nonblank))
         (prev-text (if prev-pos (lean4-indent--line-text prev-pos) "")))
    (and (> current 0)
         (<= current lean4-indent-offset)
         (lean4-indent--line-top-level-anchor-p current-text)
         (lean4-indent--line-top-level-declaration-head-p current-text)
         prev-pos
         (or (lean4-indent--starts-with-p prev-text "@\\[")
             (string-match-p
              "\\`[ \t]*\\(?:\\_<scoped\\_>\\s-*\\[[^]\n]+\\]\\s-+\\)?\\_<attribute\\_>"
              prev-text)))))

(defun lean4-indent--newline-blank-line-indent ()
  "Return a preferred indentation for a newly inserted blank line, or nil.

This is used for blank-line indentation. It tries to choose the deepest
plausible indentation in a few common completed-code situations, leaving `TAB'
to cycle to shallower alternatives."
  (when (lean4-indent--line-blank-p (lean4-indent--line-text (point)))
    (let* ((step lean4-indent-offset)
             (prev-pos (lean4-indent--prev-nonblank))
             (prevprev-pos (and prev-pos
                                (save-excursion
                                  (goto-char prev-pos)
                                  (lean4-indent--prev-nonblank))))
             (prev-text (if prev-pos (lean4-indent--line-text prev-pos) ""))
             (prev-text-no-comment
              (if (and prev-pos (not (lean4-indent--comment-line-p prev-pos)))
                  (lean4-indent--line-text-no-comment prev-pos)
                ""))
             (prevprev-text (if prevprev-pos (lean4-indent--line-text prevprev-pos) ""))
             (prevprev-text-no-comment
              (if (and prevprev-pos (not (lean4-indent--comment-line-p prevprev-pos)))
                  (lean4-indent--line-text-no-comment prevprev-pos)
                ""))
             (prev-comma-item-text
              (if (and prev-pos
                       (string-match-p "\\`[ \t]*," prev-text-no-comment))
                  (replace-regexp-in-string "\\`[ \t]*,\\s-*" ""
                                            prev-text-no-comment)
                prev-text-no-comment))
             (prev-indent (if prev-pos (lean4-indent--line-indent prev-pos) 0))
             (prevprev-indent (if prevprev-pos (lean4-indent--line-indent prevprev-pos) 0))
             (prev-line-has-outer-coloneq
              (and prev-pos
                   (lean4-indent--line-has-outer-coloneq-p prev-pos)))
             (prev-anonymous-typed-local-decl-p
              (and prev-pos
                   (string-match-p
                    "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|suffices\\)\\s-*:"
                    prev-text-no-comment)))
             (prev-leading-binder-groups
              (and prev-pos
                   (lean4-indent--line-leading-binder-group-count prev-pos)))
             (top-level-context (and prev-pos
                                     (lean4-indent--top-level-context prev-pos step)))
             (top-level-body-indent (plist-get top-level-context :body-indent))
             (top-level-body-intro-pos (plist-get top-level-context :body-intro-pos))
             (top-level-body-intro-kind (plist-get top-level-context :body-intro-kind))
             (anchor (and prev-pos
                          (lean4-indent--find-anchor prev-pos prev-indent)))
             (anchor-pos (car-safe anchor))
             (anchor-indent (if anchor (cdr anchor) 0))
             (anchor-text (if anchor-pos (lean4-indent--line-text anchor-pos) ""))
             (anchor-text-no-comment
              (if (and anchor-pos (not (lean4-indent--comment-line-p anchor-pos)))
                  (lean4-indent--line-text-no-comment anchor-pos)
                ""))
             (open-paren-pos (lean4-indent--open-paren-pos (point)))
             (open-paren-col (and open-paren-pos
                                  (save-excursion
                                    (goto-char open-paren-pos)
                                    (current-column))))
             (open-delimited-body-indent
              (and open-paren-pos open-paren-col
                   (lean4-indent--open-delimited-body-indent
                    open-paren-pos open-paren-col step)))
             (calc-relation-col
              (and prev-pos
                   (lean4-indent--calc-relation-column prev-text-no-comment)))
           (calc-inline-expression-col
            (and prev-pos
                 (lean4-indent--calc-inline-expression-column prev-text-no-comment)))
           (embedded-calc-expression-col
            (and prev-pos
                 (not (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-calc))
                 (lean4-indent--embedded-calc-expression-column prev-text-no-comment)))
           (prev-delimited-sibling-indent
            (and prev-pos
                 (or (string-match-p "[])}⟩]\\s-*$" prev-text)
                     (string-match-p "[])}⟩]\\.[[:word:]_'.]+\\s-*$"
                                     prev-text-no-comment))
                 (lean4-indent--find-prev-delimited-sibling-indent
                  prev-pos prev-indent)))
             (open-paren-pos-prev (and prev-pos (lean4-indent--open-paren-pos-at-eol prev-pos)))
             (open-paren-pos-prev-on-line
              (and open-paren-pos-prev
                   prev-pos
                   (save-excursion
                     (goto-char prev-pos)
                     (<= (line-beginning-position)
                         open-paren-pos-prev
                         (line-end-position)))
                   open-paren-pos-prev))
             (open-paren-prefix-has-real-text
              (and open-paren-pos-prev-on-line
                   (save-excursion
                     (goto-char open-paren-pos-prev-on-line)
                     (string-match-p
                      "[^ \t(\\[{⟨]"
                      (buffer-substring-no-properties
                       (line-beginning-position) open-paren-pos-prev-on-line))))))
       (cond
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             top-level-body-indent
             (> prev-indent top-level-body-indent)
             (eq (lean4-indent--line-body-intro-kind prev-text-no-comment) 'colon)
             (lean4-indent--line-ends-with-colon-p prev-text-no-comment)
             (> prev-leading-binder-groups 1))
        prev-indent)
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             top-level-body-indent
             (> prev-indent top-level-body-indent)
             (eq (lean4-indent--line-body-intro-kind prev-text-no-comment) 'colon)
             (lean4-indent--line-ends-with-colon-p prev-text-no-comment)
             (= prev-leading-binder-groups 1))
        (+ prev-indent step))
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (lean4-indent--semicolon-bracket-tactic-column prev-text-no-comment)
             (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
        (+ (lean4-indent--semicolon-bracket-tactic-column prev-text-no-comment)
           step))
       ((and prev-pos
             (lean4-indent--line-top-level-declaration-head-p prev-text-no-comment)
             prev-line-has-outer-coloneq
             (lean4-indent--simple-bare-head-after-outer-coloneq-p
              prev-text-no-comment prev-pos)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             prev-line-has-outer-coloneq
             (lean4-indent--simple-bare-head-after-outer-coloneq-p
              prev-text-no-comment prev-pos)
             (not (lean4-indent--line-top-level-declaration-head-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'where)
             (< prev-indent top-level-body-indent)
             (memq (lean4-indent--line-application-head-kind prev-text-no-comment)
                   '(atom application))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        top-level-body-indent)
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'where)
             (= prev-indent top-level-body-indent)
             (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'where)
             (> prev-indent top-level-body-indent)
             (string-match-p "\\`[ \t]*\\_<∀\\_>" prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'coloneq)
             top-level-body-indent
             (> prev-indent top-level-body-indent)
             top-level-body-intro-pos
             (= prev-pos top-level-body-intro-pos)
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment)
             (> prev-leading-binder-groups 0))
        prev-indent)
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'coloneq)
             top-level-body-indent
             (> prev-indent top-level-body-indent)
             (eq (lean4-indent--line-body-intro-kind prev-text-no-comment) 'coloneq)
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment)
             (lean4-indent--simple-parenthesized-application-before-coloneq-p
              prev-text-no-comment))
        prev-indent)
       ((and prev-pos
             (= prev-indent 0)
             (string-match-p "\\`[ \t]*\\_<variable\\_>\\s-+\\S-" prev-text-no-comment)
             (string-match-p "[([{]" prev-text-no-comment)
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (= prev-indent 0)
             (lean4-indent--line-top-level-declaration-head-p prev-text-no-comment)
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (= prev-indent 0)
             (lean4-indent--line-top-level-declaration-head-p prev-text-no-comment)
             (string-match-p ":\\s-*\\(?:haveI\\|letI\\)\\s-*:=\\s-*\\S-"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'coloneq)
             top-level-body-indent
             (> prev-indent top-level-body-indent)
             top-level-body-intro-pos
             (= prev-pos top-level-body-intro-pos)
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment))
        top-level-body-indent)
       ((and prev-pos
             prev-delimited-sibling-indent
             (> prev-indent (+ prev-delimited-sibling-indent step))
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (string-match-p "[])}⟩]\\.[[:word:]_'.]+\\s-*$" prev-text-no-comment)
             (not open-paren-pos-prev))
        (+ prev-delimited-sibling-indent step))
       ((and prev-pos
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (string-match-p "\\`[ \t]*((+" prev-text)
             (lean4-indent--inline-open-paren-argument-column prev-text-no-comment)
             (not (lean4-indent--in-calc-block-p prev-pos))
             anchor-pos
             (> prev-indent anchor-indent))
        (+ prev-indent (* 4 step)))
       ((and calc-relation-col
             (not (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment)))
        calc-relation-col)
       ((and prev-pos
             prev-line-has-outer-coloneq
             (memq (lean4-indent--line-application-head-kind prev-text-no-comment)
                   '(atom application))
             (string-match-p "_\\s-*$" prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             open-delimited-body-indent
             (string-match-p "\\`[ \t]*," prev-text-no-comment)
             (memq (lean4-indent--line-application-head-kind prev-comma-item-text)
                   '(atom application))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (max (+ open-delimited-body-indent step)
             (+ prev-indent (* 2 step))))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|let\\)\\_>.*:=\\s-*\\S-+"
              prev-text-no-comment)
             (string-match-p ")\\s-*$" prev-text)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (string-match-p ")\\s-*$" prev-text)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             open-paren-pos-prev
             open-paren-prefix-has-real-text
             (lean4-indent--focus-dot-line-p prev-text)
             (string-match-p "\\_<\\(?:have\\|let\\|suffices\\)\\_>.*:\\s-*\\S-+"
                             prev-text-no-comment)
             (or prev-anonymous-typed-local-decl-p
                 (not prev-line-has-outer-coloneq)))
        (+ prev-indent (* 4 step)))
       ((and prev-pos
             open-paren-pos-prev
             open-paren-prefix-has-real-text
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|let\\|suffices\\)\\_>.*:\\s-*\\S-+"
              prev-text-no-comment)
             (or prev-anonymous-typed-local-decl-p
                 (not prev-line-has-outer-coloneq)))
        (+ prev-indent (* 4 step)))
       ((and prev-pos
             open-delimited-body-indent
             (lean4-indent--focus-dot-line-p prev-text)
             (string-match-p "\\_<\\(?:have\\|let\\|suffices\\)\\_>.*:\\s-*\\S-+"
                             prev-text-no-comment)
             (or prev-anonymous-typed-local-decl-p
                 (not prev-line-has-outer-coloneq)))
        (max (+ open-delimited-body-indent step)
             (+ prev-indent (* 2 step))))
       ((and prev-pos
             (lean4-indent--focus-dot-line-p prev-text)
             (string-match-p "\\_<\\(?:have\\|let\\|suffices\\)\\_>.*:\\s-*\\S-+"
                             prev-text-no-comment)
             (or prev-anonymous-typed-local-decl-p
                 (not prev-line-has-outer-coloneq)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--paren-led-application-tail-line-p prev-text-no-comment)
             (lean4-indent--inline-open-paren-argument-column prev-text)
             open-paren-pos-prev-on-line
             (not (lean4-indent--projection-head-line-p prev-text-no-comment))
             (not (lean4-indent--projection-application-tail-line-p prev-text-no-comment))
             (not (lean4-indent--in-calc-block-p prev-pos)))
        (lean4-indent--inline-open-paren-argument-column prev-text))
       ((and prev-pos
             (lean4-indent--paren-led-first-argument-column prev-text-no-comment)
             open-paren-pos-prev-on-line
             (not (string-match-p "<|" prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--in-calc-block-p prev-pos)))
        (max (+ prev-indent step)
             (lean4-indent--paren-led-first-argument-column prev-text-no-comment)))
       ((and prev-pos
             (lean4-indent--branch-line-p prev-text)
             (string-match-p "\\_<with\\_>\\s-*$" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--branch-line-p prev-text)
             (string-match-p "\\(?:↦\\|=>\\)\\s-+\\S-" prev-text-no-comment)
             (not (lean4-indent--pipe-left-tail-head-kind prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        prev-indent)
       ((and prev-pos
             (lean4-indent--branch-line-p prev-text)
             (not (string-match-p "\\(?:↦\\|=>\\)" prev-text-no-comment))
             (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (= prev-indent 0)
             (string-match-p
              "\\`[ \t]*\\(?:\\_<scoped\\_>\\s-*\\[[^]\n]+\\]\\s-+\\)?\\_<attribute\\_>"
              prev-text-no-comment))
        step)
       ((and prev-pos
             top-level-context
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (> prev-indent 0)
             (string-match-p "\\_<∀\\_>" prev-text-no-comment)
             (string-match-p "," prev-text-no-comment))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (> prev-indent top-level-body-indent)
             (eq (lean4-indent--line-body-intro-kind prev-text-no-comment) 'colon))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (> prev-indent top-level-body-indent)
             (string-match-p "\\`[ \t]*(.+:\\s-*.+)\\s-*$" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (> prev-indent top-level-body-indent)
             (string-match-p "\\(?:→\\|⟶\\)\\s-*$" prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p ",\\s-*\\_<by\\_>\\s-*$" prev-text-no-comment))
        (if open-delimited-body-indent
            (max (+ prev-indent step)
                 (+ open-delimited-body-indent step))
          (+ prev-indent step)))
       ((and prev-pos
             (memq (lean4-indent--pipe-left-tail-head-kind prev-text-no-comment prev-pos)
                   '(atom application)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-body-indent
             (memq top-level-body-intro-kind '(coloneq equals))
             (= prev-indent top-level-body-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (string-match-p "\\_<fun\\_>" prev-text-no-comment)
             (string-match-p ")\\s-*\\'" prev-text))
        (+ prev-indent step))
       ((and prev-pos
             open-delimited-body-indent
             (string-match-p "\\`[ \t]*⟨\\S-" prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment)))
        (+ open-delimited-body-indent step))
       ((and prev-pos
             open-delimited-body-indent
             (> prev-indent open-delimited-body-indent)
             (string-match-p "\\`[ \t]*(\\S-" prev-text-no-comment)
             (not (lean4-indent--inside-filter-upwards-bracket-block-p prev-pos))
             (not (lean4-indent--projection-application-tail-line-p prev-text-no-comment))
             (not (lean4-indent--paren-led-application-tail-line-p prev-text-no-comment))
             (not (lean4-indent--projection-head-line-p prev-text-no-comment))
             (not (string-match-p "<|\\s-*$" prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        prev-indent)
       ((and prev-pos
             open-delimited-body-indent
             (= prev-indent open-delimited-body-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (not (plist-get top-level-context :body-intro-pos))
             (>= prev-indent top-level-body-indent))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (memq top-level-body-intro-kind '(coloneq equals))
             (= prev-indent top-level-body-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-body-indent
             (memq top-level-body-intro-kind '(coloneq equals))
             (= prev-indent top-level-body-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-body-indent
             (memq top-level-body-intro-kind '(coloneq equals))
             (= prev-indent top-level-body-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (or (not (string-match-p "\\_<fun\\_>" prev-text-no-comment))
                 (lean4-indent--application-continues-after-inline-fun-p
                  prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (= prev-indent top-level-body-indent)
             (lean4-indent--line-starts-with-paren-p prev-text-no-comment)
             (string-match-p "\\S-" prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (> prev-indent top-level-body-indent)
             anchor-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom)
             (memq (lean4-indent--line-body-intro-kind anchor-text-no-comment)
                   '(equals coloneq)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (> prev-indent top-level-body-indent)
             anchor-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (or (lean4-indent--operator-led-continuation-line-p anchor-text-no-comment)
                 (lean4-indent--line-ends-with-op-p anchor-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'colon)
             (> prev-indent top-level-body-indent))
        (+ prev-indent step))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (= prev-indent (- top-level-body-indent step))
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment))
        (+ top-level-body-indent step))
       ((and prev-pos
             (= prev-indent 0)
             (lean4-indent--starts-with-p prev-text "\\_<deriving\\_>"))
        step)
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (eq top-level-body-intro-kind 'where)
             (= prev-indent top-level-body-indent)
             (string-match-p "[])}⟩]\\s-*$" prev-text))
        top-level-body-indent)
       ((and prev-pos
             (lean4-indent--line-contains-calc-p prev-text)
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment)
             (lean4-indent--inequality-rhs-column prev-text-no-comment))
        (1- (lean4-indent--inequality-rhs-column prev-text-no-comment)))
       ((and prev-pos
             open-delimited-body-indent
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|let\\|suffices\\)\\_>.*:\\s-*\\S-+"
              prev-text-no-comment)
             (or prev-anonymous-typed-local-decl-p
                 (not prev-line-has-outer-coloneq)))
        (max (+ open-delimited-body-indent step)
             (+ prev-indent (* 2 step))))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|let\\|suffices\\)\\_>.*:\\s-*\\S-+"
              prev-text-no-comment)
             (or prev-anonymous-typed-local-decl-p
                 (not prev-line-has-outer-coloneq)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (eq (lean4-indent--tactic-term-tail-head-kind prev-text-no-comment)
                 'application)
             (not (string-match-p "\\`[ \t]*·\\s-*\\(?:exact\\|refine\\|apply\\)\\_>"
                                  prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (memq top-level-body-intro-kind '(by coloneq-by))
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--line-starts-with-quantifier-and-ends-with-comma-p
              anchor-text-no-comment)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|suffices\\)\\_>.*,\\s-*\\'"
              prev-text-no-comment)
             (not prev-line-has-outer-coloneq))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|suffices\\)\\_>.*=\\s-*\\'"
              prev-text-no-comment)
             (not prev-line-has-outer-coloneq))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|suffices\\)\\_>.*\\_<from\\_>\\s-*\\'"
              prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "\\_<using\\_>\\s-*\\'" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|suffices\\)\\_>.*\\(?:→\\|->\\)\\s-*\\'"
              prev-text-no-comment)
             (not prev-line-has-outer-coloneq))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (string-match-p "\\`[ \t]*simp\\?\\_>.*\\_<says\\_>\\s-*\\'"
                             prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "\\`[ \t]*change\\_>\\s-+\\S-" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "\\`[ \t]*convert\\_>\\s-+\\S-" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "\\`[ \t]*cond\\_>\\s-+\\S-" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\_<induction\\_>.*\\_<generalizing\\_>.*\\_<with\\_>\\s-*\\'"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:haveI\\|letI\\)\\_>.*:=\\s-*\\S-+"
              prev-text-no-comment))
        prev-indent)
       ((and prev-pos
             (string-match-p "<|\\s-*$" prev-text-no-comment)
             (>= (lean4-indent--leading-paren-count prev-text) 2))
        (+ prev-indent (* 5 step)))
       ((and prev-pos
             (string-match-p "<;>\\s-*\\'" prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p "<;>" prev-text-no-comment)
             (not (string-match-p "\\`[ \t]*<;>" prev-text-no-comment))
             (not (string-match-p "<;>\\s-*\\'" prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p ";\\s-*·\\s-*\\S-" prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        prev-indent)
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|let\\)\\_>.*:=\\s-*\\S-+"
              prev-text-no-comment)
             (lean4-indent--projection-head-line-p prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*·\\s-*\\(?:rw\\|simp_rw\\)\\_>.*\\[[^]]*\\'"
              prev-text-no-comment))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:rw\\|simp_rw\\)\\_>.*\\[[^]]*\\'"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*simp\\s-+only\\_>.*\\[[^]]*\\]\\s-*\\'"
              prev-text-no-comment)
             (not (string-match-p "\\_<at\\_>" prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--line-ends-with-left-arrow-p prev-text-no-comment))
       (+ prev-indent step))
       ((and prev-pos
             (string-match-p "<|\\s-*$" prev-text-no-comment))
        (if (string-match-p "\\`[ \t]*(+" prev-text-no-comment)
            (+ prev-indent (* 3 step))
          (+ prev-indent step)))
       ((and prev-pos
             (string-match-p "\\`[ \t]*<|\\s-+\\S-" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "]\\s-*$" prev-text-no-comment)
             (lean4-indent--inside-filter-upwards-bracket-block-p prev-pos))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment))
        (if (lean4-indent--calc-relation-simple-rhs-p prev-text-no-comment)
            (+ prev-indent (* 3 step))
          (or (and (lean4-indent--calc-relation-rhs-column prev-text-no-comment)
                   (+ (lean4-indent--calc-relation-rhs-column prev-text-no-comment)
                      (* 3 step)))
              (+ prev-indent (* 2 step)))))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--inline-coloneq-by-complete-p prev-text-no-comment))
        prev-indent)
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--inline-coloneq-by-starter-p prev-text-no-comment))
        (or (lean4-indent--inline-coloneq-by-tail-column prev-text-no-comment)
            (and (lean4-indent--calc-relation-rhs-column prev-text-no-comment)
                 (+ (lean4-indent--calc-relation-rhs-column prev-text-no-comment)
                    (* 3 step)))
            (+ prev-indent (* 2 step))))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (not (lean4-indent--line-starts-with-relop-p prev-text))
             (not (lean4-indent--line-starts-with-calc-step-p prev-text))
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment)
             (lean4-indent--inequality-rhs-column prev-text-no-comment))
        (+ (lean4-indent--inequality-rhs-column prev-text-no-comment) step))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (not (lean4-indent--line-starts-with-relop-p prev-text))
             (not (lean4-indent--line-starts-with-calc-step-p prev-text))
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment))
        (let ((body-indent (lean4-indent--calc-block-body-indent prev-pos prev-indent step)))
          (if body-indent
              (+ body-indent step)
            (+ prev-indent (* 2 step)))))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (not (lean4-indent--line-starts-with-relop-p prev-text))
             (not (lean4-indent--line-starts-with-calc-step-p prev-text))
             (lean4-indent--inline-coloneq-by-complete-p prev-text-no-comment))
        (let ((body-indent (lean4-indent--calc-block-body-indent prev-pos prev-indent step)))
          (if body-indent
              (+ body-indent step)
            (+ prev-indent step))))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*·\\s-*\\(?:have\\|let\\)\\_>.*:=\\s-*\\_<by\\_>\\s-*$"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:rcases\\|obtain\\)\\_>.*\\_<have\\_>.*:=\\s-*\\S-+"
              prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*if\\_>.*\\_<then\\_>\\s-+\\S-+"
              prev-text-no-comment))
        prev-indent)
       ((and prev-pos
             (lean4-indent--line-ends-with-by-p prev-text-no-comment)
             (not (string-match-p "<|\\s-*\\_<by\\_>\\s-*$"
                                  prev-text-no-comment))
             (not (string-match-p "\\(?:↦\\|=>\\)\\s-*by\\s-*$"
                                  prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "<|\\s-*\\_<by\\_>\\s-*$" prev-text-no-comment)
             (lean4-indent--line-starts-with-paren-p prev-text))
       (+ prev-indent (* 3 step)))
       ((and prev-pos
             (string-match-p "<|\\s-*\\_<by\\_>\\s-*$" prev-text-no-comment))
       (+ prev-indent (* 2 step)))
       ((and prev-pos
             calc-inline-expression-col
             (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-calc)
             (not calc-relation-col))
        (+ calc-inline-expression-col step))
       ((and prev-pos
             embedded-calc-expression-col
             (lean4-indent--line-contains-calc-p prev-text)
             (not calc-relation-col))
        (+ embedded-calc-expression-col step))
       ((and prev-pos
             (or (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-calc)
                 (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*\\(?:have\\|let\\)\\_>.*:=\\s-*\\_<by\\_>\\s-*$"
              prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             open-paren-pos-prev
             (string-match-p "\\(?:↦\\|=>\\)\\s-*by\\s-*$"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*classical\\_>.*\\_<\\(?:exact\\|refine\\|apply\\)\\_>[ \t]*\\'"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*·\\s-*exact\\_>\\s-+\\(?:@\\)?[[:word:]_'.]+\\(?:\\.[[:word:]_'.]+\\)+\\s-*\\'"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--line-ends-with-coloneq-p prev-text-no-comment))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--line-ends-with-op-p prev-text-no-comment))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (lean4-indent--line-opens-paren-p prev-pos))
        (+ prev-indent (* 5 step)))
       ((and prev-pos
             (lean4-indent--starts-with-p prev-text lean4-indent--re-starts-calc))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--line-ends-with-with-p prev-pos))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--proof-at-target-line-p prev-text-no-comment))
        anchor-indent)
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--proof-standalone-at-line-p prev-text-no-comment))
        anchor-indent)
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*·\\s-*\\(?:exact\\|refine\\|apply\\)\\_>\\s-*\\'"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*·\\s-*intro\\_>.*)\\s-*$"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--focus-dot-line-p prev-text)
             (not (lean4-indent--bare-tactic-term-intro-line-p prev-text-no-comment))
             (not (string-match-p
                   "\\`[ \t]*·\\s-*\\(?:exact\\|refine\\|apply\\)\\_>"
                   prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (or (lean4-indent--focus-dot-line-p anchor-text)
                 (lean4-indent--branch-line-p anchor-text))
             (or (lean4-indent--ordinary-proof-tactic-line-p prev-text-no-comment)
                 (lean4-indent--proof-standalone-at-line-p prev-text-no-comment))
             (not (lean4-indent--bare-tactic-term-intro-line-p prev-text-no-comment))
             (not (string-match-p
                   "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:exact\\|refine\\|apply\\)\\_>\\s-+\\S-"
                   prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        prev-indent)
       ((and prev-pos
             (lean4-indent--proof-with-continuation-head-line-p
              prev-text-no-comment)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (not (string-match-p
                   "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:cases\\|rcases\\|obtain\\)\\_>\\s-+\\S-+\\s-*\\'"
                   prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--proof-with-continuation-head-line-p
              prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "\\`[ \t]*show\\_>.*\\_<from\\_>\\s-*$"
                             prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--proof-with-line-p prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*obtain\\_>.*:\\s-*\\S-"
              prev-text-no-comment)
             (lean4-indent--line-ends-with-comma-p prev-text-no-comment)
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--proof-block-opener-line-p prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment)
             (string-match-p
              "\\`[ \t]*\\(?:·\\s-*\\)?\\(?:have\\|let\\|suffices\\)\\_>"
              anchor-text-no-comment))
        (+ anchor-indent (* 2 step)))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--operator-led-continuation-line-p prev-text-no-comment))
        prev-indent)
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--line-ends-with-op-p prev-text-no-comment)
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment))
             (not (lean4-indent--in-calc-block-p prev-pos))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--filter-upwards-line-p prev-text-no-comment))
        (if (and open-delimited-body-indent
                 (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
            (max (+ open-delimited-body-indent step)
                 (if open-paren-col (1+ open-paren-col) 0))
          (+ prev-indent step)))
       ((and prev-pos
             prevprev-pos
             (memq (lean4-indent--line-body-intro-kind prevprev-text-no-comment)
                   '(coloneq equals))
             (> prevprev-indent prev-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             prevprev-pos
             (memq (lean4-indent--line-body-intro-kind prevprev-text-no-comment)
                   '(coloneq equals))
             (> prev-indent prevprev-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (string-match-p "\\`[ \t]*obtain\\_>.*:=\\s-*\\'" anchor-text-no-comment)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             prevprev-pos
             (> prev-indent prevprev-indent)
             (string-match-p "[])}⟩]\\s-*$" prev-text)
             (string-match-p "\\`[ \t]*\\(?:rw\\|simp\\(?:_rw\\| only\\)?\\)\\_>.*\\["
                             prevprev-text-no-comment)
             (lean4-indent--line-ends-with-comma-p prevprev-text-no-comment))
        (max 0 (- prev-indent (* 2 step))))
       ((and prev-pos
             (lean4-indent--inside-filter-upwards-bracket-block-p prev-pos)
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (or (lean4-indent--bare-tactic-term-intro-line-p prev-text-no-comment)
                 (lean4-indent--qualified-apply-line-p prev-text-no-comment)
                 (string-match-p "\\`[ \t]*apply_rules\\_>" prev-text-no-comment)))
        (if (string-match-p "\\`[ \t]*refine\\_>\\s-*\\'" prev-text-no-comment)
            (+ prev-indent (* 2 step))
          (+ prev-indent step)))
       ((and prev-pos
             (let ((case-fold-search nil))
               (string-match-p
                "\\`[ \t]*apply\\_>\\s-+\\(?:@\\)?[[:word:]_'.]+\\(?:\\.ext\\|_ext\\)\\_>"
                prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*apply\\_>\\s-+(config\\s-*:=.+)\\s-+\\S-"
              prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p "\\`[ \t]*\\(?:exact\\|refine\\)\\_>\\s-+\\S-" prev-text-no-comment))
        (cond
         ((and (string-match-p "\\`[ \t]*refine\\_>.*⟨.*[,،]\\s-*$"
                               prev-text-no-comment)
               (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
          (+ prev-indent (* 2 step)))
         ((and (string-match-p "\\`[ \t]*refine\\_>" prev-text-no-comment)
               open-delimited-body-indent
               (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                   'application)
               (not (lean4-indent--line-starts-with-paren-p prev-text)))
          (+ open-delimited-body-indent step))
         ((and (string-match-p "\\`[ \t]*refine\\_>" prev-text-no-comment)
               (eq (lean4-indent--tactic-term-tail-head-kind prev-text-no-comment) 'atom))
          (+ prev-indent (* 2 step)))
         (t
          (+ prev-indent step))))
       ((and prev-pos
             (string-match-p "\\`[ \t]*·\\s-*exact\\_>.*\\_<by\\_>\\s-*$"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p "\\`[ \t]*·\\s-*exact\\_>\\s-*\\'"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p "\\`[ \t]*·\\s-*\\(?:refine\\|apply\\)\\_>\\s-*\\'"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (string-match-p "\\`[ \t]*·\\s-*exact\\_>\\s-+\\S-"
                             prev-text-no-comment))
        (if (or (eq (lean4-indent--tactic-term-tail-head-kind prev-text-no-comment)
                    'application)
                (and (lean4-indent--line-opens-paren-p prev-pos)
                     (not (string-match-p "[])}⟩]\\s-*$" prev-text))
                     (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
                     (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
                     (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
                (let ((case-fold-search nil))
                  (string-match-p
                   "\\`[ \t]*·\\s-*exact\\_>\\s-+\\(?:@\\(?:_root_\\.\\|[[:upper:]][[:word:]_'.]*\\(?:\\.\\|\\_>\\)\\)\\|_root_\\.\\|[[:upper:]][[:word:]_'.]*\\(?:\\.\\|\\_>\\)\\)"
                   prev-text-no-comment)))
            (+ prev-indent (* 2 step))
          (+ prev-indent step)))
       ((and prev-pos
             (string-match-p "\\`[ \t]*·\\s-*\\(?:refine\\|apply\\)\\_>\\s-+\\S-"
                             prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (memq (lean4-indent--tactic-term-tail-head-kind prev-text-no-comment)
                   '(application)))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--branch-line-p anchor-text)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom)
             (not (lean4-indent--in-calc-block-p prev-pos))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (eq (lean4-indent--line-body-intro-kind prev-text-no-comment) 'fat-arrow)
             (string-match-p "\\`[ \t]*(+" prev-text)
             (not (lean4-indent--line-starts-with-paren-and-closes-p prev-pos)))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             open-delimited-body-indent
             (> prev-indent open-delimited-body-indent)
             (lean4-indent--projection-application-tail-line-p prev-text-no-comment)
             (not (lean4-indent--in-calc-block-p prev-pos))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             open-delimited-body-indent
             (> prev-indent open-delimited-body-indent)
             (lean4-indent--paren-led-application-tail-line-p prev-text-no-comment)
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (or (lean4-indent--operator-led-continuation-line-p anchor-text-no-comment)
                 (lean4-indent--line-ends-with-op-p anchor-text-no-comment))
             (not (lean4-indent--in-calc-block-p prev-pos))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (or (lean4-indent--operator-led-continuation-line-p anchor-text-no-comment)
                 (lean4-indent--line-ends-with-op-p anchor-text-no-comment))
             (not (lean4-indent--in-calc-block-p prev-pos))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom)
             (memq (lean4-indent--line-body-intro-kind anchor-text-no-comment)
                   '(coloneq fat-arrow fun-arrow equals
                     open-brace where))
             (not (lean4-indent--in-calc-block-p prev-pos))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (eq (lean4-indent--line-body-intro-kind anchor-text-no-comment)
                 'coloneq)
             (not (lean4-indent--line-top-level-declaration-head-p
                   anchor-text-no-comment))
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom))
        (+ prev-indent step))
       ((and prev-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom)
             (string-match-p "\\." prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom)
             (let ((case-fold-search nil))
               (string-match-p
                "\\`[ \t]*\\(?:@\\(?:_root_\\.\\|[[:upper:]][[:word:]_'.]*\\(?:\\.\\|\\_>\\)\\)\\|_root_\\.\\|[[:upper:]][[:word:]_'.]*\\(?:\\.\\|\\_>\\)\\)"
                prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'atom)
             (string-match-p "\\`[ \t]*@[[:word:]_.']+\\_>\\s-+\\S-"
                             prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (let ((case-fold-search nil))
               (string-match-p
                "\\`[ \t]*\\(?:@\\(?:_root_\\.\\|[[:upper:]][[:word:]_'.]*\\(?:\\.\\|\\_>\\)\\)\\|_root_\\.\\|[[:upper:]][[:word:]_'.]*\\(?:\\.\\|\\_>\\)\\)"
                prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (string-match-p "\\`[ \t]*@[[:word:]_.']+\\_>\\s-+\\S-"
                             prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*(+@?[[:word:]_'.]+\\(?:\\.[[:word:]_'.]+\\)+\\s-*$"
              prev-text-no-comment)
             (not (string-match-p "<|" prev-text-no-comment))
             (not (lean4-indent--in-calc-block-p prev-pos)))
        (+ prev-indent (* 3 step)))
       ((and prev-pos
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (lean4-indent--pipe-left-tail-head-kind prev-text-no-comment prev-pos)
             (not (lean4-indent--in-calc-block-p prev-pos)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (not (lean4-indent--in-calc-block-p prev-pos))
             anchor-pos
             (> prev-indent anchor-indent)
             (string-match-p "\\`[ \t]*(+" anchor-text)
             (not (string-match-p "<|" anchor-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (not (lean4-indent--in-calc-block-p prev-pos))
             anchor-pos
             (> prev-indent anchor-indent)
             (memq (lean4-indent--line-application-head-kind anchor-text-no-comment)
                   '(atom application)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--projection-head-line-p prev-text-no-comment)
             (not (lean4-indent--in-calc-block-p prev-pos)))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--inline-open-paren-argument-column prev-text)
             open-paren-pos-prev-on-line
             (not (lean4-indent--in-calc-block-p prev-pos))
             (> prev-indent (max (or top-level-body-indent 0) anchor-indent))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-by-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment))
             (not (string-match-p "\\(?:↦\\|=>\\)\\s-*by\\s-*$"
                                  prev-text-no-comment)))
        (max (+ prev-indent step)
             (+ (lean4-indent--inline-open-paren-argument-column prev-text) step)))
       ((and prev-pos
             prev-delimited-sibling-indent
             (string-match-p "[])}⟩]\\s-*$" prev-text))
        prev-delimited-sibling-indent)
       ((and prev-pos
             open-delimited-body-indent
             (lean4-indent--line-ends-with-comma-p prev-text-no-comment)
             (lean4-indent--inline-open-paren-argument-column prev-text))
        (max (+ open-delimited-body-indent step)
             (+ (lean4-indent--inline-open-paren-argument-column prev-text) step)))
       ((and prev-pos
             open-delimited-body-indent
             (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
        (max (+ open-delimited-body-indent step)
             (if open-paren-col (1+ open-paren-col) 0)))
       ((and prev-pos
             open-paren-pos-prev
             open-paren-prefix-has-real-text)
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             top-level-body-indent
             (eq (plist-get top-level-context :kind) 'declaration)
             (memq top-level-body-intro-kind '(coloneq where))
             (= prev-indent top-level-body-indent)
             (string-match-p "[])}⟩]\\s-*$" prev-text))
        top-level-body-indent)
       ((and prev-pos
             (lean4-indent--line-starts-with-paren-p prev-text)
             (lean4-indent--paren-led-bare-head-line-p prev-text-no-comment)
             open-paren-pos-prev-on-line
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             open-delimited-body-indent
             (eq (lean4-indent--line-application-head-kind prev-text-no-comment)
                 'application)
             (not (lean4-indent--line-starts-with-paren-p prev-text))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ open-delimited-body-indent step))
       ((and open-delimited-body-indent
             prev-pos
             (not (and (lean4-indent--line-starts-with-paren-p prev-text)
                       (not (lean4-indent--line-body-intro-kind prev-text-no-comment))
                       (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
                       (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (string-match-p "[])}⟩]\\s-*$" prev-text)))
        open-delimited-body-indent)
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (lean4-indent--line-starts-with-relop-p prev-text)
             (lean4-indent--projection-application-tail-line-p prev-text-no-comment)
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent (* 3 step) 1))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (or (lean4-indent--line-starts-with-calc-step-p prev-text)
                 (lean4-indent--line-starts-with-relop-p prev-text))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment)))
        (let ((body-indent (lean4-indent--calc-block-body-indent prev-pos prev-indent step)))
          (if body-indent
              (+ body-indent step)
            (+ prev-indent (* 2 step)))))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             (lean4-indent--line-ends-with-coloneq-by-p prev-text-no-comment)
             (not (lean4-indent--line-starts-with-calc-step-p prev-text))
             (not (lean4-indent--line-starts-with-relop-p prev-text)))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--in-calc-block-p prev-pos)
             anchor-pos
             (> prev-indent anchor-indent)
             (not (lean4-indent--line-starts-with-calc-step-p prev-text))
             (not (lean4-indent--line-starts-with-relop-p prev-text))
             (not (lean4-indent--line-starts-with-closing-p prev-text))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (let ((body-indent (lean4-indent--calc-block-body-indent prev-pos prev-indent step)))
          (if body-indent
              (+ body-indent step)
            (+ prev-indent step))))
       ((and prev-pos
             (string-match-p
              "\\`[ \t]*·\\s-*\\(?:exact\\|refine\\|apply\\)\\_>\\s-*\\'"
              prev-text-no-comment))
        (+ prev-indent (* 2 step)))
       ((and prev-pos
             (lean4-indent--focus-dot-line-p prev-text)
             (not (lean4-indent--bare-tactic-term-intro-line-p prev-text-no-comment))
             (not (string-match-p
                   "\\`[ \t]*·\\s-*\\(?:exact\\|refine\\|apply\\)\\_>"
                   prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (memq top-level-body-intro-kind '(by coloneq-by))
             (string-match-p "\\_<first\\_>\\s-*$" prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (or (lean4-indent--focus-dot-line-p anchor-text)
                 (lean4-indent--branch-line-p anchor-text))
             (not (lean4-indent--line-starts-with-closing-p prev-text))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        prev-indent)
       ((and prev-pos
             top-level-body-indent
             (eq top-level-body-intro-kind 'do)
             (> prev-indent top-level-body-indent)
             (not (lean4-indent--line-starts-with-paren-p prev-text))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        top-level-body-indent)
       ((and prev-pos
             (string-match-p "[])}⟩]\\s-*$" prev-text)
             anchor-pos
             (lean4-indent--open-paren-pos-at-eol anchor-pos))
        (+ anchor-indent step))
       ((and prev-pos
             anchor-pos
             (> prev-indent anchor-indent)
             (lean4-indent--bare-tactic-term-intro-line-p anchor-text-no-comment)
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment)))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--line-starts-with-quantifier-and-ends-with-comma-p
              prev-text-no-comment))
        (+ prev-indent step))
       ((and prev-pos
             (lean4-indent--line-starts-with-paren-p prev-text)
             (not (lean4-indent--line-body-intro-kind prev-text-no-comment))
             (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
             (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment)))
        (+ prev-indent step))
       (t nil)))))

(defun lean4-indent--prefer-base-indent-over-newline-helper-p ()
  "Return non-nil when blank-line helper should defer to base indentation.

This handles lines that begin with a balanced parenthesized term whose
continuation should align with an outer sibling rather than an inner
already-closed parenthesized argument."
  (when (lean4-indent--line-blank-p (lean4-indent--line-text (point)))
    (let ((prev-pos (lean4-indent--prev-nonblank)))
      (when prev-pos
        (let* ((prev-text (lean4-indent--line-text prev-pos))
               (prev-text-no-comment
                (if (lean4-indent--comment-line-p prev-pos)
                    ""
                  (lean4-indent--line-text-no-comment prev-pos)))
               (prev-indent (lean4-indent--line-indent prev-pos))
               (open-prev (lean4-indent--open-paren-pos-at-eol prev-pos))
               (top-level-context
                (lean4-indent--top-level-context prev-pos lean4-indent-offset))
               (top-level-body-indent
                (plist-get top-level-context :body-indent))
               (top-level-body-intro-kind
                (plist-get top-level-context :body-intro-kind))
               (open-prev-on-line
                (and open-prev
                     (save-excursion
                       (goto-char prev-pos)
                       (<= (line-beginning-position)
                           open-prev
                           (line-end-position))))))
          (and (lean4-indent--line-starts-with-paren-p prev-text)
               (string-match-p "[])}⟩]\\s-*$" prev-text)
               (not open-prev-on-line)
               (not (and top-level-context
                         (eq (plist-get top-level-context :kind) 'declaration)
                         top-level-body-indent
                         (or (eq top-level-body-intro-kind 'colon)
                             (and (> prev-indent top-level-body-indent)
                                  (not top-level-body-intro-kind)))))
               (not (lean4-indent--projection-head-line-p prev-text-no-comment))
               (not (lean4-indent--projection-application-tail-line-p prev-text-no-comment))
               (not (lean4-indent--line-ends-with-op-p prev-text-no-comment))
               (not (lean4-indent--line-ends-with-comma-p prev-text-no-comment))
               (not (lean4-indent--line-body-intro-kind prev-text-no-comment))))))))

(defun lean4-indent--prefer-newline-helper-over-base-p ()
  "Return non-nil when blank-line helper is more structurally faithful than base."
  (when (lean4-indent--line-blank-p (lean4-indent--line-text (point)))
    (let ((prev-pos (lean4-indent--prev-nonblank)))
      (when prev-pos
        (let* ((prev-text-no-comment
                (if (lean4-indent--comment-line-p prev-pos)
                    ""
                  (lean4-indent--line-text-no-comment prev-pos)))
               (prev-indent (lean4-indent--line-indent prev-pos))
               (sibling-indent
                (lean4-indent--find-prev-delimited-sibling-indent prev-pos prev-indent))
               (open-prev (lean4-indent--open-paren-pos-at-eol prev-pos)))
          (and sibling-indent
               (lean4-indent--projection-head-line-p prev-text-no-comment)
               (string-match-p "[])}⟩]\\.[[:word:]_'.]+\\s-*$" prev-text-no-comment)
               (not open-prev)))))))

(defun lean4-indent-line-function ()
  "Indent current line according to Lean 4 rules."
  (interactive)
  (let* ((current-pos (line-beginning-position))
         (current-text (lean4-indent--line-text current-pos)))
    (cond
     ((lean4-indent--starts-with-p current-text "\\_<end\\_>")
      (let ((eolp (eolp))
            (target (if (lean4-indent--prev-nonblank)
                        (lean4-indent--find-end-anchor-indent current-pos)
                      0)))
        (indent-line-to (max 0 target))
        (when eolp
          (end-of-line))))
     ((or (lean4-indent--comment-line-p current-pos)
          (lean4-indent--string-line-p current-pos))
        (let ((eolp (eolp)))
          (indent-line-to (current-indentation))
          (when eolp
            (end-of-line)))
      )
     (t
     (let* ((newline-computed (lean4-indent--newline-blank-line-indent))
             (base-computed (lean4-indent--compute-indent))
             (newline-prev-pos
              (and newline-computed
                   (save-excursion (lean4-indent--prev-nonblank))))
             (newline-prev-indent
              (and newline-prev-pos
                   (lean4-indent--line-indent newline-prev-pos)))
             (computed (if (and newline-computed
                                (not (and (> newline-computed base-computed)
                                          newline-prev-indent
                                          (>= base-computed newline-prev-indent)
                                          (lean4-indent--prefer-base-indent-over-newline-helper-p))))
                           (if (and (< newline-computed base-computed)
                                    (lean4-indent--prefer-newline-helper-over-base-p))
                               newline-computed
                             (max newline-computed base-computed))
                         base-computed))
             (current (current-indentation))
             (tabp (eq this-command 'indent-for-tab-command))
             (target (cond
                      ((and tabp (eq last-command 'indent-for-tab-command))
                       (lean4-indent--cycle-indent computed current))
                      ((and (not tabp)
                            (not newline-computed)
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
          (end-of-line)))))))

(defun lean4-indent-region-function (start end)
  "Indent nonblank lines between START and END using Lean 4 rules.

This is a region-aware companion to `lean4-indent-line-function'.  It uses a
single forward pass to cache enclosing top-level declaration context, avoiding
repeated whole-declaration rescans near the end of large files."
  (let ((end-marker (copy-marker end))
        (lean4-indent--region-line-contexts
         (lean4-indent--build-region-line-context-cache))
        (lean4-indent--region-top-level-contexts
         nil)
        (lean4-indent--preserve-tactic-region-indentation t))
    (setq lean4-indent--region-top-level-contexts
          (lean4-indent--build-top-level-context-cache lean4-indent-offset))
    (save-excursion
      (goto-char start)
      (unless (bolp)
        (forward-line 1))
      (while (< (point) end-marker)
        (unless (or (looking-at-p "[ \t]*$")
                    (lean4-indent--string-line-p (point))
                    (lean4-indent--comment-line-cached-p (point))
                    (and (= (current-indentation) 0)
                         (lean4-indent--line-structural-top-level-anchor-p
                          (point)))
                    (lean4-indent--stable-region-shallow-top-level-anchor-line-p)
                    (lean4-indent--stable-region-body-line-p))
          (funcall indent-line-function))
        (forward-line 1)))))

(provide 'lean4-indent)
;;; lean4-indent.el ends here
