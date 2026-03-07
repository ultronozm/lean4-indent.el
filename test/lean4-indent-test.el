;;; lean4-indent-test.el --- Tests for lean4 indentation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'lean4-syntax)
(require 'lean4-indent)

(defmacro lean4-test-with-indent-buffer (contents &rest body)
  "Evaluate BODY in a temporary Lean indentation test buffer seeded with CONTENTS."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (set-syntax-table lean4-syntax-table)
     (setq-local indent-tabs-mode nil)
     (setq-local indent-line-function #'lean4-indent-line-function)
     ,@body))

(defun lean4-test--line-string ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lean4-test--goto-eob ()
  "Move point to the end of the last line of CONTENTS.

Many test fixtures end with a trailing newline.  In that case,
`point-max' is at BOL of the next (empty) line, but we want to act on
the previous line, similar to how Vim's `G` targets the last content
line in these specs."
  (goto-char (point-max))
  (when (and (bolp) (not (bobp)))
    (forward-line -1))
  (end-of-line))

(defun lean4-test--open-line-below ()
  "Like Vim's `o`: go to EOL, insert newline, stay on the new line."
  (end-of-line)
  (insert "\n"))

(defun lean4-test--insert-and-indent-line (text)
  "Insert TEXT on the current line and indent the line."
  (insert text)
  (funcall #'lean4-indent-line-function))

(defun lean4-test--insert-line-below-and-indent (text)
  "Insert TEXT on a new line below point, then indent that line."
  (lean4-test--open-line-below)
  (lean4-test--insert-and-indent-line text))

(defun lean4-test--insert-lines-and-assert (pairs)
  "Insert each line from PAIRS and assert its indentation.

PAIRS should be a list of (TEXT EXPECTED) entries."
  (dolist (pair pairs)
    (lean4-test--insert-line-below-and-indent (car pair))
    (should (equal (lean4-test--line-string) (cadr pair)))))

(defun lean4-test--tab-indent ()
  "Call `indent-line-function' as if invoked by `indent-for-tab-command'."
  (let ((this-command 'indent-for-tab-command))
    (funcall indent-line-function)))

(defun lean4-test--goto-line (n)
  "Move point to line N (1-based)."
  (goto-char (point-min))
  (forward-line (1- n)))

(ert-deftest lean4-indent--predicate-branch-line ()
  (should (lean4-indent--branch-line-p "  | .ok x => x"))
  (should-not (lean4-indent--branch-line-p "  .ok x => x"))
  (should-not (lean4-indent--branch-line-p "  |>.map foo")))

(ert-deftest lean4-indent--predicate-focus-dot ()
  (should (lean4-indent--focus-dot-line-p "· exact h"))
  (should (lean4-indent--focus-dot-line-p "  · exact h"))
  (should-not (lean4-indent--focus-dot-line-p "  . exact h")))

(ert-deftest lean4-indent--predicate-macro-rules ()
  (should (lean4-indent--macro-rules-line-p "macro_rules"))
  (should (lean4-indent--macro-rules-line-p "scoped macro_rules"))
  (should-not (lean4-indent--macro-rules-line-p "macro_rule")))

(ert-deftest lean4-indent--predicate-label-colon ()
  (should (lean4-indent--label-colon-line-p "FLAGS:"))
  (should-not (lean4-indent--label-colon-line-p "Flags:")))

(ert-deftest lean4-indent--predicate-operator-continuation ()
  (should (lean4-indent--operator-continuation-p "  foo +"))
  (should-not (lean4-indent--operator-continuation-p "  foo := by"))
  (should-not (lean4-indent--operator-continuation-p "  foo :=")))

(ert-deftest lean4-indent--balanced-bracket-ignores-comment ()
  (lean4-test-with-indent-buffer
   (concat
    "-- [not a list]\n")
   (goto-char (point-min))
   (should-not (lean4-indent--line-contains-balanced-bracket-p (point)))))

(ert-deftest lean4-indent--balanced-bracket-ignores-string ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo := \"[not a list]\"\n")
   (goto-char (point-min))
   (should-not (lean4-indent--line-contains-balanced-bracket-p (point)))))

(ert-deftest lean4-indent--balanced-bracket-allows-quoted-comment ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo := by\n"
    "  simp [a, b] -- \"note\"\n")
   (goto-char (point-min))
   (forward-line 1)
   (should (lean4-indent--line-contains-balanced-bracket-p (point)))))

(ert-deftest lean4-indent--operator-continuation-with-comment ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  a + -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "b")
   (should (equal (lean4-test--line-string) "  b"))))

(ert-deftest lean4-indent--operator-continuation-breaks-on-comment-line ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  a +\n"
    "  -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "b")
   (should (equal (lean4-test--line-string) "  b"))))

(ert-deftest lean4-indent--coloneq-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo := -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "bar")
   (should (equal (lean4-test--line-string) "  bar"))))

(ert-deftest lean4-indent--colon-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo : -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--equals-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo = -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "bar")
   (should (equal (lean4-test--line-string) "    bar"))))

(ert-deftest lean4-indent--tab-does-not-move-point-in-text ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "    exact trivial\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 1)
   (end-of-line)
   (let ((last-command nil))
     (lean4-test--tab-indent))
   (should (eolp))))

(ert-deftest lean4-indent--tab-cycles-by-dedenting ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo [Ring R] [PartialOrder R]\n"
    "    [ZeroLEOneClass R] : True := by\n"
    "  trivial\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 2)
   (should (equal (lean4-test--line-string) "  trivial"))
   (let ((last-command nil))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "  trivial"))
   (let ((last-command 'indent-for-tab-command))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "trivial"))
   (let ((last-command 'indent-for-tab-command))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "  trivial"))))

(ert-deftest lean4-indent--tab-cycles-with-nonstandard-offset ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo : Nat :=\n"
    "    1\n")
   (setq lean4-indent-offset 4)
   (goto-char (point-min))
   (forward-line 1)
   (let ((last-command nil))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "    1"))
   (let ((last-command 'indent-for-tab-command))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "1"))
   (let ((last-command 'indent-for-tab-command))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "    1"))))

(ert-deftest lean4-indent--coloneq-by-ignores-commented-by ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo := -- by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("rfl" "  rfl")
      ("rfl" "  rfl")))))

(ert-deftest lean4-indent--by-ignores-commented-by ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  exact trivial -- by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("rfl" "  rfl")
      ("rfl" "  rfl")))))

(ert-deftest lean4-indent--then-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "if h then -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("trivial" "  trivial")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--else-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "else -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("trivial" "  trivial")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--do-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "do -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("trivial" "  trivial")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--fun-arrow-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "fun x ↦ -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("x" "  x")
      ("x" "  x")))))

(ert-deftest lean4-indent--comma-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  [1, -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("2]" "    2]")
      ("2]" "    2]")))))

(ert-deftest lean4-indent--comment-only-does-not-trigger-operator-continuation ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  1\n"
    "-- +\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("2" "  2")
      ("2" "  2")))))

(ert-deftest lean4-indent--tab-fixes-stale-indent-after-coloneq ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo : Nat :=\n"
    "        1\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 1)
   (let ((last-command nil))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "  1"))))

(ert-deftest lean4-indent--match-branches-align ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo (n : Nat) : Nat := by\n"
    "  match n with\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "| 0 => 0")
   (should (equal (lean4-test--line-string) "  | 0 => 0"))))

(ert-deftest lean4-indent--induction-branches-align ()
  (lean4-test-with-indent-buffer
   (concat
    "lemma foo (xs : List Nat) : True := by\n"
    "  induction xs with\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "| nil => trivial")
   (should (equal (lean4-test--line-string) "  | nil => trivial"))))

(ert-deftest lean4-indent--macro-rules-branches-align ()
  (lean4-test-with-indent-buffer "macro_rules\n"
                                 (setq lean4-indent-offset 2)
                                 (lean4-test--goto-eob)
                                 (lean4-test--insert-line-below-and-indent "| `(foo) => bar")
                                 (should (equal (lean4-test--line-string) "  | `(foo) => bar"))))

(ert-deftest lean4-indent--macro-rules-branches-align-after-comment ()
  (lean4-test-with-indent-buffer
   (concat
    "macro_rules\n"
    "-- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "| `(foo) => bar")
   (should (equal (lean4-test--line-string) "  | `(foo) => bar"))))

(ert-deftest lean4-indent--macro-rules-inline-branches-align ()
  (lean4-test-with-indent-buffer
   (concat
    "macro_rules | `(tactic| use_discharger) => `(tactic| rfl)\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "macro_rules | `(tactic| use_discharger) => `(tactic| assumption)")
   (should (equal (lean4-test--line-string)
                  "macro_rules | `(tactic| use_discharger) => `(tactic| assumption)"))))

(ert-deftest lean4-indent--scoped-macro-rules-branches-align ()
  (lean4-test-with-indent-buffer "scoped macro_rules\n"
                                 (setq lean4-indent-offset 2)
                                 (lean4-test--goto-eob)
                                 (lean4-test--insert-line-below-and-indent "| `(foo) => bar")
                                 (should (equal (lean4-test--line-string) "  | `(foo) => bar"))))

(ert-deftest lean4-indent--scoped-macro-rules-inline-align ()
  (lean4-test-with-indent-buffer
   (concat
    "scoped macro_rules | `([$l,*]) => `(List.nil)\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "scoped macro_rules | `([$l,*]) => `(List.cons 1 [])")
   (should (equal (lean4-test--line-string)
                  "scoped macro_rules | `([$l,*]) => `(List.cons 1 [])"))))

(ert-deftest lean4-indent--wrapped-declaration-where-body ()
  (lean4-test-with-indent-buffer
   (concat
    "lemma IsOrderedRing.of_mul_nonneg [Ring R] [PartialOrder R]\n"
    "    [ZeroLEOneClass R] (mul_nonneg : ∀ a b : R, 0 ≤ a → 0 ≤ b → 0 ≤ a * b) :\n"
    "    IsOrderedRing R where\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "mul_le_mul_of_nonneg_left := by")
   (should (equal (lean4-test--line-string) "      mul_le_mul_of_nonneg_left := by"))))

(ert-deftest lean4-indent--wrapped-declaration-where-continuation ()
  (lean4-test-with-indent-buffer
   (concat
    "lemma foo :\n"
    "    True\n"
    "    where\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "bar := by")
   (should (equal (lean4-test--line-string) "      bar := by"))))

(ert-deftest lean4-indent--wrapped-declaration-where-inline ()
  (lean4-test-with-indent-buffer
   (concat
    "instance [Add α] {a b : Thunk α} (εa εb : Type*) :\n"
    "    EstimatorData (a + b) (εa × εb) where\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "bound e := by")
   (should (equal (lean4-test--line-string) "      bound e := by"))))

(ert-deftest lean4-indent--have-continues-arguments ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_int_term1 : IntervalIntegrable\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("(fun x ↦ True) volume a b := by"
       "      (fun x ↦ True) volume a b := by")
      ("exact trivial"
       "    exact trivial")))))

(ert-deftest lean4-indent--colon-continues-from-anchor ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have hu_cont :\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("True" "      True")))))

(ert-deftest lean4-indent--wrapped-declaration-colon-continues ()
  (lean4-test-with-indent-buffer
   (concat
    "lemma foo [Ring R]\n"
    "    [ZeroLEOneClass R] :\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("True := by" "    True := by")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--wrapped-declaration-after-bare-coloneq ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo [Ring R] [PartialOrder R]\n"
    "    [ZeroLEOneClass R] : True :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("by" "  by")
      ("trivial" "    trivial")))))

(ert-deftest lean4-indent--wrapped-declaration-by-body ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo [Ring R] [PartialOrder R]\n"
    "    [ZeroLEOneClass R] : True := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("trivial" "  trivial")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--calc-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo : True := by\n"
    "  calc\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True := by")
   (should (equal (lean4-test--line-string) "    True := by"))))

(ert-deftest lean4-indent--calc-multiline-rhs-indents-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo : True := by\n"
    "  calc\n"
    "    True := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "      exact trivial"))))

(ert-deftest lean4-indent--fun-arrow-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h := fun x ↦\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("True" "    True")
      ("True" "    True")))))

(ert-deftest lean4-indent--have-multiline-type-and-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_int_term2 : IntervalIntegrable\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "(fun x ↦ True) volume a b := by")
   (should (equal (lean4-test--line-string)
                  "      (fun x ↦ True) volume a b := by"))
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "    exact trivial"))))

(ert-deftest lean4-indent--label-colon-indents-one-level ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo : Cmd := `[Cli|\n"
    "  FLAGS:\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "output : String")
   (should (equal (lean4-test--line-string) "    output : String"))))

(ert-deftest lean4-indent--label-colon-multi-block ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo : Cmd := `[Cli|\n"
    "  FLAGS:\n"
    "    output : String; \"path\"\n"
    "\n"
    "  ARGS:\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "arg : Nat")
   (should (equal (lean4-test--line-string) "    arg : Nat"))))

(ert-deftest lean4-indent--multiline-definition-and-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :\n"
    "    Nat → Nat\n"
    "    := by\n"
    "  intro n\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact n")
   (should (equal (lean4-test--line-string) "  exact n"))))

(ert-deftest lean4-indent--mathlib-freealgebra-induction ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem induction {motive : FreeAlgebra R X → Prop}\n"
    "    (grade0 : ∀ r, motive (algebraMap R (FreeAlgebra R X) r)) (grade1 : ∀ x, motive (ι R x))\n"
    "    (mul : ∀ a b, motive a → motive b → motive (a * b))\n"
    "    (add : ∀ a b, motive a → motive b → motive (a + b))\n"
    "    (a : FreeAlgebra R X) : motive a := by\n"
    "  let s : Subalgebra R (FreeAlgebra R X) :=\n"
    "    { carrier := motive\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("mul_mem' := mul _ _" "      mul_mem' := mul _ _")
      ("add_mem' := add _ _" "      add_mem' := add _ _")
      ("algebraMap_mem' := grade0 }" "      algebraMap_mem' := grade0 }")
      ("let of : X → s := Subtype.coind (ι R) grade1" "  let of : X → s := Subtype.coind (ι R) grade1")
      ("have of_id : AlgHom.id R (FreeAlgebra R X) = s.val.comp (lift R of) := by"
       "  have of_id : AlgHom.id R (FreeAlgebra R X) = s.val.comp (lift R of) := by")
      ("ext" "    ext")
      ("simp [of, Subtype.coind]" "    simp [of, Subtype.coind]")
      ("suffices a = lift R of a by" "  suffices a = lift R of a by")
      ("rw [this]" "    rw [this]")
      ("exact Subtype.prop (lift R of a)" "    exact Subtype.prop (lift R of a)")
      ("simp only [AlgHom.ext_iff, AlgHom.coe_id, id_eq, AlgHom.coe_comp, Subalgebra.coe_val,"
       "  simp only [AlgHom.ext_iff, AlgHom.coe_id, id_eq, AlgHom.coe_comp, Subalgebra.coe_val,")
      ("Function.comp_apply] at of_id" "    Function.comp_apply] at of_id")
      ("exact of_id a" "  exact of_id a")))))

(ert-deftest lean4-indent--mul-mem-inline-brace ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  let s : Subalgebra R (FreeAlgebra R X) := { carrier := motive\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "mul_mem' := mul _ _")
   (should (equal (lean4-test--line-string) "    mul_mem' := mul _ _"))))

(ert-deftest lean4-indent--inline-brace-newline-after-motive ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  let s : Subalgebra R (FreeAlgebra R X) :=\n"
    "    { carrier := motive\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (search-forward "motive")
   (newline)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "      "))))

(ert-deftest lean4-indent--simp-bracket-newline-keeps-indent ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h : True := by\n"
    "    simp [of, Subtype.coind]\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--open-line-below)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "    "))))

(ert-deftest lean4-indent--freealgebra-fun-classical-newlines ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem ι_injective [Nontrivial R] : Function.Injective (ι R : X → FreeAlgebra R X) :=\n"
    "  fun x y hoxy ↦\n"
    "  by_contradiction <| by\n"
    "    classical exact fun hxy : x ≠ y ↦\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (goto-char (point-min))
   (search-forward "fun x y hoxy ↦")
   (newline)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "  "))
   (search-forward "classical exact fun hxy : x ≠ y ↦")
   (newline)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "        "))))

(ert-deftest lean4-indent--inline-by-after-fun-indents-two ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem algebraMap_leftInverse :\n"
    "    Function.LeftInverse algebraMapInv (algebraMap R <| FreeAlgebra R X) := fun x ↦ by\n"
    "  simp [algebraMapInv]\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (search-forward "by")
   (newline)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "  "))))

(ert-deftest lean4-indent--multiline-theorem-binders-indent ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem lemma_aachIBP_parts (σ : ℝ) (φ : ℝ → ℝ) (a b : ℝ) (hab : a < b) (ha_pos : 0 < a)\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent ": True := by")
   (should (equal (lean4-test--line-string) "    : True := by"))))

(ert-deftest lean4-indent--multiline-theorem-with-by-line ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo :\n"
    "    True\n"
    "    ∧ True\n"
    "    := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact ⟨trivial, trivial⟩")
   (should (equal (lean4-test--line-string) "  exact ⟨trivial, trivial⟩"))))

(ert-deftest lean4-indent--coloneq-by-after-continuation ()
  (lean4-test-with-indent-buffer
   (concat
    "lemma prod_range_diag_flip (n : ℕ) (f : ℕ → ℕ → M) :\n"
    "    (∏ m ∈ range n, ∏ k ∈ range (m + 1), f k (m - k)) =\n"
    "      ∏ m ∈ range n, ∏ k ∈ range (n - m), f m k := by\n"
    "  rw [prod_sigma', prod_sigma']\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (search-forward "by")
   (newline)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "  "))))

(ert-deftest lean4-indent--calc-line-coloneq-by-indents-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem sum_range_id_mul_two (n : ℕ) : (∑ i ∈ range n, i) * 2 = n * (n - 1) :=\n"
    "  calc\n"
    "    (∑ i ∈ range n, i) * 2 = (∑ i ∈ range n, i) + ∑ i ∈ range n, (n - 1 - i) := by\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (search-forward "by")
   (newline)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "      "))))

(ert-deftest lean4-indent--calc-next-step-by-ignores-nested-coloneq-in-prior-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    True = True := by\n"
    "      let x := True\n"
    "      exact x\n"
    "    _ = True := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "      exact trivial"))))

(ert-deftest lean4-indent--calc-next-step-by-ignores-nested-equals-in-prior-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    True = True := by\n"
    "      have h :\n"
    "          True =\n"
    "            True := by\n"
    "        exact rfl\n"
    "      exact h\n"
    "    _ = True := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "      exact trivial"))))

(ert-deftest lean4-indent--semicolon-operator-continuation ()
  (lean4-test-with-indent-buffer
   (concat
    "lemma prod_range_diag_flip (n : ℕ) (f : ℕ → ℕ → M) :\n"
    "    (∏ m ∈ range n, ∏ k ∈ range (m + 1), f k (m - k)) =\n"
    "      ∏ m ∈ range n, ∏ k ∈ range (n - m), f m k := by\n"
    "  refine prod_nbij' (fun a ↦ ⟨a.2, a.1 - a.2⟩) (fun a ↦ ⟨a.1 + a.2, a.1⟩) ?_ ?_ ?_ ?_ ?_ <;>\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "simp +contextual only [mem_sigma]")
   (should (equal (lean4-test--line-string) "    simp +contextual only [mem_sigma]"))))

(ert-deftest lean4-indent--focus-calc-indents-one-step ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  · calc\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "    exact trivial"))))

(ert-deftest lean4-indent--focus-have-by-indents-one-step ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  · have h : True := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "    exact trivial"))))

(ert-deftest lean4-indent--calc-operator-continues-one-step ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    True = True *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "      True"))))

(ert-deftest lean4-indent--calc-step-relop-p ()
  (should (lean4-indent--line-starts-with-calc-step-p "    _ ≤ foo"))
  (should (lean4-indent--line-starts-with-calc-step-p "    _ ≃ foo"))
  (should-not (lean4-indent--line-starts-with-calc-step-p "    _ != foo")))

(ert-deftest lean4-indent--calc-operator-continues-with-prod-line ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    (∏ x ∈ s, f x) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "∏ x ∈ s, f x := by")
   (should (equal (lean4-test--line-string) "      ∏ x ∈ s, f x := by"))))

(ert-deftest lean4-indent--calc-operator-continues-with-cast-mul-line ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = ((t ^ (-s.re) : ℝ) : ℂ) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "Complex.exp 0 := by")
   (should (equal (lean4-test--line-string) "        Complex.exp 0 := by"))))

(ert-deftest lean4-indent--calc-operator-continues-with-existing-line ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "        ((t ^ (-s.re) : ℝ) : ℂ) *\n"
    "          (Complex.exp 0) := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-line 4)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "          (Complex.exp 0) := by"))))

(ert-deftest lean4-indent--calc-mul-continuation-with-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = ((t ^ (-s.re) : ℝ) : ℂ) *\n"
    "        Complex.exp (2 * π * I * (ν * t - (s.im / (2 * π)) * Real.log t)) := by\n"
    "          congr 1\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-line 4)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string)
                  "        Complex.exp (2 * π * I * (ν * t - (s.im / (2 * π)) * Real.log t)) := by"))
   (lean4-test--goto-line 5)
   (funcall #'lean4-indent-line-function)
   (should (equal (lean4-test--line-string) "      congr 1"))))

(ert-deftest lean4-indent--calc-equals-continues-two-steps ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    (∏ x ∈ s, h (if hx : p x then f x hx else g x hx)) =\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "(∏ x ∈ s with p x, h (if hx : p x then f x hx else g x hx)) *")
   (should (equal (lean4-test--line-string)
                  "        (∏ x ∈ s with p x, h (if hx : p x then f x hx else g x hx)) *"))))

(ert-deftest lean4-indent--do-indents-after-withref ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  withRef binder do\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "match binder with")
   (should (equal (lean4-test--line-string) "    match binder with"))))

(ert-deftest lean4-indent--fat-arrow-with-comment-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  match stx with\n"
    "  | `(($a + $b = $n)) => -- Maybe this is too cute.\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "return processed |>.push (← `(⟨$a, $b⟩), ← `(Finset.Nat.antidiagonal $n))")
   (should (equal (lean4-test--line-string)
                  "    return processed |>.push (← `(⟨$a, $b⟩), ← `(Finset.Nat.antidiagonal $n))"))))

(ert-deftest lean4-indent--if-then-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  if h : ts.size = 1 then\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "return ts[0]")
   (should (equal (lean4-test--line-string) "    return ts[0]"))))

(ert-deftest lean4-indent--else-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  if h : ts.size = 1 then\n"
    "    return ts[0]\n"
    "  else\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "processed.foldrM (fun s p => `(SProd.sprod $(s.2) $p)) processed.back.2")
   (should (equal (lean4-test--line-string)
                  "    processed.foldrM (fun s p => `(SProd.sprod $(s.2) $p)) processed.back.2"))))

(ert-deftest lean4-indent--end-aligns-with-namespace ()
  (lean4-test-with-indent-buffer
   (concat
    "namespace Foo\n"
    "  instance : Module.Free R (FreeAlgebra R X) :=\n"
    "    .of_equiv equivMonoidAlgebraFreeMonoid.symm.toLinearEquiv\n"
    "end\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 3)
   (should (equal (lean4-test--line-string) "end"))))

(ert-deftest lean4-indent--end-aligns-with-mutual ()
  (lean4-test-with-indent-buffer
   (concat
    "mutual\n"
    "  def foo := 1\n"
    "end\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 2)
   (should (equal (lean4-test--line-string) "end"))))

(ert-deftest lean4-indent--closing-brace-aligns ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  { field := 1\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "}")
   (should (equal (lean4-test--line-string) "  }"))))

(ert-deftest lean4-indent--closing-angle-aligns ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  ⟨\n"
    "    1,\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "⟩")
   (should (equal (lean4-test--line-string) "  ⟩"))))

(ert-deftest lean4-indent--closing-angle-aligns-after-exact ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  exact ⟨\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "⟩")
   (should (equal (lean4-test--line-string) "        ⟩"))))

(ert-deftest lean4-indent--angle-literal-multiline ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  exact ⟨\n"
    "    rfl,\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "rfl")
   (should (equal (lean4-test--line-string) "    rfl"))))

(ert-deftest lean4-indent--top-level-snap-skips-namespace-comment ()
  (lean4-test-with-indent-buffer
   (concat
    "namespace Foo\n"
    "-- comment\n"
    "  open Bar\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 2)
   (should (equal (lean4-test--line-string) "  open Bar"))))

(ert-deftest lean4-indent--find-anchor-includes-comment-line ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  bar\n"
    "-- comment\n"
    "    baz\n")
   (goto-char (point-min))
   (forward-line 3)
   (let* ((pos (point))
          (anchor (lean4-indent--find-anchor pos (lean4-indent--line-indent pos))))
     (should anchor)
     (should (equal (lean4-indent--line-text (car anchor)) "-- comment"))
     (should (= (cdr anchor) 0)))))

(ert-deftest lean4-indent--where-aligns-with-decl ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem foo : True := by\n"
    "  exact True\n"
    "where\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 2)
   (should (equal (lean4-test--line-string) "where"))))

(ert-deftest lean4-indent--prod-dite-top-level-star-continuation ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem prod_dite {s : Finset ι} {p : ι → Prop} [DecidablePred p] (f : ∀ x : ι, p x → M)\n"
    "    (g : ∀ x : ι, ¬p x → M) :\n"
    "    ∏ x ∈ s, (if hx : p x then f x hx else g x hx) =\n"
    "      (∏ x : {x ∈ s | p x}, f x.1 (by simpa using (mem_filter.mp x.2).2)) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "∏ x : {x ∈ s | ¬p x}, g x.1 (by simpa using (mem_filter.mp x.2).2) := by")
   (should (equal (lean4-test--line-string)
                  "        ∏ x : {x ∈ s | ¬p x}, g x.1 (by simpa using (mem_filter.mp x.2).2) := by"))))

(ert-deftest lean4-indent--prod-dite-by-dedents-to-top-level ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem prod_dite {s : Finset ι} {p : ι → Prop} [DecidablePred p] (f : ∀ x : ι, p x → M)\n"
    "    (g : ∀ x : ι, ¬p x → M) :\n"
    "    ∏ x ∈ s, (if hx : p x then f x hx else g x hx) =\n"
    "      (∏ x : {x ∈ s | p x}, f x.1 (by simpa using (mem_filter.mp x.2).2)) *\n"
    "        ∏ x : {x ∈ s | ¬p x}, g x.1 (by simpa using (mem_filter.mp x.2).2) := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "simp [prod_apply_dite _ _ fun x => x]")
   (should (equal (lean4-test--line-string)
                  "  simp [prod_apply_dite _ _ fun x => x]"))))

(ert-deftest lean4-indent--calc-step-operator-continues-one-step ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = a *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "b := by")
   (should (equal (lean4-test--line-string) "        b := by"))))

(ert-deftest lean4-indent--prod-apply-dite-calc-step-1 ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem prod_apply_dite {p : ι → Prop} [DecidablePred p]\n"
    "    [DecidablePred fun x => ¬p x] (f : ∀ x : ι, p x → γ) (g : ∀ x : ι, ¬p x → γ) (h : γ → M) :\n"
    "    (∏ x ∈ s, h (if hx : p x then f x hx else g x hx)) =\n"
    "      (∏ x : {x ∈ s | p x}, h (f x.1 <| by simpa using (mem_filter.mp x.2).2)) *\n"
    "        ∏ x : {x ∈ s | ¬p x}, h (g x.1 <| by simpa using (mem_filter.mp x.2).2) :=\n"
    "  calc\n"
    "    (∏ x ∈ s, h (if hx : p x then f x hx else g x hx)) =\n"
    "        (∏ x ∈ s with p x, h (if hx : p x then f x hx else g x hx)) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "∏ x ∈ s with ¬p x, h (if hx : p x then f x hx else g x hx) :=")
   (should (equal (lean4-test--line-string)
                  "          ∏ x ∈ s with ¬p x, h (if hx : p x then f x hx else g x hx) :="))))

(ert-deftest lean4-indent--prod-apply-dite-calc-step-1-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = _ :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "(prod_filter_mul_prod_filter_not s p _).symm")
   (should (equal (lean4-test--line-string)
                  "      (prod_filter_mul_prod_filter_not s p _).symm"))))

(ert-deftest lean4-indent--prod-apply-dite-calc-step-2 ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = (∏ x : {x ∈ s | p x}, h (if hx : p x.1 then f x.1 hx else g x.1 hx)) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent
     "∏ x : {x ∈ s | ¬p x}, h (if hx : p x.1 then f x.1 hx else g x.1 hx) :=")
    (should (equal (lean4-test--line-string)
                   "        ∏ x : {x ∈ s | ¬p x}, h (if hx : p x.1 then f x.1 hx else g x.1 hx) :="))))

(ert-deftest lean4-indent--prod-apply-dite-calc-step-2-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = _ :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "congr_arg₂ _ (prod_attach _ _).symm (prod_attach _ _).symm")
   (should (equal (lean4-test--line-string)
                  "      congr_arg₂ _ (prod_attach _ _).symm (prod_attach _ _).symm"))))

(ert-deftest lean4-indent--calc-operator-continues-with-prod-line-2 ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = (∏ x : {x ∈ s | p x}, h (if hx : p x.1 then f x.1 hx else g x.1 hx)) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "∏ x : {x ∈ s | ¬p x}, h (if hx : p x.1 then f x.1 hx else g x.1 hx) :=")
   (should (equal (lean4-test--line-string)
                  "        ∏ x : {x ∈ s | ¬p x}, h (if hx : p x.1 then f x.1 hx else g x.1 hx) :="))))

(ert-deftest lean4-indent--prod-apply-dite-calc-step-3 ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = (∏ x : {x ∈ s | p x}, h (f x.1 <| by simpa using (mem_filter.mp x.2).2)) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "∏ x : {x ∈ s | ¬p x}, h (g x.1 <| by simpa using (mem_filter.mp x.2).2) :=")
   (should (equal (lean4-test--line-string)
                  "        ∏ x : {x ∈ s | ¬p x}, h (g x.1 <| by simpa using (mem_filter.mp x.2).2) :="))))

(ert-deftest lean4-indent--prod-apply-dite-calc-step-3-proof ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    _ = _ :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "congr_arg₂ _ (prod_congr rfl fun x _hx ↦")
   (should (equal (lean4-test--line-string)
                  "      congr_arg₂ _ (prod_congr rfl fun x _hx ↦"))))

(ert-deftest lean4-indent--prod-apply-dite-top-level-coloneq-snippet ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem prod_apply_dite {p : ι → Prop} [DecidablePred p]\n"
    "    [DecidablePred fun x => ¬p x] (f : ∀ x : ι, p x → γ) (g : ∀ x : ι, ¬p x → γ) (h : γ → M) :\n"
    "    (∏ x ∈ s, h (if hx : p x then f x hx else g x hx)) =\n"
    "      (∏ x : {x ∈ s | p x}, h (f x.1 <| by simpa using (mem_filter.mp x.2).2)) *\n"
    "        ∏ x : {x ∈ s | ¬p x}, h (g x.1 <| by simpa using (mem_filter.mp x.2).2) :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "calc")
   (should (equal (lean4-test--line-string) "  calc"))))

(ert-deftest lean4-indent--calc-coloneq-continuation-uses-calc-step-indent ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    (∏ x ∈ s, h (if hx : p x then f x hx else g x hx)) =\n"
    "        (∏ x ∈ s with p x, h (if hx : p x then f x hx else g x hx)) *\n"
    "          ∏ x ∈ s with ¬p x, h (if hx : p x then f x hx else g x hx) :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "(prod_filter_mul_prod_filter_not s p _).symm")
   (should (equal (lean4-test--line-string)
                  "      (prod_filter_mul_prod_filter_not s p _).symm"))))

(ert-deftest lean4-indent--star-operator-continues-term ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h := a *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "b")
   (should (equal (lean4-test--line-string) "    b"))))

(ert-deftest lean4-indent--star-operator-does-not-continue-at-star ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  simp at *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "exact trivial")
   (should (equal (lean4-test--line-string) "  exact trivial"))))

(ert-deftest lean4-indent--star-operator-continues-mathlib-multiplication ()
  (lean4-test-with-indent-buffer
   (concat
    "have h2 (n : ℕ) : cos (b ^ (n + m) * π * x) =\n"
    "      (-1) ^ (⌊b ^ m * x + 2⁻¹⌋) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "cos (b ^ n * (b ^ m * x - ⌊b ^ m * x + 2⁻¹⌋) * π) := by")
   (should (equal (lean4-test--line-string)
                  "        cos (b ^ n * (b ^ m * x - ⌊b ^ m * x + 2⁻¹⌋) * π) := by"))))

(ert-deftest lean4-indent--star-operator-continues-paren-line ()
  (lean4-test-with-indent-buffer
   (concat
    "        ((t : ℂ) ^ ((-s).re : ℂ)) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "(Complex.exp (((-s).im * I) * Real.log t)) := by")
   (should (equal (lean4-test--line-string)
                  "          (Complex.exp (((-s).im * I) * Real.log t)) := by"))))

(ert-deftest lean4-indent--calc-coloneq-dedents-to-calc ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  calc\n"
    "    True = True :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "by")
   (should (equal (lean4-test--line-string) "      by"))))

(ert-deftest lean4-indent--open-paren-continues-args ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  exact congr_arg₂ _ (foo\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "bar")
   (should (equal (lean4-test--line-string) "    bar"))))

(ert-deftest lean4-indent--paren-line-not-closed-indents-under-opener ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :=\n"
    "  (bar :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "baz")
   (should (equal (lean4-test--line-string) "    baz"))))

(ert-deftest lean4-indent--mathlib-suffices-convert-simp ()
  (lean4-test-with-indent-buffer
   (concat
    "theorem two_nsmul_lie_lmul_lmul_add_eq_lie_lmul_lmul_add [IsCommJordan A] (a b : A) :\n"
    "    2 • (⁅L a, L (a * b)⁆ + ⁅L b, L (b * a)⁆) = ⁅L (a * a), L b⁆ + ⁅L (b * b), L a⁆ := by\n"
    "  suffices 2 • ⁅L a, L (a * b)⁆ + 2 • ⁅L b, L (b * a)⁆ + ⁅L b, L (a * a)⁆ + ⁅L a, L (b * b)⁆ = 0 by\n"
    "    rwa [← sub_eq_zero, ← sub_sub, sub_eq_add_neg, sub_eq_add_neg, lie_skew, lie_skew, nsmul_add]\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent
    "convert (commute_lmul_lmul_sq (a + b)).lie_eq using 1")
   (should (equal (lean4-test--line-string)
                  "  convert (commute_lmul_lmul_sq (a + b)).lie_eq using 1"))
   (lean4-test--insert-line-below-and-indent
    "simp only [add_mul, mul_add, map_add, lie_add, add_lie, mul_comm b a,")
   (should (equal (lean4-test--line-string)
                  "  simp only [add_mul, mul_add, map_add, lie_add, add_lie, mul_comm b a,"))
   (lean4-test--insert-line-below-and-indent
    "(commute_lmul_lmul_sq a).lie_eq, (commute_lmul_lmul_sq b).lie_eq, zero_add, add_zero, two_smul]")
   (should (equal (lean4-test--line-string)
                  "    (commute_lmul_lmul_sq a).lie_eq, (commute_lmul_lmul_sq b).lie_eq, zero_add, add_zero, two_smul]"))
   (lean4-test--insert-line-below-and-indent "abel")
   (should (equal (lean4-test--line-string) "  abel"))))

(ert-deftest lean4-indent--have-coloneq-continues ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h :=\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("True" "    True")
      ("True" "    True")))))

(ert-deftest lean4-indent--have-eq-continues ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_lhs :\n"
    "      True =\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("True := by" "      True := by")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--have-multiline-term-operator-continuation ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_lhs : IntervalIntegrable\n"
    "      (fun x ↦ x /)\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("(2 * x) := by" "      (2 * x) := by")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--let-uprime-continuation-line-keeps-split-indent ()
  (lean4-test-with-indent-buffer
   (concat
    "private lemma neg_u'_mul_v_eq (σ : ℝ) (φ : ℝ → ℝ) (x : ℝ) :\n"
    "    let u' : ℝ → ℂ := fun t ↦ ((-σ * t ^ (-σ - 1) : ℝ) : ℂ) /\n"
    "      (2 * π * I * ↑(deriv φ t)) +\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-line 3)
   (let ((last-command nil))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string)
                  "      (2 * π * I * ↑(deriv φ t)) +"))))

(ert-deftest lean4-indent--let-uprime-eq-line-dedents-after-let-chain ()
  (lean4-test-with-indent-buffer
   (concat
    "private lemma neg_u'_mul_v_eq (σ : ℝ) (φ : ℝ → ℝ) (x : ℝ) :\n"
    "    let u' : ℝ → ℂ := fun t ↦ ((-σ * t ^ (-σ - 1) : ℝ) : ℂ) /\n"
    "      (2 * π * I * ↑(deriv φ t)) +\n"
    "      ((t ^ (-σ) : ℝ) : ℂ) *\n"
    "        (-↑(deriv (deriv φ) t) / (2 * π * I * ↑(deriv φ t) ^ 2));\n"
    "    let v : ℝ → ℂ := fun t ↦ e (φ t);\n"
    "    -(u' x * v x) =\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-line 7)
   (let ((last-command nil))
     (lean4-test--tab-indent))
   (should (equal (lean4-test--line-string) "    -(u' x * v x) ="))))

(ert-deftest lean4-indent--let-uprime-cj-after-plus-keeps-let-body-indent ()
  (lean4-test-with-indent-buffer
   (concat
    "private lemma neg_u'_mul_v_eq (σ : ℝ) (φ : ℝ → ℝ) (x : ℝ) :\n"
    "    let u' : ℝ → ℂ := fun t ↦ ((-σ * t ^ (-σ - 1) : ℝ) : ℂ) /\n"
    "      (2 * π * I * ↑(deriv φ t)) +\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-line 3)
   (lean4-test--insert-line-below-and-indent "((t ^ (-σ) : ℝ) : ℂ) *")
   (should (equal (lean4-test--line-string)
                  "      ((t ^ (-σ) : ℝ) : ℂ) *"))))

(ert-deftest lean4-indent--let-uprime-cj-after-star-indents-one-step ()
  (lean4-test-with-indent-buffer
   (concat
    "private lemma neg_u'_mul_v_eq (σ : ℝ) (φ : ℝ → ℝ) (x : ℝ) :\n"
    "    let u' : ℝ → ℂ := fun t ↦ ((-σ * t ^ (-σ - 1) : ℝ) : ℂ) /\n"
    "      (2 * π * I * ↑(deriv φ t)) +\n"
    "      ((t ^ (-σ) : ℝ) : ℂ) *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-line 4)
   (lean4-test--insert-line-below-and-indent
    "(-↑(deriv (deriv φ) t) / (2 * π * I * ↑(deriv φ t) ^ 2));")
   (should (equal (lean4-test--line-string)
                  "        (-↑(deriv (deriv φ) t) / (2 * π * I * ↑(deriv φ t) ^ 2));"))))

(ert-deftest lean4-indent--let-star-continuation-indents-one-step ()
  (lean4-test-with-indent-buffer
   (concat
    "example : Nat := by\n"
    "  let x := a *\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "b")
   (should (equal (lean4-test--line-string) "    b"))))

(ert-deftest lean4-indent--let-semicolon-keeps-chain-indent ()
  (lean4-test-with-indent-buffer
   (concat
    "example : Nat := by\n"
    "  let x := foo;\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "let y := bar")
   (should (equal (lean4-test--line-string) "  let y := bar"))))

(ert-deftest lean4-indent--operator-continuation-non-ascii ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h : True := by\n"
    "    exact True ∧\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-iso ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h : True := by\n"
    "    exact True ≅\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-equiv ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h : True := by\n"
    "    exact True ≃\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-iso-equalizerproducts ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h : True := by\n"
    "    exact True ≅\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-equiv-derangements ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h : True := by\n"
    "    exact True ≃\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "True")
   (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--paren-continuation-aligns-to-open-paren ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_lhs : IntervalIntegrable\n"
    "      (fun x ↦ (x /)\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("(2 * x)) := by" "        (2 * x)) := by")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--paren-aligns-after-closing-paren ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  exact (\n"
    "    (pre := fun x => x)\n"
    "    (mid := fun x => x)\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "(post := fun x => x))")
   (should (equal (lean4-test--line-string) "    (post := fun x => x))"))))

(ert-deftest lean4-indent--deep-paren-continuation-matches-example ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_int_term2 : IntervalIntegrable\n"
    "      (fun x ↦ (x ^ (-σ) : ℝ) * (deriv (deriv φ) x) /\n"
    "        (2 * π * I * (deriv φ x) ^ 2) * e (φ x)) volume a b := by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("exact trivial" "    exact trivial")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--nested-paren-continuation-ibp-example ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h_int_term2 : IntervalIntegrable\n"
    "      (fun x ↦ ((x ^ (-σ) : ℝ) : ℂ) * ↑(deriv (deriv φ) x) /\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("(2 * π * I * ↑(deriv φ x) ^ 2) * e (φ x)) volume a b := by"
       "        (2 * π * I * ↑(deriv φ x) ^ 2) * e (φ x)) volume a b := by")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--nested-paren-continuation-mathlib-refine ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  classical\n"
    "  refine\n"
    "    (congr rfl (ext fun x => ?_)).mp\n"
    "      (((h.image_comp_equiv (Equiv.Set.sumCompl (range f))).image_comp_equiv\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("(Equiv.sumCongr (Equiv.ofInjective f f.injective)"
       "         (Equiv.sumCongr (Equiv.ofInjective f f.injective)")
      ("(Fintype.equivFin (↥(range f)ᶜ)).symm)).image_comp_sumInl_fin"
       "           (Fintype.equivFin (↥(range f)ᶜ)).symm)).image_comp_sumInl_fin")
      ("_)" "      _)")))))

(ert-deftest lean4-indent--nested-paren-continuation-mathlib-log-deriv ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have : True := by\n"
    "    have h := (by\n"
    "      have : HasDerivAt\n"
    "        (fun x ↦ 1 / 2 * log ((1 + x) / (1 - x)) - (∑ i ∈ range n, x ^ (2 * i + 1) / (2 * i + 1)))\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("((y ^ 2) ^ n / (1 - y ^ 2)) y := by"
       "        ((y ^ 2) ^ n / (1 - y ^ 2)) y := by")
      ("exact trivial" "        exact trivial")
      ("exact trivial" "        exact trivial")))))

(ert-deftest lean4-indent--nested-paren-continuation-mathlib-estimator ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  theorem Estimator.improveUntilAux_spec (a : Thunk α) (p : α → Bool)\n"
    "      [Estimator a ε] [WellFoundedGT (range (bound a : ε → α))] (e : ε) (r : Bool) :\n"
    "      match Estimator.improveUntilAux a p e r with\n"
    "      | .error _ => ¬ p a.get\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("| .ok e' => p (bound a e') := by"
       "      | .ok e' => p (bound a e') := by")
      ("exact trivial" "    exact trivial")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--match-branch-body-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "example : Nat := by\n"
    "  match 0 with\n"
    "  | 0 =>\n"
    "      by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("exact 0" "        exact 0")
      ("exact 0" "        exact 0")))))

(ert-deftest lean4-indent--structure-where-fields ()
  (lean4-test-with-indent-buffer
   (concat
    "structure Foo where\n"
    "  /-- doc -/\n"
    "  x : Nat\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent "y : Nat")
   (should (equal (lean4-test--line-string) "  y : Nat"))))

(ert-deftest lean4-indent--instance-where-fields ()
  (lean4-test-with-indent-buffer
   (concat
    "instance : Inhabited Nat where\n"
    "  default := 0\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-line-below-and-indent " := 1")
   (should (equal (lean4-test--line-string) "  := 1"))))

(ert-deftest lean4-indent--multiline-have-with-body ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  have h :\n"
    "      True := by\n"
    "    trivial\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("exact trivial" "    exact trivial")
      ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--declaration-type-continues ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo :\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("Nat := by" "    Nat := by")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--termination-by-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo (n : Nat) : Nat := by\n"
    "  exact n\n"
    "termination_by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("n" "  n")
      ("n" "  n")))))

(ert-deftest lean4-indent--decreasing-by-indents ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo (n : Nat) : Nat := by\n"
    "  exact n\n"
    "decreasing_by\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("simp" "  simp")
      ("simp" "  simp")))))

(ert-deftest lean4-indent--never-indent-does-not-reset-nested ()
  (lean4-test-with-indent-buffer
   (concat
    "namespace Foo\n"
    "  def bar : Nat := by\n"
    "    trivial\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (forward-line 1)
   (funcall indent-line-function)
   (should (equal (lean4-test--line-string) "  def bar : Nat := by"))))

(ert-deftest lean4-indent--top-level-section-snaps-left ()
  (lean4-test-with-indent-buffer "  section Foo\n"
                                 (setq lean4-indent-offset 2)
                                 (goto-char (point-min))
                                 (funcall indent-line-function)
                                 (should (equal (lean4-test--line-string) "section Foo"))))

(ert-deftest lean4-indent--top-level-public-section-snaps-left ()
  (lean4-test-with-indent-buffer "  public section\n"
                                 (setq lean4-indent-offset 2)
                                 (goto-char (point-min))
                                 (funcall indent-line-function)
                                 (should (equal (lean4-test--line-string) "public section"))))

(ert-deftest lean4-indent--top-level-section-trivial-snaps-left ()
  (lean4-test-with-indent-buffer "  section trivial\n"
                                 (setq lean4-indent-offset 2)
                                 (goto-char (point-min))
                                 (funcall indent-line-function)
                                 (should (equal (lean4-test--line-string) "section trivial"))))

(ert-deftest lean4-indent--top-level-namespace-snaps-left ()
  (lean4-test-with-indent-buffer "  namespace Foo\n"
                                 (setq lean4-indent-offset 2)
                                 (goto-char (point-min))
                                 (funcall indent-line-function)
                                 (should (equal (lean4-test--line-string) "namespace Foo"))))

(ert-deftest lean4-indent--top-level-anchor-compile-inductive ()
  (lean4-test-with-indent-buffer "  compile_inductive Foo\n"
                                 (setq lean4-indent-offset 2)
                                 (goto-char (point-min))
                                 (funcall indent-line-function)
                                 (should (equal (lean4-test--line-string) "compile_inductive Foo"))))

(ert-deftest lean4-indent--top-level-anchor-partial-fixpoint ()
  (lean4-test-with-indent-buffer "  partial_fixpoint Foo\n"
                                 (setq lean4-indent-offset 2)
                                 (goto-char (point-min))
                                 (funcall indent-line-function)
                                 (should (equal (lean4-test--line-string) "partial_fixpoint Foo"))))

(ert-deftest lean4-indent--top-level-anchor-open-universe-attribute ()
  (lean4-test-with-indent-buffer
   (concat
    "  open Foo\n"
    "  universe u\n"
    "  attribute [simp] Foo.bar\n")
   (setq lean4-indent-offset 2)
   (goto-char (point-min))
   (funcall indent-line-function)
   (should (equal (lean4-test--line-string) "open Foo"))
   (forward-line 1)
   (funcall indent-line-function)
   (should (equal (lean4-test--line-string) "universe u"))
   (forward-line 1)
   (funcall indent-line-function)
   (should (equal (lean4-test--line-string) "attribute [simp] Foo.bar"))))

(ert-deftest lean4-indent--dedents-after-sorry-with-comment ()
  (lean4-test-with-indent-buffer
   (concat
    "example : 37 = 37 ∧ 73 = 73 := by\n"
    "  sorry -- comment\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("#check 37" "#check 37")
      ("#check 73" "#check 73")))))

(ert-deftest lean4-indent--dedents-after-double-indented-type ()
  (lean4-test-with-indent-buffer "example :\n"
                                 (setq lean4-indent-offset 2)
                                 (lean4-test--goto-eob)
                                 (lean4-test--insert-line-below-and-indent "2 = 2 :=")
                                 (lean4-test--insert-lines-and-assert
                                  '(("rfl" "  rfl")
                                    ("rfl" "  rfl")))
                                 (goto-char (point-min))
                                 (forward-line 1)
                                 (should (equal (lean4-test--line-string) "    2 = 2 :="))))

(ert-deftest lean4-indent--top-level-anchor-word-boundary ()
  (should-not (lean4-indent--line-top-level-anchor-p "define foo := 1"))
  (should (lean4-indent--line-top-level-anchor-p "def foo := 1")))

(ert-deftest lean4-indent--block-comment-recovery ()
  (lean4-test-with-indent-buffer
   (concat
    "def foo := by\n"
    "  /-\n"
    "  comment\n"
    "  -/\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("exact rfl" "  exact rfl")
      ("exact rfl" "  exact rfl")))))

(ert-deftest lean4-indent--mutual-indents-defs ()
  (lean4-test-with-indent-buffer
   (concat
    "mutual\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("def foo : Nat := by" "  def foo : Nat := by")
      ("def bar : Nat := by" "  def bar : Nat := by")))))

(ert-deftest lean4-indent--branch-does-not-align-to-unrelated-with ()
  (lean4-test-with-indent-buffer
   (concat
    "example : True := by\n"
    "  simp [with]\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("| exact trivial" "  | exact trivial")
      ("| exact trivial" "  | exact trivial")))))

(ert-deftest lean4-indent--branch-ignores-inner-with ()
  (lean4-test-with-indent-buffer
   (concat
    "example : Nat := by\n"
    "  match x with\n"
    "  | a =>\n"
    "    match y with\n"
    "    | b =>\n"
    "      rfl\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("| c =>" "  | c =>")
      ("| c =>" "  | c =>")))))

(ert-deftest lean4-indent--else-if-indents-body ()
  (lean4-test-with-indent-buffer
   (concat
    "if h then\n"
    "  trivial\n"
    "else if h2 then\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("trivial" "  trivial")
      ("trivial" "  trivial")))))

(ert-deftest lean4-indent--inductive-branches-align ()
  (lean4-test-with-indent-buffer
   (concat
    "inductive Foo where\n")
   (setq lean4-indent-offset 2)
   (lean4-test--goto-eob)
   (lean4-test--insert-lines-and-assert
    '(("| bar" "  | bar")
      ("| baz" "  | baz")))))

(provide 'lean4-indent-test)
;;; lean4-indent-test.el ends here
