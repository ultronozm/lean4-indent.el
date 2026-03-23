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
     (setq-local lean4-indent-offset 2)
     (lean4-indent-setup-buffer)
     ,@body))

(defmacro lean4-define-final-line-indent-test (name contents)
  "Define NAME as a final-line indentation regression test for CONTENTS."
  (declare (indent 1) (debug (symbolp form)))
  `(ert-deftest ,name ()
     (lean4-test-with-indent-buffer ,contents
       (lean4-test--reindent-final-line-and-assert-same))))

(defun lean4-test--line-string ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lean4-test--reindent-final-line-and-assert-same ()
  "Reindent the final line of the current buffer and assert it is unchanged."
  (lean4-test--goto-eob)
  (let ((before (lean4-test--line-string)))
    (funcall #'lean4-indent-line-function)
    (should (equal (lean4-test--line-string) before))))

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

(defun lean4-test--newline-and-indent ()
  "Insert a newline and indent it as `newline-and-indent' would."
  (call-interactively #'newline-and-indent))

(defun lean4-test--newline-and-assert (expected)
  "Run `newline-and-indent' and assert the new line matches EXPECTED."
  (lean4-test--newline-and-indent)
  (should (equal (lean4-test--line-string) expected)))

(defun lean4-test--newline-lower-bound-and-assert ()
  "Run `newline-and-indent' and assert the new line is deep enough.

The inserted line must be indented at least as much as the original
following line in the test fixture."
  (let ((expected
         (save-excursion
           (forward-line 1)
           (current-indentation))))
    (lean4-test--newline-and-indent)
    (should (>= (current-indentation) expected))))

(defun lean4-test--newline-bounds-and-assert (min-indent max-indent)
  "Run `newline-and-indent' and assert the new line stays within bounds."
  (lean4-test--newline-and-indent)
  (should (>= (current-indentation) min-indent))
  (should (<= (current-indentation) max-indent)))

(defun lean4-test--newline-next-line-bounds-and-assert (max-indent)
  "Use the original next line as a lower bound and MAX-INDENT as an upper bound."
  (let ((min-indent
         (save-excursion
           (forward-line 1)
           (current-indentation))))
    (lean4-test--newline-bounds-and-assert min-indent max-indent)))

(defun lean4-test--open-line-below ()
  "Insert a newline below point without indenting it first."
  (end-of-line)
  (insert "\n"))

(defun lean4-test--insert-and-indent-line (text)
  "Insert TEXT on the current line and indent the line."
  (insert text)
  (funcall #'lean4-indent-line-function))

(defun lean4-test--insert-line-below-and-indent (text)
  "Insert TEXT on a new line below point, then reindent that line.

This models typing the new line's contents first and then invoking
`indent-line-function', which is different from raw `C-j' behavior."
  (lean4-test--open-line-below)
  (lean4-test--insert-and-indent-line text))

(defun lean4-test--insert-lines-and-assert (pairs)
  "Insert each line from PAIRS and assert its indentation.

PAIRS should be a list of (TEXT EXPECTED) entries."
  (dolist (pair pairs)
    (lean4-test--insert-line-below-and-indent (car pair))
    (should (equal (lean4-test--line-string) (cadr pair)))))

(defun lean4-test--indent-region-and-assert-same ()
  "Indent the whole buffer and assert it stays unchanged."
  (let ((before (buffer-string)))
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) before))))

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
  (should (lean4-indent--macro-rules-line-p "local macro_rules"))
  (should-not (lean4-indent--macro-rules-line-p "macro_rule")))

(ert-deftest lean4-indent--predicate-label-colon ()
  (should (lean4-indent--label-colon-line-p "FLAGS:"))
  (should-not (lean4-indent--label-colon-line-p "Flags:")))

(ert-deftest lean4-indent--predicate-operator-continuation ()
  (should (lean4-indent--operator-continuation-p "  foo +"))
  (should-not (lean4-indent--operator-continuation-p "  foo := by"))
  (should-not (lean4-indent--operator-continuation-p "  foo :=")))

(ert-deftest lean4-indent--predicate-body-intro-kind ()
  (should (eq (lean4-indent--line-body-intro-kind "foo :") 'colon))
  (should (eq (lean4-indent--line-body-intro-kind "foo := by") 'coloneq-by))
  (should (eq (lean4-indent--line-body-intro-kind "foo :=") 'coloneq))
  (should (eq (lean4-indent--line-body-intro-kind "by") 'by))
  (should (eq (lean4-indent--line-body-intro-kind "fun x ↦") 'fun-arrow))
  (should (eq (lean4-indent--line-body-intro-kind "have") 'bare-have-suffices))
  (should (eq (lean4-indent--line-body-intro-kind "suffices") 'bare-have-suffices))
  (should (eq (lean4-indent--line-body-intro-kind "termination_by") 'termination))
  (should-not (lean4-indent--line-body-intro-kind "List.recOn l")))

(ert-deftest lean4-indent--predicate-body-intro-precedence ()
  (should (lean4-indent--line-ends-with-colon-p "foo :"))
  (should-not (lean4-indent--line-ends-with-colon-p "foo :="))
  (should-not (lean4-indent--line-ends-with-colon-p "foo := by"))
  (should (lean4-indent--line-ends-with-coloneq-p "foo :="))
  (should-not (lean4-indent--line-ends-with-coloneq-p "foo := by"))
  (should (lean4-indent--line-ends-with-coloneq-by-p "foo := by")))

(ert-deftest lean4-indent--predicate-outer-coloneq ()
  (lean4-test-with-indent-buffer
      (concat
       "example : Nat := 2\n"
       "theorem foo (x : Nat) : Nat := x\n"
       "theorem bar (x : Nat)\n")
    (goto-char (point-min))
    (should (lean4-indent--line-has-outer-coloneq-p (point)))
    (forward-line 1)
    (should (lean4-indent--line-has-outer-coloneq-p (point)))
    (forward-line 1)
    (should-not (lean4-indent--line-has-outer-coloneq-p (point)))))

(ert-deftest lean4-indent--predicate-application-head-kind ()
  (should (eq (lean4-indent--line-application-head-kind "Iff.intro") 'atom))
  (should (eq (lean4-indent--line-application-head-kind "List.recOn l") 'application))
  (should-not (lean4-indent--line-application-head-kind "if"))
  (should-not (lean4-indent--line-application-head-kind "match"))
  (should-not (lean4-indent--line-application-head-kind "have"))
  (should-not (lean4-indent--line-application-head-kind "suffices")))

(ert-deftest lean4-indent--predicate-structured-term-start ()
  (should (lean4-indent--line-starts-structured-term-p "if"))
  (should (lean4-indent--line-starts-structured-term-p "match"))
  (should (lean4-indent--line-starts-structured-term-p "have"))
  (should (lean4-indent--line-starts-structured-term-p "(fun x => x)"))
  (should-not (lean4-indent--line-starts-structured-term-p "Iff.intro")))

(lean4-define-final-line-indent-test
 lean4-indent--bare-have-line-indents-continuation
 "example : True := by
  have
    foo := bar")

(lean4-define-final-line-indent-test
 lean4-indent--bare-suffices-line-indents-continuation
 "example : True := by
  suffices
    foo := bar")

(ert-deftest lean4-indent--indent-region-first-line-does-not-error ()
  (lean4-test-with-indent-buffer
      "example : True := by\n"
    (should
     (condition-case nil
         (progn
           (indent-region (point-min) (line-end-position))
           t)
       (error nil)))))

(ert-deftest lean4-indent--non-tab-reindent-preserves-dedented-tactic-line ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h : True := by\n"
       "    trivial\n"
       "  exact h\n")
    (lean4-test--goto-eob)
    (let ((before (lean4-test--line-string)))
      (funcall #'lean4-indent-line-function)
      (should (equal (lean4-test--line-string) before)))))

(ert-deftest lean4-indent--non-tab-reindent-preserves-dedented-focus-tactic-line ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sb_right_inv {x : α} (hx : x ∉ sbSet f g) : g (invFun g x) = x := by\n"
       "  have : x ∈ g '' univ := by\n"
       "    contrapose! hx\n"
       "    rw [sbSet, mem_iUnion]\n"
       "    use 0\n"
       "    rw [sbAux, mem_diff]\n"
       "    constructor\n"
       "    · exact trivial\n"
       "    · assumption\n")
    (lean4-test--goto-eob)
    (let ((before (lean4-test--line-string)))
      (funcall #'lean4-indent-line-function)
      (should (equal (lean4-test--line-string) before)))))

(ert-deftest lean4-indent--non-tab-reindent-preserves-mid-buffer-focus-tactic-line ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sb_right_inv {x : α} (hx : x ∉ sbSet f g) : g (invFun g x) = x := by\n"
       "  have : x ∈ g '' univ := by\n"
       "    contrapose! hx\n"
       "    rw [sbSet, mem_iUnion]\n"
       "    use 0\n"
       "    rw [sbAux, mem_diff]\n"
       "    constructor\n"
       "    · exact trivial\n"
       "    · assumption\n"
       "  have : ∃ y, g y = x := by\n"
       "    contrapose! hx\n"
       "    rw [sbSet, mem_iUnion]\n"
       "    use 0\n"
       "    rw [sbAux, mem_diff]\n"
       "    constructor\n"
       "    · exact trivial\n"
       "    simpa\n"
       "  rcases this with ⟨y, hy⟩\n"
       "  apply invFun_eq\n")
    (lean4-test--goto-line 9)
    (let ((before (lean4-test--line-string)))
      (funcall #'lean4-indent-line-function)
      (should (equal (lean4-test--line-string) before)))))

(ert-deftest lean4-indent--indent-region-preserves-complete-tactic-proof ()
  (let ((contents
         (concat
          "theorem sb_right_inv {x : α} (hx : x ∉ sbSet f g) : g (invFun g x) = x := by\n"
          "  have : x ∈ g '' univ := by\n"
          "    contrapose! hx\n"
          "    rw [sbSet, mem_iUnion]\n"
          "    use 0\n"
          "    rw [sbAux, mem_diff]\n"
          "    constructor\n"
          "    · exact trivial\n"
          "    · assumption\n"
          "  have : ∃ y, g y = x := by\n"
          "    contrapose! hx\n"
          "    rw [sbSet, mem_iUnion]\n"
          "    use 0\n"
          "    rw [sbAux, mem_diff]\n"
          "    constructor\n"
          "    · exact trivial\n"
          "    simpa\n"
          "  rcases this with ⟨y, hy⟩\n"
          "  apply invFun_eq\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-doc-and-bullets ()
  (let ((contents
         (concat
          "theorem mul_toSubmodule_le (S T : Subalgebra R A) :\n"
          "    Subalgebra.toSubmodule S * Subalgebra.toSubmodule T ≤ Subalgebra.toSubmodule (S ⊔ T) := by\n"
          "  rw [Submodule.mul_le]\n"
          "  intro y hy z hz\n"
          "  simp only [mem_toSubmodule]\n"
          "  exact mul_mem (Algebra.mem_sup_left hy) (Algebra.mem_sup_right hz)\n"
          "\n"
          "/-- As submodules, subalgebras are idempotent. -/\n"
          "@[simp]\n"
          "theorem isIdempotentElem_toSubmodule (S : Subalgebra R A) :\n"
          "    IsIdempotentElem S.toSubmodule := by\n"
          "  apply le_antisymm\n"
          "  · refine (mul_toSubmodule_le _ _).trans_eq ?_\n"
          "    rw [sup_idem]\n"
          "  · intro x hx1\n"
          "    rw [← mul_one x]\n"
          "    exact Submodule.mul_mem_mul hx1 (show (1 : A) ∈ S from one_mem S)\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-doc-before-protected-def ()
  (let ((contents
         (concat
          "variable {R' : Type*} [Semiring R'] [MulSemiringAction R' A] [SMulCommClass R' R A]\n\n"
          "/-- The action on a subalgebra corresponding to applying the action to every element.\n\n"
          "This is available as an instance in the `Pointwise` locale. -/\n"
          "protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where\n"
          "  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)\n"
          "  one_smul S := (congr_arg (fun f => S.map f) (AlgHom.ext <| one_smul R')).trans S.map_id\n"
          "  mul_smul _a₁ _a₂ S :=\n"
          "    (congr_arg (fun f => S.map f) (AlgHom.ext <| mul_smul _ _)).trans (S.map_map _ _).symm\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-mathlib-mul-to-submodule ()
  (let ((contents
         (concat
          "theorem mul_toSubmodule {R : Type*} {A : Type*} [CommSemiring R] [CommSemiring A] [Algebra R A]\n"
          "    (S T : Subalgebra R A) : (Subalgebra.toSubmodule S) * (Subalgebra.toSubmodule T)\n"
          "        = Subalgebra.toSubmodule (S ⊔ T) := by\n"
          "  refine le_antisymm (mul_toSubmodule_le _ _) ?_\n"
          "  rintro x (hx : x ∈ Algebra.adjoin R (S ∪ T : Set A))\n"
          "  refine\n"
          "    Algebra.adjoin_induction (fun x hx => ?_) (fun r => ?_) (fun _ _ _ _ => Submodule.add_mem _)\n"
          "      (fun x y _ _ hx hy => ?_) hx\n"
          "  · rcases hx with hxS | hxT\n"
          "    · rw [← mul_one x]\n"
          "      exact Submodule.mul_mem_mul hxS (show (1 : A) ∈ T from one_mem T)\n"
          "    · rw [← one_mul x]\n"
          "      exact Submodule.mul_mem_mul (show (1 : A) ∈ S from one_mem S) hxT\n"
          "  · rw [← one_mul (algebraMap _ _ _)]\n"
          "    exact Submodule.mul_mem_mul (show (1 : A) ∈ S from one_mem S) (algebraMap_mem T _)\n"
          "  have := Submodule.mul_mem_mul hx hy\n"
          "  rwa [mul_assoc, mul_comm _ (Subalgebra.toSubmodule T), ← mul_assoc _ _ (Subalgebra.toSubmodule S),\n"
          "    isIdempotentElem_toSubmodule, mul_comm T.toSubmodule, ← mul_assoc,\n"
          "    isIdempotentElem_toSubmodule] at this\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-rank-sup-proof ()
  (let ((contents
         (concat
          "theorem rank_sup_eq_rank_left_mul_rank_of_free :\n"
          "    Module.rank R ↥(A ⊔ B) = Module.rank R A * Module.rank A (Algebra.adjoin A (B : Set S)) := by\n"
          "  rcases subsingleton_or_nontrivial R with _ | _\n"
          "  · haveI := Module.subsingleton R S; simp\n"
          "  nontriviality S using rank_subsingleton'\n"
          "  letI : Algebra A (Algebra.adjoin A (B : Set S)) := Subalgebra.algebra _\n"
          "  letI : SMul A (Algebra.adjoin A (B : Set S)) := Algebra.toSMul\n"
          "  haveI : IsScalarTower R A (Algebra.adjoin A (B : Set S)) :=\n"
          "    IsScalarTower.of_algebraMap_eq (congrFun rfl)\n"
          "  rw [rank_mul_rank R A (Algebra.adjoin A (B : Set S))]\n"
          "  change _ = Module.rank R ((Algebra.adjoin A (B : Set S)).restrictScalars R)\n"
          "  rw [Algebra.restrictScalars_adjoin]; rfl\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-restrict-scalars-def ()
  (let ((contents
         (concat
          "def restrictScalars (U : Subalgebra S A) : Subalgebra R A :=\n"
          "  { U with\n"
          "    algebraMap_mem' := fun x ↦ by\n"
          "      rw [IsScalarTower.algebraMap_apply R S A]\n"
          "      exact U.algebraMap_mem _ }\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-restrict-scalars-injective ()
  (let ((contents
         (concat
          "theorem restrictScalars_injective :\n"
          "    Function.Injective (restrictScalars R : Subalgebra S A → Subalgebra R A) := fun U V H ↦\n"
          "  ext fun x ↦ by rw [← mem_restrictScalars R, H, mem_restrictScalars]\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-adjoin-range-proof ()
  (let ((contents
         (concat
          "theorem adjoin_range_toAlgHom (t : Set A) :\n"
          "    (Algebra.adjoin (toAlgHom R S A).range t).restrictScalars R =\n"
          "      (Algebra.adjoin S t).restrictScalars R :=\n"
          "        Subalgebra.ext fun z ↦\n"
          "          show z ∈ Subsemiring.closure (Set.range (algebraMap (toAlgHom R S A).range A) ∪ t : Set A) ↔\n"
          "               z ∈ Subsemiring.closure (Set.range (algebraMap S A) ∪ t : Set A) by\n"
          "            suffices Set.range (algebraMap (toAlgHom R S A).range A) = Set.range (algebraMap S A) by\n"
          "              rw [this]\n"
          "              ext z\n"
          "              exact ⟨fun ⟨⟨_, y, h1⟩, h2⟩ ↦ ⟨y, h2 ▸ h1⟩, fun ⟨y, hy⟩ ↦ ⟨⟨z, y, hy⟩, rfl⟩⟩\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-comment-continuation-from-before-file ()
  (let ((contents
         (concat
          "/-!\n"
          "## TODO\n\n"
          "* once we have scalar actions by semigroups (as opposed to monoids), implement the action of a\n"
          "  non-unital subalgebra on the larger algebra.\n"
          "-/\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(lean4-define-final-line-indent-test
  lean4-indent--multiline-string-continuation-keeps-indent
  "def foo :=\n  s!\"hello \\\n    -- world\"\n")

(ert-deftest lean4-indent--indent-region-preserves-multiline-string-continuation ()
  (lean4-test-with-indent-buffer
      "def foo :=\n  s!\"hello \\\n    -- world\"\n"
    (lean4-test--indent-region-and-assert-same)))

(ert-deftest lean4-indent--indent-region-preserves-wrapped-instance-where-body-from-before-file ()
  (let ((contents
         (concat
          "instance instInvolutiveStar {S R : Type*} [InvolutiveStar R] [SetLike S R] [StarMemClass S R]\n"
          "    (s : S) : InvolutiveStar s where\n"
          "  star_involutive r := Subtype.ext <| star_star (r : R)\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-structure-doc-and-fields-from-before-file ()
  (let ((contents
         (concat
          "structure NonUnitalStarSubalgebra (R : Type u) (A : Type v) [CommSemiring R]\n"
          "    [NonUnitalNonAssocSemiring A] [Module R A] [Star A] : Type v\n"
          "    extends NonUnitalSubalgebra R A where\n"
          "  /-- The `carrier` of a `NonUnitalStarSubalgebra` is closed under the `star` operation. -/\n"
          "  star_mem' : ∀ {a : A} (_ha : a ∈ carrier), star a ∈ carrier\n\n"
          "/-- Reinterpret a `NonUnitalStarSubalgebra` as a `NonUnitalSubalgebra`. -/\n"
          "add_decl_doc NonUnitalStarSubalgebra.toNonUnitalSubalgebra\n")))
    (lean4-test-with-indent-buffer contents
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) before))))))

(ert-deftest lean4-indent--indent-region-preserves-local-let-structure-literal-from-before-file ()
  (let ((contents
         (concat
          "theorem coe_iSup_of_directed [Nonempty ι] {S : ι → NonUnitalStarSubalgebra R A}\n"
          "    (dir : Directed (· ≤ ·) S) : ↑(iSup S) = ⋃ i, (S i : Set A) :=\n"
          "  let K : NonUnitalStarSubalgebra R A :=\n"
          "    { __ := NonUnitalSubalgebra.copy _ _ (NonUnitalSubalgebra.coe_iSup_of_directed dir).symm\n"
          "      star_mem' := fun hx ↦\n"
          "        let ⟨i, hi⟩ := Set.mem_iUnion.1 hx\n"
          "        Set.mem_iUnion.2 ⟨i, star_mem (s := S i) hi⟩ }\n"
          "  have : iSup S = K := le_antisymm (iSup_le fun i ↦ le_iSup (fun i ↦ (S i : Set A)) i)\n"
          "    (Set.iUnion_subset fun _ ↦ le_iSup S _)\n"
          "  this.symm ▸ rfl\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-canlift-where-body-from-before-file ()
  (let ((contents
         (concat
          "instance (priority := 100) : CanLift (Set A) (NonUnitalStarSubalgebra R A) (↑)\n"
          "    (fun s ↦ 0 ∈ s ∧ (∀ {x y}, x ∈ s → y ∈ s → x + y ∈ s) ∧ (∀ {x y}, x ∈ s → y ∈ s → x * y ∈ s) ∧\n"
          "      (∀ (r : R) {x}, x ∈ s → r • x ∈ s) ∧ ∀ {x}, x ∈ s → star x ∈ s) where\n"
          "  prf s h :=\n"
          "    ⟨ { carrier := s\n"
          "        zero_mem' := h.1\n"
          "        add_mem' := h.2.1\n"
          "        mul_mem' := h.2.2.1\n"
          "        smul_mem' := h.2.2.2.1\n"
          "        star_mem' := h.2.2.2.2 },\n"
          "      rfl ⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-where-field-body-line-from-mathlib ()
  (let ((contents
         (concat
          "instance : ToFormat SimpTheorems where\n"
          "  format s :=\n"
          "f!\"pre:\n"
          "{s.pre.values.toList}\n"
          "post:\n"
          "{s.post.values.toList}\"\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-injective-theorem-from-before-file ()
  (let ((contents
         (concat
          "theorem toNonUnitalSubalgebra_injective :\n"
          "    Function.Injective\n"
          "      (toNonUnitalSubalgebra : NonUnitalStarSubalgebra R A → NonUnitalSubalgebra R A) :=\n"
          "  fun S T h =>\n"
          "  ext fun x => by rw [← mem_toNonUnitalSubalgebra, ← mem_toNonUnitalSubalgebra, h]\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-initialize-simps-from-before-file ()
  (let ((contents
         (concat
          "instance : SetLike (Subalgebra R A) A where\n"
          "  coe s := s.carrier\n"
          "  coe_injective' p q h := by cases p; cases q; congr; exact SetLike.coe_injective' h\n\n"
          "initialize_simps_projections Subalgebra (carrier → coe, as_prefix coe)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-scoped-instance-from-before-file ()
  (let ((contents
         (concat
          "theorem foo : True := by\n"
          "  trivial\n\n"
          "scoped instance isScalarTower_right (X) [MulAction A X] :\n"
          "    letI := (inclusion h).toModule; IsScalarTower S T X :=\n"
          "  letI := (inclusion h).toModule; ⟨fun _ ↦ mul_smul _⟩\n\n"
          "scoped instance faithfulSMul :\n"
          "    letI := (inclusion h).toModule; FaithfulSMul S T :=\n"
          "  letI := (inclusion h).toModule\n"
          "  ⟨fun {x y} h ↦ Subtype.ext <| by\n"
          "    exact h⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-grind-pattern ()
  (let ((contents
         (concat
          "example : True := by\n"
          "  trivial\n\n"
          "grind_pattern IsStrictlyPositive.spectrum_pos => x ∈ spectrum 𝕜 a, IsStrictlyPositive a\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-local-grind-pattern-from-mathlib ()
  (let ((contents
         (concat
          "-- This pattern is convenient in this file.\n"
          "local grind_pattern card_le_card => #s, #t\n"
          "\n"
          "@[mono]\n"
          "theorem card_mono : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-standalone-scoped-notation-prefix-from-mathlib ()
  (let ((contents
         (concat
          "/-- doc -/\n"
          "scoped\n"
          "notation:50 c \" ≺[\" m:25 \"] \" d:50 => True\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-proof-wanted-from-mathlib ()
  (let ((contents
         (concat
          "/-- doc -/\n"
          "proof_wanted angle_eq_angle_add_angle_iff {x y z : V} (hy : y ≠ 0) :\n"
          "    angle x z = angle x y + angle y z ↔ True\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-extend-docs-from-mathlib ()
  (let ((contents
         (concat
          "-- comment\n"
          "extend_docs Finsupp.fun₀ after\n"
          "  \"doc string\"\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-run-cmd-from-mathlib ()
  (let ((contents
         (concat
          "run_cmd Lean.Elab.Command.liftTermElabM do\n"
          "  pure ()\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-wrapped-open-from-mathlib ()
  (let ((contents
         (concat
          "open TopologicalSpace Set MeasureTheory intervalIntegral\n"
          " Metric Filter Function Complex\n"
          "\n"
          "open UpperHalfPlane hiding I\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-local-macro-rules-from-mathlib ()
  (let ((contents
         (concat
          "local macro_rules | `($t:term$n:superscript) => `(Fin $n → $t)\n"
          "\n"
          "theorem foo : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-add-aesop-rules-from-mathlib ()
  (let ((contents
         (concat
          "add_aesop_rules safe tactic\n"
          "  (rule_sets := [Measurable])\n"
          "  (index := [target @AEStronglyMeasurable ..])\n"
          "  (by fun_prop (disch := measurability))\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-branches-from-mathlib ()
  (let ((contents
         (concat
          "lemma isChain_flatten : ∀ {L : List (List α)}, [] ∉ L →\n"
          "    (IsChain R L.flatten ↔ True) :=\n"
          "| [], _ => by simp\n"
          "| [l], _ => by simp [flatten]\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-equation-branches-from-mathlib ()
  (let ((contents
         (concat
          "meta def elabDeprecatedCross : TermElab\n"
          "| `($x ×₃%$tk $y) => fun ty? => do\n"
          "  logWarningAt tk <| m!\"deprecated\"\n"
          "| _ => fun _ => throwUnsupportedSyntax\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-wrapped-top-level-equation-branches-from-mathlib ()
  (let ((contents
         (concat
          "theorem list_findIdx₁ {p : α → β → Bool} (hp : Primrec₂ p) :\n"
          "    ∀ l : List β, Primrec fun a => l.findIdx (p a)\n"
          "| [] => const 0\n"
          "| a :: l => (cond (hp.comp .id (const a)) (const 0) (succ.comp (list_findIdx₁ hp l))).of_eq fun n =>\n"
          "  by simp [List.findIdx_cons]\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-macro-rules-branches-from-mathlib ()
  (let ((contents
         (concat
          "macro_rules\n"
          "| `($a ⊔ $b) => `(Max.max $a $b)\n"
          "| `($a ⊓ $b) => `(Min.min $a $b)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-prefix-arg-from-mathlib ()
  (let ((contents
         (concat
          "local prefix:arg \"ι\" => Ordnode.singleton\n"
          "\n"
          "instance : Singleton α (Ordnode α) :=\n"
          "  ⟨Ordnode.singleton⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-mk-iff-of-inductive-prop-from-mathlib ()
  (let ((contents
         (concat
          "mk_iff_of_inductive_prop Sum.LiftRel Sum.liftRel_iff\n"
          "\n"
          "namespace LiftRel\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-assert-not-exists-from-mathlib ()
  (let ((contents
         (concat
          "-- comment\n"
          "assert_not_exists IsLocalization IsLocalRing\n"
          "\n"
          "theorem foo : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-elab-rules-from-mathlib ()
  (let ((contents
         (concat
          "elab_rules : tactic | `(tactic| ghost_calc $[$ids']*) => do\n"
          "  pure ()\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-shallow-top-level-decl-after-attribute-from-mathlib ()
  (let ((contents
         (concat
          "@[simp]\n"
          " theorem equivNum_apply : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-focus-lines-in-decreasing-by-from-mathlib ()
  (let ((contents
         (concat
          "termination_by WellFounded.wrap foo\n"
          "decreasing_by\n"
          "· exact deg_reduce\n"
          "· apply degree_sub_LTerm_lt\n"
          "  intro hf0\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-irreducible-def-from-mathlib ()
  (let ((contents
         (concat
          "/-- doc -/\n"
          "irreducible_def oneTangentSpaceIcc {x y : ℝ} [h : Fact (x < y)] (z : Icc x y) :\n"
          "    TangentSpace (𝓡∂ 1) z :=\n"
          "  foo\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-irreducible-def-structure-body-from-mathlib ()
  (let ((contents
         (concat
          "irreducible_def preLiftAlgHom {s : A → A → Prop} {f : A →ₐ[S] B}\n"
          "  (h : ∀ ⦃x y⦄, s x y → f x = f y) : RingQuot s →ₐ[S] B :=\n"
          "{ toFun := fun x ↦ Quot.lift f\n"
          "            (by\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-where-and-simproc-from-mathlib ()
  (let ((contents
         (concat
          "def prodEq (f : α → α) : α :=\n"
          "  foo\n"
          "where\n"
          "  bar : α := foo\n"
          "\n"
          "simproc_decl sum_univ_ofNat (∑ _ : Fin _, _) := .ofQ fun u _ e => do\n"
          "  pure .continue\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-flush-left-wrapped-lemma-binder-from-mathlib ()
  (let ((contents
         (concat
          "lemma source_subset_preimage_source\n"
          " (h : LiftSourceTargetPropertyAt I I' n f x P) :\n"
          "    h.domChart.source ⊆ f ⁻¹' h.codChart.source :=\n"
          "  h.localPresentationAt.source_subset_preimage_source\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-alias-rhs-from-mathlib ()
  (let ((contents
         (concat
          "@[deprecated (since := \"2025-06-12\")]\n"
          "alias _root_.Diffeomorph.contMDiffWithinAt_transDiffeomorph_right :=\n"
          "contMDiffWithinAt_transContinuousLinearEquiv_right\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-mutual-from-mathlib ()
  (let ((contents
         (concat
          "def normBareNumeral : Nat :=\n"
          "  0\n\n"
          "mutual\n\n"
          "  partial def normPow : Nat :=\n"
          "    0\n\n"
          "  partial def normIntNumeral' : Nat :=\n"
          "    0\n\n"
          "end\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-local-notation-from-mathlib ()
  (let ((contents
         (concat
          "namespace Polynomial\n"
          "variable {ι R S : Type*} [CommRing R] [Ring S] [Algebra R S]\n\n"
          "local notation \"deg(\"p\")\" => natDegree p\n"
          "local notation3 \"coeffs(\"p\")\" => Set.range (coeff p)\n"
          "local notation3 \"spanCoeffs(\"p\")\" => 1 ⊔ Submodule.span R coeffs(p)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-scoped-infix-from-mathlib ()
  (let ((contents
         (concat
          "namespace Computation\n\n"
          "def Promises (s : Nat) (a : Nat) : Prop :=\n"
          "  True\n\n"
          "scoped infixl:50 \" ~> \" => Promises\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-termination-by-from-mathlib ()
  (let ((contents
         (concat
          "theorem foo (L : List Nat) : True := by\n"
          "  trivial\n"
          "termination_by L.length\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-deriving-from-mathlib ()
  (let ((contents
         (concat
          "inductive Foo where\n"
          "  | bar\n"
          "deriving BEq\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-hash-commands-from-mathlib ()
  (let ((contents
         (concat
          "/-- info: Except.ok (\"feat\", some \"x\") -/\n"
          "#guard_msgs in\n"
          "#eval Parser.run prTitle \"feat(x): foo\"\n"
          "#adaptation_note\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-private-local-instance-from-mathlib ()
  (let ((contents
         (concat
          "instance : InverseSystem (embFunctor F E) where\n"
          "  map_self _ _ := rfl\n"
          "  map_map _ _ _ _ _ _ := rfl\n\n"
          "private local instance (i : ι) : Decidable (succ i = i) :=\n"
          "  .isFalse (lt_succ i).ne'\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-shallow-first-top-level-body-line-from-mathlib ()
  (let ((contents
         (concat
          "def subgroupEquivAlgEquiv [FiniteDimensional F E] (H : Subgroup Gal(E/F)) :\n"
          "    H ≃* Gal(E/IntermediateField.fixedField H) :=\n"
          " (MulEquiv.subgroupCongr (fixingSubgroup_fixedField H).symm).trans (fixingSubgroupEquiv _)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-first-top-level-body-line-from-mathlib ()
  (let ((contents
         (concat
          "lemma rescale_to_shell (p : Seminorm 𝕜 E) {c : 𝕜} (hc : 1 < ‖c‖) {ε : ℝ} (εpos : 0 < ε) {x : E}\n"
          "    (hx : p x ≠ 0) :\n"
          "    ∃ d : 𝕜, d ≠ 0 ∧ p (d • x) < ε ∧ (ε / ‖c‖ ≤ p (d • x)) ∧ (‖d‖⁻¹ ≤ ε⁻¹ * ‖c‖ * p x) :=\n"
          "let ⟨_, hn⟩ := p.rescale_to_shell_zpow hc εpos hx; ⟨_, hn⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-tactic-body-line-from-mathlib ()
  (let ((contents
         (concat
          "instance {J : Type} [Finite J] (Z : J → ModuleCat.{v} k) [∀ j, Module.Finite k (Z j)] :\n"
          "    Module.Finite k (∐ fun j => Z j : ModuleCat.{v} k) := by\n"
          "  classical\n"
          "exact (Module.Finite.equiv_iff (ModuleCat.coprodIsoDirectSum Z).toLinearEquiv).mpr inferInstance\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-delimited-top-level-body-line-from-mathlib ()
  (let ((contents
         (concat
          "lemma normal_iff_forall_fieldRange_eq : Normal F K ↔ ∀ σ : K →ₐ[F] L, σ.fieldRange = K :=\n"
          "⟨@AlgHom.fieldRange_of_normal (E := K), normal_iff_forall_fieldRange_le.2 ∘ fun h σ ↦ (h σ).le⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-body-after-inline-attribute ()
  (let ((contents
         (concat
          "@[simp] lemma MeasureTheory.Measure.cdf_eq_iff (μ ν : Measure ℝ) [IsProbabilityMeasure μ]\n"
          "    [IsProbabilityMeasure ν] :\n"
          "    cdf μ = cdf ν ↔ μ = ν :=\n"
          "⟨eq_of_cdf μ ν, fun h ↦ by rw [h]⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-flush-left-wrapped-variable-block-from-mathlib ()
  (let ((contents
         (concat
          "section Div\n\n"
          "variable {𝕜 : Type*} [NontriviallyNormedField 𝕜] {n : WithTop ℕ∞}\n"
          "{H : Type*} [TopologicalSpace H] {E : Type*}\n"
          "  [NormedAddCommGroup E] [NormedSpace 𝕜 E] {I : ModelWithCorners 𝕜 E H} {G : Type*}\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-flush-left-wrapped-deriving-instance-header-from-mathlib ()
  (let ((contents
         (concat
          "deriving instance\n"
          "  T2Space, CommGroup,\n"
          "for PontryaginDual A\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-brace-body-from-mathlib ()
  (let ((contents
         (concat
          "instance (priority := 100) toFoo : Foo :=\n"
          "{ x := 0\n"
          "}\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-brace-closing-line-after-decreasing-by ()
  (let ((contents
         (concat
          "decreasing_by all_goals {\n"
          "  subst_vars\n"
          "  pgame_wf_tac\n"
          "}\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-closing-bracket-from-mathlib ()
  (let ((contents
         (concat
          "def foo : Array Nat := #[\n"
          "  1,\n"
          "  2\n"
          "]\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-meta-register-option-from-mathlib ()
  (let ((contents
         (concat
          "meta register_option mathlib.foo : Bool := {\n"
          "  defValue := false\n"
          "  descr := \"x\"\n"
          "}\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-more-tactic-linter-toplevels ()
  (dolist (contents
           (list
            (concat
             "initialize addLinter commandRangesLinter\n")
            (concat
             "partial\n"
             "def parallelScanAux (as : Array FormatError) (L M : String) : Array FormatError :=\n"
             "  as\n")
            (concat
             "elab (name := deprecated_modules)\n"
             "    \"deprecated_module foo\" : command => do\n"
             "  pure ()\n")
            (concat
             "macro \"#import_bumps\" : command => `(\n"
             "  run_cmd logInfo \"Counting imports from here.\"\n"
             "  set_option Elab.async false\n"
             "  set_option linter.minImports true)\n")
            (concat
             "public register_option linter.privateModule : Bool := {\n"
             "  defValue := false\n"
             "  descr := \"x\"\n"
             "}\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-nonrec-and-alias-from-mathlib ()
  (let ((contents
         (concat
          "namespace Metric\n"
          "variable {α β : Type*}\n\n"
          "nonrec theorem isUniformEmbedding_iff {f : α → β} : True := by\n"
          "  trivial\n\n"
          "alias ⟨foo, _⟩ := isUniformEmbedding_iff\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-standalone-top-level-nonrec-from-mathlib ()
  (let ((contents
         (concat
          "nonrec\n"
          "theorem _root_.ContinuousWithinAt.clog {f : α → ℂ} {s : Set α} {x : α}\n"
          "    (h₁ : ContinuousWithinAt f s x) (h₂ : f x ∈ slitPlane) :\n"
          "    ContinuousWithinAt (fun t => log (f t)) s x :=\n"
          "  h₁.clog h₂\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-noncomputable-from-mathlib ()
  (let ((contents
         (concat
          "class Foo where\n"
          "  x : Nat\n\n"
          "noncomputable\n"
          "def bar : Nat :=\n"
          "  0\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-export-from-mathlib ()
  (let ((contents
         (concat
          "class OneMemClass (S M : Type*) where\n"
          "  one_mem : ∀ s : S, True\n\n"
          "export OneMemClass (one_mem)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-include-from-mathlib ()
  (let ((contents
         (concat
          "section\n"
          "variable (hf : True) (hg : True)\n"
          "include hf hg\n\n"
          "theorem foo : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-include-in-from-mathlib ()
  (let ((contents
         (concat
          "section\n"
          "variable (S f : True)\n\n"
          "include S f in\n"
          "lemma foo : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-omit-in-from-mathlib ()
  (let ((contents
         (concat
          "variable {K L M} in\n"
          "omit [IsScalarTower K L M] [Algebra L M] in\n"
          "lemma foo : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-unseal-in-from-mathlib ()
  (let ((contents
         (concat
          "unseal OreLocalization.one in\n"
          "@[to_additive]\n"
          "theorem mk_one : mk 1 (1 : S) = 1 := OreLocalization.one_def\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-suppress-compilation-in-from-mathlib ()
  (let ((contents
         (concat
          "suppress_compilation in\n"
          "/-- doc -/\n"
          "def foo : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-inline-macro-rules-from-mathlib ()
  (let ((contents
         (concat
          "@[inherit_doc OreLocalization]\n"
          "scoped syntax:1075 term noWs atomic(\"[\" term \"⁻¹\" noWs \"]\") : term\n"
          "macro_rules | `($R[$S⁻¹]) => ``(OreLocalization $S $R)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-unif-hint-from-mathlib ()
  (let ((contents
         (concat
          "/-- This unification hint helps with problems of the form `(forget ?C).obj R =?= carrier R'`. -/\n"
          "unif_hint forget_obj_eq_coe (R R' : SemiRingCat) where\n"
          "  R ≟ R' ⊢\n"
          "  (forget SemiRingCat).obj R ≟ SemiRingCat.carrier R'\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-library-note2-from-mathlib ()
  (let ((contents
         (concat
          "library_note2 «lower cancel priority» /--\n"
          "We lower the priority of inheriting from cancellative structures.\n"
          "-/\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-insert-to-additive-translation-from-mathlib ()
  (let ((contents
         (concat
          "/- `LinearOrderedCommGroup` and `LinearOrderedAddCommGroup` no longer exist. -/\n"
          "insert_to_additive_translation LinearOrderedCommGroup LinearOrderedAddCommGroup\n"
          "\n"
          "section LinearOrderedCommGroup\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-attribute-continuation-from-mathlib ()
  (let ((contents
         (concat
          "@[to_additive (attr := simps -isSimp,\n"
          "deprecated MulEquiv.monoidHomCongrLeftEquiv (since := \"2025-08-12\"))\n"
          "/-- doc -/]\n"
          "def precompEquiv : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-scoped-attribute-from-mathlib ()
  (let ((contents
         (concat
          "scoped [BooleanAlgebraOfBooleanRing] attribute [instance 100] BooleanRing.sup\n"
          "scoped [BooleanAlgebraOfBooleanRing] attribute [instance 100] BooleanRing.inf\n"
          "open BooleanAlgebraOfBooleanRing\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-wrapped-notation-from-mathlib ()
  (let ((contents
         (concat
          "@[inherit_doc]\n"
          "notation:25 -- `→ᵃᵢ` would be more consistent with the linear isometry notation, but it is uglier\n"
          "P \" →ᵃⁱ[\" 𝕜:25 \"] \" P₂:0 => AffineIsometry 𝕜 P P₂\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-top-level-private-modifier-line-from-mathlib ()
  (let ((contents
         (concat
          "lemma foo : True := by\n"
          "  trivial\n\n"
          "private\n"
          "theorem bar : True := by\n"
          "  trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-non-top-level-meta-application-from-mathlib ()
  (let ((contents
         (concat
          "meta def delabGal : Delab := whenNotPPOption getPPExplicit <| whenPPOption getPPNotation do\n"
          "  -- comment\n"
          "  Meta.withLocalInstances (← getLCtx).decls.toList.reduceOption do\n"
          "  guard True\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-indented-set-option-in-proof-body-from-mathlib ()
  (let ((contents
         (concat
          "theorem foo : True := by\n"
          "  set_option push_neg.use_distrib true in\n"
          "  contrapose!; trivial\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-zero-indent-top-level-match-from-mathlib ()
  (let ((contents
         (concat
          "partial def typeToCharP (t : Nat) : Nat :=\n"
          "match t with\n"
          "| 0 =>\n"
          "  0\n"
          "| _ =>\n"
          "  1\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-have-this-theorem-from-before-file ()
  (let ((contents
         (concat
          "theorem star_adjoin_comm (s : Set A) :\n"
          "    star (NonUnitalAlgebra.adjoin R s) = NonUnitalAlgebra.adjoin R (star s) :=\n"
          "  have this :\n"
          "    ∀ t : Set A, NonUnitalAlgebra.adjoin R (star t) ≤ star (NonUnitalAlgebra.adjoin R t) := fun _ =>\n"
          "    NonUnitalAlgebra.adjoin_le fun _ hx => NonUnitalAlgebra.subset_adjoin R hx\n"
          "  le_antisymm (by simpa only [star_star] using NonUnitalSubalgebra.star_mono (this (star s)))\n"
          "    (this s)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-iunionlift-structure-literal-from-before-file ()
  (let ((contents
         (concat
          "noncomputable def iSupLift [Nonempty ι] (K : ι → NonUnitalStarSubalgebra R A)\n"
          "    (dir : Directed (· ≤ ·) K) (f : ∀ i, K i →⋆ₙₐ[R] B)\n"
          "    (hf : ∀ (i j : ι) (h : K i ≤ K j), f i = (f j).comp (inclusion h))\n"
          "    (T : NonUnitalStarSubalgebra R A) (hT : T = iSup K) : ↥T →⋆ₙₐ[R] B := by\n"
          "  subst hT\n"
          "  exact\n"
          "    { toFun :=\n"
          "        Set.iUnionLift (fun i => ↑(K i)) (fun i x => f i x)\n"
          "          (fun i j x hxi hxj => by\n"
          "            let ⟨k, hik, hjk⟩ := dir i j\n"
          "            simp only\n"
          "            rw [hf i k hik, hf j k hjk]\n"
          "            rfl)\n"
          "          _ (by rw [coe_iSup_of_directed dir])\n"
          "      map_zero' := by\n"
          "        dsimp only [SetLike.coe_sort_coe, NonUnitalAlgHom.coe_comp, Function.comp_apply,\n"
          "          inclusion_mk, Eq.ndrec, id_eq, eq_mpr_eq_cast]\n"
          "        exact Set.iUnionLift_const _ (fun i : ι => (0 : K i)) (fun _ => rfl) _ (by simp)\n"
          "      map_mul' := by\n"
          "        dsimp only [SetLike.coe_sort_coe, NonUnitalAlgHom.coe_comp, Function.comp_apply,\n"
          "          inclusion_mk, Eq.ndrec, id_eq, eq_mpr_eq_cast, ZeroMemClass.coe_zero,\n"
          "          AddSubmonoid.mk_add_mk, Set.inclusion_mk]\n"
          "        apply Set.iUnionLift_binary (coe_iSup_of_directed dir) dir _ (fun _ => (· * ·))\n"
          "        all_goals simp\n"
          "      map_add' := by\n"
          "        dsimp only [SetLike.coe_sort_coe, NonUnitalAlgHom.coe_comp, Function.comp_apply,\n"
          "          inclusion_mk, Eq.ndrec, id_eq, eq_mpr_eq_cast]\n"
          "        apply Set.iUnionLift_binary (coe_iSup_of_directed dir) dir _ (fun _ => (· + ·))\n"
          "        all_goals simp\n"
          "      map_smul' := fun r => by\n"
          "        dsimp only [SetLike.coe_sort_coe, NonUnitalAlgHom.coe_comp, Function.comp_apply,\n"
          "          inclusion_mk, Eq.ndrec, id_eq, eq_mpr_eq_cast]\n"
          "        apply Set.iUnionLift_unary (coe_iSup_of_directed dir) _ (fun _ x => r • x)\n"
          "          (fun _ _ => rfl)\n"
          "        all_goals simp\n"
          "      map_star' := by\n"
          "        dsimp only [SetLike.coe_sort_coe, NonUnitalStarAlgHom.comp_apply, inclusion_mk, Eq.ndrec,\n"
          "          id_eq, eq_mpr_eq_cast, ZeroMemClass.coe_zero, AddSubmonoid.mk_add_mk, Set.inclusion_mk,\n"
          "          MulMemClass.mk_mul_mk, NonUnitalAlgHom.toDistribMulActionHom_eq_coe,\n"
          "          DistribMulActionHom.toFun_eq_coe, NonUnitalAlgHom.coe_toDistribMulActionHom,\n"
          "          NonUnitalAlgHom.coe_mk]\n"
          "        apply Set.iUnionLift_unary (coe_iSup_of_directed dir) _ (fun _ x => star x)\n"
          "          (fun _ _ => rfl)\n"
          "        all_goals simp [map_star] }\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-wrapped-relation-proof-line-from-before-file ()
  (let ((contents
         (concat
          "@[simp, norm_cast]\n"
          "theorem coe_range (φ : F) :\n"
          "    ((NonUnitalStarAlgHom.range φ : NonUnitalStarSubalgebra R B) : Set B) =\n"
          "    Set.range (φ : A → B) := by\n"
          "  rfl\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-show-from-proof-from-before-file ()
  (let ((contents
         (concat
          "theorem adjoin_eq_starClosure_adjoin (s : Set A) :\n"
          "    adjoin R s = (NonUnitalAlgebra.adjoin R s).starClosure :=\n"
          "  toNonUnitalSubalgebra_injective <| show\n"
          "    NonUnitalAlgebra.adjoin R (s ∪ star s) =\n"
          "      NonUnitalAlgebra.adjoin R s ⊔ star (NonUnitalAlgebra.adjoin R s)\n"
          "    from\n"
          "      (NonUnitalSubalgebra.star_adjoin_comm R s).symm ▸ NonUnitalAlgebra.adjoin_union s (star s)\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--indent-region-preserves-have-body-line-from-before-file ()
  (let ((contents
         (concat
          "instance _root_.NonUnitalStarAlgHom.subsingleton [Subsingleton (NonUnitalStarSubalgebra R A)] :\n"
          "    Subsingleton (A →⋆ₙₐ[R] B) :=\n"
          "  ⟨fun f g => NonUnitalStarAlgHom.ext fun a =>\n"
          "    have : a ∈ (⊥ : NonUnitalStarSubalgebra R A) :=\n"
          "      Subsingleton.elim (⊤ : NonUnitalStarSubalgebra R A) ⊥ ▸ mem_top\n"
          "    (mem_bot.mp this).symm ▸ (map_zero f).trans (map_zero g).symm⟩\n")))
    (lean4-test-with-indent-buffer contents
      (lean4-test--indent-region-and-assert-same))))

(ert-deftest lean4-indent--non-tab-reindent-still-normalizes-term-code ()
  (lean4-test-with-indent-buffer
      (concat
       "example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=\n"
       "  Iff.intro\n"
       "    (fun hnpnq: _ =>\n"
       "      have hnp := hnpnq.left\n"
       "      have hnq := hnpnq.right\n"
       "  (fun hpq: p ∨ q =>\n")
    (lean4-test--goto-eob)
    (funcall #'lean4-indent-line-function)
    (should (equal (lean4-test--line-string) "      (fun hpq: p ∨ q =>"))))

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

(lean4-define-final-line-indent-test
 lean4-indent--operator-continuation-with-comment
 "def foo :=
  a + -- comment
  b")

(lean4-define-final-line-indent-test
 lean4-indent--operator-continuation-breaks-on-comment-line
 "def foo :=
  a +
  -- comment
  b")

(lean4-define-final-line-indent-test
 lean4-indent--coloneq-with-comment-indents
 "def foo := -- comment
  bar")

(lean4-define-final-line-indent-test
 lean4-indent--colon-with-comment-indents
 "def foo : -- comment
    True")

(lean4-define-final-line-indent-test
 lean4-indent--equals-with-comment-indents
 "def foo = -- comment
    bar")

(ert-deftest lean4-indent--tab-does-not-move-point-in-text ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "    exact trivial\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("rfl" "  rfl")
       ("rfl" "  rfl")))))

(ert-deftest lean4-indent--by-ignores-commented-by ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  exact trivial -- by\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("rfl" "  rfl")
       ("rfl" "  rfl")))))

(ert-deftest lean4-indent--then-with-comment-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "if h then -- comment\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("trivial" "  trivial")
       ("trivial" "  trivial")))))

(ert-deftest lean4-indent--else-with-comment-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "else -- comment\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("trivial" "  trivial")
       ("trivial" "  trivial")))))

(ert-deftest lean4-indent--do-with-comment-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "do -- comment\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("trivial" "  trivial")
       ("trivial" "  trivial")))))

(lean4-define-final-line-indent-test
 lean4-indent--de-morgan-final-line-keeps-indent
 "example {p q} : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
    (fun hnpq: _ =>
      ⟨(fun hp: p => (hnpq (Or.inl hp))),
       (fun hq: q => (hnpq (Or.inr hq)))⟩)
    (fun hnpnq: _ =>
      have hnp := hnpnq.left
      have hnq := hnpnq.right
      (fun hpq: p ∨ q =>
        hpq.elim hnp hnq))")

(ert-deftest lean4-indent--fun-arrow-with-comment-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "fun x ↦ -- comment\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("x" "  x")
       ("x" "  x")))))

(ert-deftest lean4-indent--comma-with-comment-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo :=\n"
       "  [1, -- comment\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("2" "  2")
       ("2" "  2")))))

(ert-deftest lean4-indent--tab-fixes-stale-indent-after-coloneq ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : Nat :=\n"
       "        1\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((last-command nil))
      (lean4-test--tab-indent))
    (should (equal (lean4-test--line-string) "  1"))))

(lean4-define-final-line-indent-test
 lean4-indent--match-branches-align
 "def foo (n : Nat) : Nat := by
  match n with
  | 0 => 0")

(lean4-define-final-line-indent-test
 lean4-indent--induction-branches-align
 "lemma foo (xs : List Nat) : True := by
  induction xs with
  | nil => trivial")

(lean4-define-final-line-indent-test
 lean4-indent--macro-rules-branches-align
 "macro_rules
  | `(foo) => bar")

(lean4-define-final-line-indent-test
 lean4-indent--macro-rules-branches-align-after-comment
 "macro_rules
-- comment
  | `(foo) => bar")

(ert-deftest lean4-indent--macro-rules-inline-branches-align ()
  (lean4-test-with-indent-buffer
      (concat
       "macro_rules | `(tactic| use_discharger) => `(tactic| rfl)\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent
     "macro_rules | `(tactic| use_discharger) => `(tactic| assumption)")
    (should (equal (lean4-test--line-string)
                   "macro_rules | `(tactic| use_discharger) => `(tactic| assumption)"))))

(lean4-define-final-line-indent-test
 lean4-indent--scoped-macro-rules-branches-align
 "scoped macro_rules
  | `(foo) => bar")

(ert-deftest lean4-indent--scoped-macro-rules-inline-align ()
  (lean4-test-with-indent-buffer
      (concat
       "scoped macro_rules | `([$l,*]) => `(List.nil)\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent
     "scoped macro_rules | `([$l,*]) => `(List.cons 1 [])")
    (should (equal (lean4-test--line-string)
                   "scoped macro_rules | `([$l,*]) => `(List.cons 1 [])"))))

(lean4-define-final-line-indent-test
 lean4-indent--wrapped-declaration-where-body
 "lemma IsOrderedRing.of_mul_nonneg [Ring R] [PartialOrder R]
    [ZeroLEOneClass R] (mul_nonneg : ∀ a b : R, 0 ≤ a → 0 ≤ b → 0 ≤ a * b) :
    IsOrderedRing R where
  mul_le_mul_of_nonneg_left := by")

(lean4-define-final-line-indent-test
 lean4-indent--wrapped-declaration-where-continuation
 "lemma foo :
    True
    where
  bar := by")

(lean4-define-final-line-indent-test
 lean4-indent--wrapped-declaration-where-inline
 "instance [Add α] {a b : Thunk α} (εa εb : Type*) :
    EstimatorData (a + b) (εa × εb) where
  bound e := by")

(ert-deftest lean4-indent--have-continues-arguments ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h_int_term1 : IntervalIntegrable\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("True" "    True")))))

(ert-deftest lean4-indent--wrapped-declaration-colon-continues ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma foo [Ring R]\n"
       "    [ZeroLEOneClass R] :\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("True := by" "    True := by")
       ("trivial" "  trivial")))))

(ert-deftest lean4-indent--wrapped-declaration-after-bare-coloneq ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo [Ring R] [PartialOrder R]\n"
       "    [ZeroLEOneClass R] : True :=\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("by" "  by")
       ("trivial" "    trivial")))))

(ert-deftest lean4-indent--wrapped-declaration-by-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo [Ring R] [PartialOrder R]\n"
       "    [ZeroLEOneClass R] : True := by\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("trivial" "  trivial")
       ("trivial" "  trivial")))))

(lean4-define-final-line-indent-test
 lean4-indent--calc-indents
 "theorem foo : True := by
  calc
    True := by")

(lean4-define-final-line-indent-test
 lean4-indent--calc-multiline-rhs-indents-proof
 "theorem foo : True := by
  calc
    True := by
      exact trivial")

(ert-deftest lean4-indent--fun-arrow-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h := fun x ↦\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("True" "    True")
       ("True" "    True")))))

(ert-deftest lean4-indent--have-multiline-type-and-proof ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h_int_term2 : IntervalIntegrable\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent
     "(fun x ↦ True) volume a b := by")
    (should (equal (lean4-test--line-string)
                   "      (fun x ↦ True) volume a b := by"))
    (lean4-test--insert-line-below-and-indent "exact trivial")
    (should (equal (lean4-test--line-string) "    exact trivial"))))

(lean4-define-final-line-indent-test
 lean4-indent--label-colon-indents-one-level
 "def foo : Cmd := `[Cli|
  FLAGS:
    output : String")

(lean4-define-final-line-indent-test
 lean4-indent--label-colon-multi-block
 "def foo : Cmd := `[Cli|
  FLAGS:
    output : String; \"path\"

  ARGS:
    arg : Nat")

(lean4-define-final-line-indent-test
 lean4-indent--multiline-definition-and-proof
 "def foo :
    Nat → Nat
    := by
  intro n
  exact n")

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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "mul_mem' := mul _ _")
    (should (equal (lean4-test--line-string) "    mul_mem' := mul _ _"))))

(ert-deftest lean4-indent--inline-brace-newline-after-motive ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  let s : Subalgebra R (FreeAlgebra R X) :=\n"
       "    { carrier := motive\n")
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
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--freealgebra-fun-classical-newlines ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem ι_injective [Nontrivial R] : Function.Injective (ι R : X → FreeAlgebra R X) :=\n"
       "  fun x y hoxy ↦\n"
       "  by_contradiction <| by\n"
       "    classical exact fun hxy : x ≠ y ↦\n")
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
    (goto-char (point-min))
    (search-forward "by")
    (newline)
    (funcall #'lean4-indent-line-function)
    (should (equal (lean4-test--line-string) "  "))))

(ert-deftest lean4-indent--multiline-theorem-binders-indent ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem lemma_aachIBP_parts (σ : ℝ) (φ : ℝ → ℝ) (a b : ℝ) (hab : a < b) (ha_pos : 0 < a)\n")
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
    (goto-char (point-min))
    (search-forward "by")
    (newline)
    (funcall #'lean4-indent-line-function)
    (should (equal (lean4-test--line-string) "        "))))

(lean4-define-final-line-indent-test
 lean4-indent--calc-absolute-value-step-coloneq-by-line
 "  calc
    |s n| = |s n - a + a| := by simp")

(ert-deftest lean4-indent--calc-next-step-by-ignores-nested-coloneq-in-prior-proof ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  calc\n"
       "    True = True := by\n"
       "      let x := True\n"
       "      exact x\n"
       "    _ = True := by\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "simp +contextual only [mem_sigma]")
    (should (equal (lean4-test--line-string) "    simp +contextual only [mem_sigma]"))))

(ert-deftest lean4-indent--focus-calc-indents-one-step ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  · calc\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "exact trivial")
    (should (equal (lean4-test--line-string) "    exact trivial"))))

(ert-deftest lean4-indent--focus-have-by-indents-one-step ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  · have h : True := by\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "exact trivial")
    (should (equal (lean4-test--line-string) "    exact trivial"))))

(ert-deftest lean4-indent--calc-operator-continues-one-step ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  calc\n"
       "    True = True *\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "∏ x ∈ s, f x := by")
    (should (equal (lean4-test--line-string) "      ∏ x ∈ s, f x := by"))))

(ert-deftest lean4-indent--calc-operator-continues-with-cast-mul-line ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  calc\n"
       "    _ = ((t ^ (-s.re) : ℝ) : ℂ) *\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "match binder with")
    (should (equal (lean4-test--line-string) "    match binder with"))))

(ert-deftest lean4-indent--fat-arrow-with-comment-indents ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  match stx with\n"
       "  | `(($a + $b = $n)) => -- Maybe this is too cute.\n")
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
    (goto-char (point-min))
    (forward-line 3)
    (should (equal (lean4-test--line-string) "end"))))

(ert-deftest lean4-indent--end-aligns-with-mutual ()
  (lean4-test-with-indent-buffer
      (concat
       "mutual\n"
       "  def foo := 1\n"
       "end\n")
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (lean4-test--line-string) "end"))))

(lean4-define-final-line-indent-test
 lean4-indent--closing-brace-aligns
 "def foo :=
  { field := 1
  }")

(lean4-define-final-line-indent-test
 lean4-indent--closing-angle-aligns
 "def foo :=
  ⟨
    1,
  ⟩")

(lean4-define-final-line-indent-test
 lean4-indent--closing-angle-aligns-after-exact
 "example : True := by
  exact ⟨
        ⟩")

(lean4-define-final-line-indent-test
 lean4-indent--angle-literal-multiline
 "example : True := by
  exact ⟨
    rfl,
    rfl")

(ert-deftest lean4-indent--top-level-snap-skips-namespace-comment ()
  (lean4-test-with-indent-buffer
      (concat
       "namespace Foo\n"
       "-- comment\n"
       "  open Bar\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "(prod_filter_mul_prod_filter_not s p _).symm")
    (should (equal (lean4-test--line-string)
                   "      (prod_filter_mul_prod_filter_not s p _).symm"))))

(ert-deftest lean4-indent--star-operator-continues-term ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h := a *\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "b")
    (should (equal (lean4-test--line-string) "    b"))))

(ert-deftest lean4-indent--star-operator-does-not-continue-at-star ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  simp at *\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "exact trivial")
    (should (equal (lean4-test--line-string) "  exact trivial"))))

(ert-deftest lean4-indent--star-operator-continues-mathlib-multiplication ()
  (lean4-test-with-indent-buffer
      (concat
       "have h2 (n : ℕ) : cos (b ^ (n + m) * π * x) =\n"
       "      (-1) ^ (⌊b ^ m * x + 2⁻¹⌋) *\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent
     "cos (b ^ n * (b ^ m * x - ⌊b ^ m * x + 2⁻¹⌋) * π) := by")
    (should (equal (lean4-test--line-string)
                   "        cos (b ^ n * (b ^ m * x - ⌊b ^ m * x + 2⁻¹⌋) * π) := by"))))

(ert-deftest lean4-indent--star-operator-continues-paren-line ()
  (lean4-test-with-indent-buffer
      (concat
       "        ((t : ℂ) ^ ((-s).re : ℂ)) *\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "by")
    (should (equal (lean4-test--line-string) "      by"))))

(ert-deftest lean4-indent--open-paren-continues-args ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  exact congr_arg₂ _ (foo\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "bar")
    (should (equal (lean4-test--line-string) "    bar"))))

(ert-deftest lean4-indent--paren-line-not-closed-indents-under-opener ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo :=\n"
       "  (bar :=\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "b")
    (should (equal (lean4-test--line-string) "    b"))))

(ert-deftest lean4-indent--let-semicolon-keeps-chain-indent ()
  (lean4-test-with-indent-buffer
      (concat
       "example : Nat := by\n"
       "  let x := foo;\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "let y := bar")
    (should (equal (lean4-test--line-string) "  let y := bar"))))

(ert-deftest lean4-indent--operator-continuation-non-ascii ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h : True := by\n"
       "    exact True ∧\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "True")
    (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-iso ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h : True := by\n"
       "    exact True ≅\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "True")
    (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-equiv ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h : True := by\n"
       "    exact True ≃\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "True")
    (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-iso-equalizerproducts ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h : True := by\n"
       "    exact True ≅\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "True")
    (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--operator-continuation-equiv-derangements ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h : True := by\n"
       "    exact True ≃\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-line-below-and-indent "True")
    (should (equal (lean4-test--line-string) "    True"))))

(ert-deftest lean4-indent--paren-continuation-aligns-to-open-paren ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h_lhs : IntervalIntegrable\n"
       "      (fun x ↦ (x /)\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("exact 0" "        exact 0")
       ("exact 0" "        exact 0")))))

(lean4-define-final-line-indent-test
 lean4-indent--structure-where-fields
 "structure Foo where
  /-- doc -/
  x : Nat
  y : Nat")

(lean4-define-final-line-indent-test
 lean4-indent--instance-where-fields
 "instance : Inhabited Nat where
  default := 0
  := 1")

(lean4-define-final-line-indent-test
 lean4-indent--protected-def-where-multiline-field-body
 "protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where
  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)
  one_smul S := (congr_arg (fun f => S.map f) (AlgHom.ext <| one_smul R')).trans S.map_id
  mul_smul _a₁ _a₂ S :=
    (congr_arg (fun f => S.map f) (AlgHom.ext <| mul_smul _ _)).trans (S.map_map _ _).symm")

(lean4-define-final-line-indent-test
 lean4-indent--protected-def-where-multiline-field-body-after-variable
 "variable {R' : Type*} [Semiring R'] [MulSemiringAction R' A] [SMulCommClass R' R A]

protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where
  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)
  one_smul S := (congr_arg (fun f => S.map f) (AlgHom.ext <| one_smul R')).trans S.map_id
  mul_smul _a₁ _a₂ S :=
    (congr_arg (fun f => S.map f) (AlgHom.ext <| mul_smul _ _)).trans (S.map_map _ _).symm")

(ert-deftest lean4-indent--multiline-have-with-body ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  have h :\n"
       "      True := by\n"
       "    trivial\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("exact trivial" "    exact trivial")
       ("exact trivial" "    exact trivial")))))

(ert-deftest lean4-indent--declaration-type-continues ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo :\n")
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
    (goto-char (point-min))
    (forward-line 1)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "def bar : Nat := by"))))

(ert-deftest lean4-indent--top-level-section-snaps-left ()
  (lean4-test-with-indent-buffer "  section Foo\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "section Foo"))))

(ert-deftest lean4-indent--top-level-public-section-snaps-left ()
  (lean4-test-with-indent-buffer "  public section\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "public section"))))

(ert-deftest lean4-indent--top-level-section-trivial-snaps-left ()
  (lean4-test-with-indent-buffer "  section trivial\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "section trivial"))))

(ert-deftest lean4-indent--top-level-namespace-snaps-left ()
  (lean4-test-with-indent-buffer "  namespace Foo\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "namespace Foo"))))

(ert-deftest lean4-indent--top-level-anchor-compile-inductive ()
  (lean4-test-with-indent-buffer "  compile_inductive Foo\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "compile_inductive Foo"))))

(ert-deftest lean4-indent--top-level-anchor-partial-fixpoint ()
  (lean4-test-with-indent-buffer "  partial_fixpoint Foo\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "partial_fixpoint Foo"))))

(ert-deftest lean4-indent--top-level-irreducible-def-is-declaration-head ()
  (should (lean4-indent--line-top-level-declaration-head-p
           "irreducible_def foo : True := by"))
  (should (lean4-indent--line-top-level-anchor-p
           "irreducible_def foo : True := by")))

(ert-deftest lean4-indent--top-level-simproc-decl-is-declaration-head ()
  (should (lean4-indent--line-top-level-declaration-head-p
           "simproc_decl foo (bar) := .ofQ fun _ => do"))
  (should (lean4-indent--line-top-level-anchor-p
           "simproc_decl foo (bar) := .ofQ fun _ => do")))

(ert-deftest lean4-indent--top-level-anchor-open-universe-attribute ()
  (lean4-test-with-indent-buffer
      (concat
       "  open Foo\n"
       "  universe u\n"
       "  attribute [simp] Foo.bar\n"
       "  scoped [Pointwise] attribute [instance] Foo.bar\n")
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "open Foo"))
    (forward-line 1)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "universe u"))
    (forward-line 1)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "attribute [simp] Foo.bar"))
    (forward-line 1)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "scoped [Pointwise] attribute [instance] Foo.bar"))))

(ert-deftest lean4-indent--top-level-anchor-more-command-forms-snap-left ()
  (lean4-test-with-indent-buffer
      (concat
       "  initialize_simps_projections Foo (bar → baz)\n"
       "  grind_pattern Foo.bar => True\n"
       "  local grind_pattern Foo.bar => True\n"
       "  scoped\n"
       "  irreducible_def foo : True := by\n"
       "  proof_wanted foo : True\n"
       "  export OneMemClass (one_mem)\n"
       "  include hf hg\n"
       "  include S f in\n"
       "  omit [Foo α] [Bar β] in\n"
       "  unseal Foo.bar in\n"
       "  suppress_compilation in\n"
       "  unsuppress_compilation in\n"
       "  macro_rules | `(foo) => `(bar)\n"
       "  nonrec\n"
       "  unif_hint foo (R R' : C) where\n"
       "  private\n"
       "  alias ⟨foo, _⟩ := bar\n"
       "  noncomputable\n"
       "  extend_docs Foo.bar after\n"
       "  run_cmd Foo.bar do\n"
       "  mk_iff_of_inductive_prop Foo.bar Foo.baz\n"
       "  assert_not_exists Foo Bar\n"
       "  elab_rules : tactic | `(tactic| foo) => do\n")
    (dolist (expected '("initialize_simps_projections Foo (bar → baz)"
                        "grind_pattern Foo.bar => True"
                        "local grind_pattern Foo.bar => True"
                        "scoped"
                        "irreducible_def foo : True := by"
                        "proof_wanted foo : True"
                        "export OneMemClass (one_mem)"
                        "include hf hg"
                        "include S f in"
                        "omit [Foo α] [Bar β] in"
                        "unseal Foo.bar in"
                        "suppress_compilation in"
                        "unsuppress_compilation in"
                        "macro_rules | `(foo) => `(bar)"
                        "nonrec"
                        "unif_hint foo (R R' : C) where"
                        "private"
                        "alias ⟨foo, _⟩ := bar"
                        "noncomputable"
                        "extend_docs Foo.bar after"
                        "run_cmd Foo.bar do"
                        "mk_iff_of_inductive_prop Foo.bar Foo.baz"
                        "assert_not_exists Foo Bar"
                        "elab_rules : tactic | `(tactic| foo) => do"))
      (funcall indent-line-function)
      (should (equal (lean4-test--line-string) expected))
      (forward-line 1))))

(ert-deftest lean4-indent--top-level-library-note2-snaps-left ()
  (lean4-test-with-indent-buffer
      (concat
       "  library_note2 foo /--\n"
       "bar\n"
       "-/\n")
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "library_note2 foo /--"))))

(ert-deftest lean4-indent--top-level-anchor-notation-forms-snap-left ()
  (lean4-test-with-indent-buffer
      (concat
      "  local notation3 \"coeffs(\"p\")\" => Set.range (coeff p)\n"
      "  scoped infixl:50 \" ~> \" => Promises\n"
      "  prefix:100 \"foo\" => bar\n"
      "  local prefix:arg \"ι\" => Ordnode.singleton\n")
    (dolist (expected '("local notation3 \"coeffs(\"p\")\" => Set.range (coeff p)"
                        "scoped infixl:50 \" ~> \" => Promises"
                        "prefix:100 \"foo\" => bar"
                        "local prefix:arg \"ι\" => Ordnode.singleton"))
      (funcall indent-line-function)
      (should (equal (lean4-test--line-string) expected))
      (forward-line 1))))

(ert-deftest lean4-indent--top-level-nonrec-theorem-snaps-left ()
  (lean4-test-with-indent-buffer "  nonrec theorem foo : True := by\n"
    (goto-char (point-min))
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "nonrec theorem foo : True := by"))))

(ert-deftest lean4-indent--top-level-termination-and-register-option-snap-left ()
  (lean4-test-with-indent-buffer
      (concat
       "  termination_by foo\n"
       "  meta register_option mathlib.foo : Bool := {\n")
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "termination_by foo"))
    (forward-line 1)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "meta register_option mathlib.foo : Bool := {"))))

(ert-deftest lean4-indent--top-level-deriving-snaps-left ()
  (lean4-test-with-indent-buffer
      (concat
       "inductive Foo where\n"
       "  | bar\n"
       "  deriving BEq\n")
    (forward-line 2)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "deriving BEq"))))

(ert-deftest lean4-indent--wrapped-top-level-deriving-instance-line-snaps-left ()
  (lean4-test-with-indent-buffer
      (concat
       "deriving instance\n"
       "  T2Space, CommGroup,\n"
       "  for PontryaginDual A\n")
    (forward-line 2)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "for PontryaginDual A"))))

(ert-deftest lean4-indent--top-level-equation-branch-snaps-left ()
  (lean4-test-with-indent-buffer
      (concat
       "meta def foo : Nat → Nat\n"
       "  | n => n\n")
    (forward-line 1)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "| n => n"))))

(ert-deftest lean4-indent--top-level-hash-commands-snap-left ()
  (lean4-test-with-indent-buffer
      (concat
       "  #guard_msgs in\n"
       "  #eval foo\n"
       "  #check Nat\n"
       "  #adaptation_note\n")
    (dolist (expected '("#guard_msgs in" "#eval foo" "#check Nat" "#adaptation_note"))
      (funcall indent-line-function)
      (should (equal (lean4-test--line-string) expected))
      (forward-line 1))))

(ert-deftest lean4-indent--top-level-closing-bracket-snaps-left ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : Array Nat := #[\n"
       "  1,\n"
       "  2\n"
       "  ]\n")
    (forward-line 3)
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string) "]"))))

(ert-deftest lean4-indent--top-level-more-command-forms-snap-left ()
  (lean4-test-with-indent-buffer
      (concat
       "  initialize addLinter commandRangesLinter\n"
       "  add_aesop_rules safe tactic\n"
       "  local macro_rules | `(foo) => `(bar)\n"
       "  partial\n"
       "  elab (name := deprecated_modules)\n"
       "  public register_option linter.privateModule : Bool := {\n")
    (dolist (expected '("initialize addLinter commandRangesLinter"
                        "add_aesop_rules safe tactic"
                        "local macro_rules | `(foo) => `(bar)"
                        "partial"
                        "elab (name := deprecated_modules)"
                        "public register_option linter.privateModule : Bool := {"))
      (funcall indent-line-function)
      (should (equal (lean4-test--line-string) expected))
      (forward-line 1))))

(ert-deftest lean4-indent--top-level-insert-to-additive-translation-snaps-left ()
  (lean4-test-with-indent-buffer
      "  insert_to_additive_translation Foo Bar\n"
    (funcall indent-line-function)
    (should (equal (lean4-test--line-string)
                   "insert_to_additive_translation Foo Bar"))))

(lean4-define-final-line-indent-test
 lean4-indent--top-level-attribute-after-open
 "open Pointwise

@[simp, norm_cast]")

(lean4-define-final-line-indent-test
 lean4-indent--scoped-attribute-after-where-block
"protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where
  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)
  one_smul S := (congr_arg (fun f => S.map f) (AlgHom.ext <| one_smul R')).trans S.map_id
  mul_smul _a₁ _a₂ S :=
    (congr_arg (fun f => S.map f) (AlgHom.ext <| mul_smul _ _)).trans (S.map_map _ _).symm

scoped[Pointwise] attribute [instance] Subalgebra.pointwiseMulAction")

(lean4-define-final-line-indent-test
 lean4-indent--spaced-scoped-attribute-after-open
 "open Pointwise

scoped [Pointwise] attribute [instance] Subalgebra.pointwiseMulAction")

(lean4-define-final-line-indent-test
 lean4-indent--top-level-variable-after-section
 "section Pointwise

variable {R : Type*} {A : Type*} [CommSemiring R] [Semiring A] [Algebra R A]")

(lean4-define-final-line-indent-test
 lean4-indent--wrapped-top-level-variable-line
 "variable {ι R : Type*} {S : ι → Type*} [CommSemiring R] [∀ i, Semiring (S i)] [∀ i, Algebra R (S i)]
  {s : Set ι} {t t₁ t₂ : ∀ i, Subalgebra R (S i)} {x : ∀ i, S i}")

(ert-deftest lean4-indent--dedents-after-sorry-with-comment ()
  (lean4-test-with-indent-buffer
      (concat
       "example : 37 = 37 ∧ 73 = 73 := by\n"
       "  sorry -- comment\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("#check 37" "#check 37")
       ("#check 73" "#check 73")))))

(ert-deftest lean4-indent--newline-after-top-level-proof-dedents ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  trivial\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "  ")))

(ert-deftest lean4-indent--newline-after-top-level-check-dedents ()
  (lean4-test-with-indent-buffer "#check 37\n"
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "")))

(ert-deftest lean4-indent--newline-after-section-stays-flush-left ()
  (lean4-test-with-indent-buffer "section Pointwise\n"
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "")))

(ert-deftest lean4-indent--newline-after-single-line-top-level-decl-dedents ()
  (lean4-test-with-indent-buffer "example : Nat := 2\n"
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "")))

(ert-deftest lean4-indent--newline-inside-tactic-proof-keeps-prev-indent ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sb_right_inv {x : α} (hx : x ∉ sbSet f g) : g (invFun g x) = x := by\n"
       "  have : x ∈ g '' univ := by\n"
       "    contrapose! hx\n"
       "    rw [sbSet, mem_iUnion]\n"
       "    use 0\n"
       "    rw [sbAux, mem_diff]\n"
       "    constructor\n"
       "    · exact trivial\n"
       "    · assumption\n"
       "  have : ∃ y, g y = x := by\n"
       "    contrapose! hx\n"
       "    rw [sbSet, mem_iUnion]\n"
       "    use 0\n"
       "    rw [sbAux, mem_diff]\n"
       "    constructor\n"
       "    · exact trivial\n"
       "    simpa\n"
       "  rcases this with ⟨y, hy⟩\n"
       "  apply invFun_eq\n")
    (lean4-test--goto-line 18)
    (end-of-line)
    (lean4-test--newline-and-assert "  ")
    (delete-region (line-beginning-position) (line-end-position))
    (delete-char -1)
    (lean4-test--goto-line 19)
    (end-of-line)
    (lean4-test--newline-and-assert "  ")))

(ert-deftest lean4-indent--newline-after-ext-fun-theorem-body-stays-at-body-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem adjoin_ext {s : Set A} ⦃φ₁ φ₂ : adjoin R s →ₐ[R] B⦄\n"
       "    (h : ∀ x hx, φ₁ ⟨x, subset_adjoin hx⟩ = φ₂ ⟨x, subset_adjoin hx⟩) : φ₁ = φ₂ :=\n"
       "  ext fun ⟨x, hx⟩ ↦ adjoin_induction h (fun _ ↦ φ₂.commutes _ ▸ φ₁.commutes _)\n"
       "    (fun _ _ _ _ h₁ h₂ ↦ by convert congr_arg₂ (· + ·) h₁ h₂ <;> rw [← map_add] <;> rfl)\n"
       "    (fun _ _ _ _ h₁ h₂ ↦ by convert congr_arg₂ (· * ·) h₁ h₂ <;> rw [← map_mul] <;> rfl) hx\n"
       "\n"
       "theorem ext_of_eq_adjoin {S : Subalgebra R A} {s : Set A} (hS : S = adjoin R s) ⦃φ₁ φ₂ : S →ₐ[R] B⦄\n")
    (lean4-test--goto-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-let-left-arrow-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : IO Unit := do\n"
       "  let (x, newState) ←\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-paren-led-application-line-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : IO Unit := do\n"
       "  let (x, newState) ←\n"
       "    (withOptions (fun _ => info.options) x).toIO\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-paren-led-bare-head-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : Set Stmt :=\n"
       "  tr_cfgs ∪\n"
       "    (trStmts₁\n"
       "        (move₂ (fun _ => false) main aux <| Λ'.ret k))\n")
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-nested-top-level-do-expression-stays-in-do-body ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : IO Unit := do\n"
       "  modify fun state => { state with\n"
       "    messages := state.messages ++ newState.messages,\n"
       "    traceState.traces := state.traceState.traces ++ newState.traceState.traces }\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "  ")))

(ert-deftest lean4-indent--newline-after-inline-open-paren-arg-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : IO Unit := do\n"
       "  (·.1) <$> info.runCoreMWithMessages (Lean.Meta.MetaM.run\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-record-argument-line-stays-at-sibling-column ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : IO Unit := do\n"
       "  let (x, newState) ←\n"
       "    (withOptions (fun _ => info.options) x).toIO\n"
       "      { currNamespace := info.currNamespace, openDecls := info.openDecls\n"
       "        fileName := ctx.fileName, fileMap := ctx.fileMap }\n"
       "      { env, ngen := info.ngen }\n")
    (lean4-test--goto-line 5)
    (end-of-line)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-calc-indents-to-step-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc  2 * 3\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "          ")))

(ert-deftest lean4-indent--newline-after-with-line-indents-branches ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  induction n with\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-rcases-line-allows-with-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo (hg : True) : True := by\n"
       "  rcases hg\n"
       "    with ⟨h⟩\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-wrapped-rcases-line-allows-deeper-with-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  rcases eventually_closure_subset_of_isCompact_absorbing_of_isOpen_of_omegaLimit_subset f ϕ s hc₁\n"
       "      hc₂ hn₁ hn₂ with\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-proof-with-line-indents-its-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have h : True := by\n"
       "    cases h with\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-show-from-line-indents-proof-term ()
  (lean4-test-with-indent-buffer
      (concat
       "def subtypeEquivCodomain (f : { x' // x' ≠ x } → Y) :\n"
       "    { g : X → Y // g ∘ (↑) = f } ≃ Y :=\n"
       "  (subtypePreimage _ f).trans <|\n"
       "    @funUnique { x' // ¬x' ≠ x } _ <|\n"
       "      show Unique { x' // ¬x' ≠ x } from\n"
       "        @Equiv.unique _ _\n")
    (goto-char (point-min))
    (forward-line 4)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-all-goals-indents-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  all_goals\n"
       "    simp only [foo]\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-any-goals-indents-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  any_goals\n"
       "    obtain ⟨h₁, h₂⟩ := h\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-split-ifs-with-indents-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "      split_ifs with h\n"
       "        <;> simp only [List.reverse_cons]\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-apply-line-indents-following-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  apply _root_.Partrec.of_eq_tot\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-filter-upwards-line-indents-with-clause ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  filter_upwards [eventually_true] with h\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-refine-line-with-inline-term-indents-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "    refine And.intro ?_ ?_\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-local-have-type-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have h : True ∧\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-local-have-equality-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem units_semiconj_of_translationNumber_eq {f₁ f₂ : CircleDeg1Liftˣ}\n"
       "    (h : τ f₁ = τ f₂) :\n"
       "    ∃ F : CircleDeg1Lift, Semiconj F f₁ f₂ :=\n"
       "  have : ∀ n : Multiplicative ℤ,\n"
       "      τ ((Units.coeHom _).comp (zpowersHom _ f₁) n) =\n"
       "        τ ((Units.coeHom _).comp (zpowersHom _ f₂) n) := fun n ↦ by\n")
    (goto-char (point-min))
    (forward-line 4)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-bullet-have-wrapped-type-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  refine ext fun b => ⟨fun H => ?_, fun H => ?_⟩\n"
       "  · rcases mem_bind_iff.1 H with ⟨c, _, h₂⟩\n"
       "    exact h₂\n"
       "  · have : ∀ m, (Nat.rec (motive := fun _ => Part σ)\n"
       "          (Part.some (g a)) (fun y IH => IH.bind fun _ => h a n) m).Dom := by\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-local-have-inline-application-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have cf := Primrec.fst.comp (Primrec.snd (α := X)\n"
       "      (β := Y)\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-apply-rules-line-indents-following-list ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  apply_rules\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-quantifier-line-ending-comma-indents-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have :\n"
       "    ∀ {m n},\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-comma-inside-delimited-term-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  rw [add_sq,\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-wrapped-top-level-header-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma foo :\n"
       "    ∀ x,\n"
       "          True := by\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-proof-wanted-header-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "proof_wanted TM2ComputableInPolyTime.comp\n"
       "    {α β γ : Type} {eα : FinEncoding α} {eβ : FinEncoding β}\n")
    (lean4-test--goto-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-calc-step-coloneq-indents-proof-term ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc (fun x => x) = fun x => x :=\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "              ")))

(ert-deftest lean4-indent--newline-after-inline-calc-aligns-under-relation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have := calc 1 < 2 := by decide\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "                 ")))

(ert-deftest lean4-indent--newline-after-calc-step-operator-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "      _ = (a +\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "            ")))

(ert-deftest lean4-indent--newline-after-calc-step-by-indents-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "      _ = a := by\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "            ")))

(ert-deftest lean4-indent--newline-after-inline-calc-inequality-by-indents-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "          calc (0 : ℝ) ≤ (2 : ℝ) ^ n * max n₀ 2 := by\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "                        ")))

(ert-deftest lean4-indent--newline-after-inline-calc-gcongr-indents-bullets ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "      _ ≥ c₂ * f x + c₄ * g x := by gcongr\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "                                    ")))

(ert-deftest lean4-indent--newline-after-filter-upwards-bracket-block-indents-with ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  filter_upwards [eventually_gt_atTop 0,\n"
       "      hfnew, hf_nonneg,\n"
       "      (tendsto_id.const_mul_atTop hb.1).eventually_forall_ge_atTop hf_nonneg]\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "        ")))

(ert-deftest lean4-indent--newline-inside-filter-upwards-bracket-block-indents-projection-arg ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  filter_upwards [eventually_gt_atTop 1,\n"
       "                  (tendsto_id.const_mul_atTop hb.1).eventually_forall_ge_atTop\n"
       "                    <| h_tendsto.eventually (eventually_gt_atTop (-Real.log b))] with x hx_pos hx\n")
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-coloneq-header-indents-body ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma injective_sumCoeffsExp : Function.Injective (fun (p : Real) => p) :=\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-wrapped-binder-coloneq-header-keeps-column ()
  (lean4-test-with-indent-buffer
      (concat
       "def Estimator.improveUntilAux\n"
       "    (a : Thunk α) (p : α → Bool) [Estimator a ε]\n"
       "    [WellFoundedGT (range (bound a : ε → α))]\n"
       "    (e : ε) (r : Bool) : Except (Option ε) ε :=\n"
       "    if p (bound a e) then\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-wrapped-binder-coloneq-with-inline-fun-dedents-to-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem option_bind {f : α → Option β} {g : α → β → Option σ} (hf : Computable f)\n"
       "    (hg : Computable₂ g) : Computable fun a => (f a).bind (g a) :=\n"
       "  (option_casesOn hf (const Option.none) hg).of_eq fun a => by cases f a <;> rfl\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-wrapped-theorem-quantifier-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem star_rmatch_iff (P : RegularExpression α) :\n"
       "    ∀ x : List α, (star P).rmatch x ↔ ∃ S : List (List α), x\n"
          "          = S.flatten ∧ ∀ t ∈ S, t ≠ [] ∧ P.rmatch t :=\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-paren-led-theorem-proposition-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem decode_prod_val (n : ℕ) :\n"
       "    (@decode (α × β) _ n : Option (α × β))\n"
       "      = (decode n.unpair.1).bind fun a => (decode n.unpair.2).map <| Prod.mk a := by\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-bare-top-level-deriving-indents-classes ()
  (lean4-test-with-indent-buffer
      (concat
       "inductive Foo\n"
       "  | bar\n"
       "deriving\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "  ")))

(ert-deftest lean4-indent--newline-after-suffices-proposition-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · suffices ∀ a', True → ∀ k, True →\n"
        "        True by\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-suffices-binder-comma-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  suffices ∀ i f L' R' l₁ l₂ h,\n"
       "      True by\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-suffices-equation-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  suffices ∀ f, stepAux (readAux n f) v (trTape' enc0 L R) =\n"
       "      stepAux (f (enc R.head)) v (trTape' enc0 (L.cons R.head) R.tail) by\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-havei-inline-value-stays-at-proof-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  haveI := Classical.decEq Nat\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "  ")))

(ert-deftest lean4-indent--newline-after-at-qualified-head-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo : { n // True } :=\n"
       "  suffices ∀ k, True → { n // True } from\n"
       "    this 0 fun _ => trivial\n"
       "  @WellFounded.fix _ _ wf\n"
       "    (by\n")
    (lean4-test--goto-eob)
    (forward-line -1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-lowercase-at-qualified-head-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "      @prec' 1 _ _\n"
       "        (fun v => by\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-pipe-left-continuation-indents-argument ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  exact h\n"
       "    <| f x\n"
       "        y\n")
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-focus-refine-indents-argument ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · refine IsBigO.mul_isLittleO\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-bare-focus-refine-with-comment-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · refine -- pred\n"
       "      supports_insert.2 ⟨⟨fun _ => h₁, fun _ => h₂.2 _ (Or.inl W),\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-operator-led-continuation-keeps-indent ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "      _ = (a + b\n"
       "                - c\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "                ")))

(ert-deftest lean4-indent--newline-after-nested-coloneq-line-indents-proof-term ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "    have h : True :=\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "          ")))

(ert-deftest lean4-indent--newline-inside-proof-body-keeps-current-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have h : True := by\n"
       "    simp\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-focused-tactic-line-keeps-subproof-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  split_ifs with h\n"
       "  · simp only [List.tail_cons, false_and, Sum.inl.injEq, reduceCtorEq] at this\n"
       "    subst this\n")
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-plain-application-line-indents-following-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have h :=\n"
       "    Foo.bar baz qux\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-refine-qualified-name-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  refine DifferentiableOn.mul\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-calc-expression-line-indents-next-step ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "      f x\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "          ")))

(ert-deftest lean4-indent--newline-after-local-have-coloneq-by-indents-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have h : True := by\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-bullet-exact-by-pipeline-indents-proof ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · exact h_base _ <| by\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-bullet-exact-application-indents-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · exact h a\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-bullet-exact-qualified-head-indents-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · exact Nat.eq_sqrt.2\n"
       "      ⟨h₁, h₂⟩\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bullet-exact-parenthesized-projection-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "instance [μ.IsMulLeftInvariant] : ErgodicSMul G G μ := by\n"
       "  refine ⟨fun {s} hsm hs ↦ ?_⟩\n"
       "  suffices (∃ᵐ x ∂μ, x ∈ s) → ∀ᵐ x ∂μ, x ∈ s by\n"
       "    simp only [eventuallyConst_set, ← not_frequently]\n"
       "    exact or_not_of_imp this\n"
       "  intro hμs\n"
       "  obtain ⟨a, has, ha⟩ : ∃ a ∈ s, ∀ᵐ b ∂μ, (b * a ∈ s ↔ a ∈ s) := by\n"
       "    refine (hμs.and_eventually ?_).exists\n"
       "    rw [ae_ae_comm]\n"
       "    · exact ae_of_all _ fun b ↦ (hs b).mem_iff\n"
       "    · exact ((hsm.preimage <| measurable_snd.mul measurable_fst).mem.iff\n"
       "        (hsm.preimage measurable_fst).mem).setOf\n")
    (goto-char (point-min))
    (forward-line 9)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-obtain-proposition-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  obtain ⟨d, -, hd⟩ : ∃ d, d ∈ s ∧ ∀ {ι'} {l : Filter ι'} (w : ι' → AddCircle T) (δ : ι' → ℝ),\n"
       "    Tendsto δ l (𝓝[>] 0) → (∀ᶠ j in l, d ∈ closedBall (w j) (1 * δ j)) →\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-inline-refine-angle-comma-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem preErgodic_conjugate_iff {e : α ≃ᵐ β} (h : MeasurePreserving e μ μ') :\n"
       "    PreErgodic (e ∘ f ∘ e.symm) μ' ↔ PreErgodic f μ := by\n"
       "  refine ⟨fun hf => preErgodic_of_preErgodic_semiconj (h.symm e) hf ?_,\n"
       "      fun hf => preErgodic_of_preErgodic_semiconj h hf ?_⟩\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-obtain-inline-application-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  obtain ⟨d, -, hd⟩ : ∃ d, d ∈ s :=\n"
       "    exists_mem_of_measure_ne_zero_of_ae h\n"
       "      (IsUnifLocDoublingMeasure.ae_tendsto_measure_inter_div μ s 1)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-second-line-of-coloneq-application-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem ae_empty_or_univ_of_forall_vadd_ae_eq_self {s : Set <| AddCircle T} : True := by\n"
       "  right\n"
       "  obtain ⟨d, -, hd⟩ : ∃ d, d ∈ s ∧ Tendsto f l (𝓝 1) :=\n"
       "    exists_mem_of_measure_ne_zero_of_ae h\n"
       "      (IsUnifLocDoublingMeasure.ae_tendsto_measure_inter_div μ s 1)\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-shallow-coloneq-application-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have hμ_diff : μ (f ⁻¹' s \\ s) = μ (s \\ f ⁻¹' s) :=\n"
       "    measure_diff_symm (hfμ.measurable hsm).nullMeasurableSet hsm.nullMeasurableSet\n"
       "      (hfμ.measure_preimage hsm.nullMeasurableSet) (by finiteness)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-closing-rw-bracket-dedents-to-sibling-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have hu₀ : ∀ j, True := fun j => by\n"
       "    trivial\n"
       "  have hnu : ∀ j, True := fun j => by\n"
       "    rw [← addOrderOf_dvd_iff_zsmul_eq_zero, hu₀, Int.natCast_pow, Int.natCast_natAbs, ← abs_pow,\n"
       "      abs_dvd]\n"
       "  have hu₁ : ∀ j, True := fun j => by\n")
    (goto-char (point-min))
    (forward-line 5)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-pipe-left-qualified-head-indents-following-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  | n + 1 => (matches'_mul _ _).trans <| Eq.trans\n"
       "      (congrFun (congrArg HMul.hMul (matches'_pow P n)) (matches' P))\n")
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-calc-relop-projection-application-indents-deeply ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "           ≤ (Finset.Ico a b).sup' h\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "                  ")))

(ert-deftest lean4-indent--newline-after-calc-relop-projection-application-allows-bare-lambda ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  calc\n"
       "           ≥ (Finset.Ico a b).inf' h\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "                  ")))

(ert-deftest lean4-indent--newline-after-nested-fat-arrow-under-parens-keeps-body-depth ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo {f : ℕ →. ℕ} (hf : Nat.Partrec f) : True := by\n"
       "  refine\n"
       "    Partrec.map\n"
       "      ((@Partrec₂.unpaired' fun a b : ℕ =>\n"
       "            Nat.rfind fun n => (fun m => m = 0) <$> f (Nat.pair a (n + b))).1\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bare-application-head-in-local-body-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo {f : ℕ →. ℕ} (hf : Nat.Partrec f) : True := by\n"
       "  have : Nat.Partrec (fun a => Nat.rfind (fun n => f (Nat.pair a n))) :=\n"
       "    rfind\n"
       "      (Partrec₂.unpaired'.2\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-nested-projection-application-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo {f : ℕ →. ℕ} (hf : Nat.Partrec f) : True := by\n"
       "  have : Nat.Partrec (fun a => Nat.rfind (fun n => f (Nat.pair a n))) :=\n"
       "    rfind\n"
       "      (Partrec₂.unpaired'.2\n"
       "        ((Partrec.nat_iff.2 hf).comp\n"
       "            (Primrec₂.pair.comp Primrec.fst Primrec.snd)\n")
    (goto-char (point-min))
    (forward-line 4)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-change-line-indents-equation-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  change G₁ ((a, xs), n, m)\n"
       "    = some (F a (ofNat Code (n + 4)))\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-cond-line-indents-branches ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  cond n.bodd\n"
       "    (cond n.div2.bodd (rf a (ofNat Code m, s))\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-application-body-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True :=\n"
       "  primrec_recOn' hc hz hs hl hr\n"
       "    (pr := fun a b => pr a b.1 b.2)\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-application-with-inline-fun-keeps-args ()
  (lean4-test-with-indent-buffer
      (concat
       "def bestFirstSearch (f : α → MLList m α) (a : α) : MLList m α :=\n"
       "  bestFirstSearchCore Thunk.pure (fun a : α => { x // x = a }) f a\n"
       "    (β := α)\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-top-level-application-with-universe-args-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "abbrev functor : Type u ⥤ LightCondSet.{u} :=\n"
       "  CompHausLike.LocallyConstant.functor.{u, u}\n"
       "    (P := fun X ↦ TotallyDisconnectedSpace X ∧ SecondCountableTopology X)\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-bare-head-indents-first-arg-deeply ()
  (lean4-test-with-indent-buffer
      (concat
       "@[simp]\n"
       "theorem contSupp_cons₁ (fs k) :\n"
       "    contSupp (Cont'.cons₁ fs k) =\n"
       "      trStmts₁\n"
       "          (move₂ (fun _ => false) main aux <|)\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-application-under-operator-continuation-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem tr_respects_aux₂ :\n"
       "    ∃ L' : ListBlank (∀ k, Option (Γ k)),\n"
       "      (∀ k, L'.map (proj k) = ListBlank.mk ((S' k).map some).reverse) ∧\n"
       "        TM1.stepAux (trStAct q o) v\n"
       "            ((Tape.move Dir.right)^[(S k).length] (Tape.mk' ∅ (addBottom L))) =\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-application-under-quantifier-anchor-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem nat_iff {f : α → β → σ} : Primrec₂ f ↔ Nat.Primrec\n"
       "    (.unpaired fun m n => encode <| (@decode α _ m).bind fun a => (@decode β _ n).map (f a)) := by\n"
       "  have :\n"
       "    ∀ (a : Option α) (b : Option β),\n"
       "      Option.map (fun p : α × β => f p.1 p.2)\n"
       "          (Option.bind a fun a : α => Option.map (Prod.mk a) b) =\n")
    (goto-char (point-min))
    (forward-line 4)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-application-with-inline-fun-indents-next-arg ()
  (lean4-test-with-indent-buffer
      (concat
       "def liftOn₂ : φ :=\n"
       "  d₁.liftOn (fun p => d₂.liftOn (f p) fun _ _ hq => h _ _ _ _ (by rfl) hq)\n"
       "    (by\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-simpa-using-indents-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  simpa [a0, add_comm, add_left_comm] using\n"
       "    evaln_mono h hk₂\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-induction-with-indents-branch-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  induction c generalizing n x with\n"
       "      simp [eval, evaln] at h ⊢\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-tactic-first-indents-branch-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : Decidable (a = b) := by\n"
       "  induction a generalizing b <;> cases b <;> first\n"
       "    | apply Decidable.isFalse; rintro ⟨⟨⟩⟩; done\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-simp-says-indents-rewrite ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  simp? [Bind.bind, Option.bind_eq_some_iff] at h ⊢ says\n"
       "    simp only [bind, Option.mem_def] at h ⊢\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-simp-only-brackets-indents-at-clause ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem stmts₁_supportsStmt_mono {S : Finset Λ} {q₁ q₂ : Stmt Γ Λ σ} (h : q₁ ∈ stmts₁ q₂)\n"
       "    (hs : SupportsStmt S q₂) : SupportsStmt S q₁ := by\n"
       "  induction q₂ with\n"
       "    simp only [stmts₁, SupportsStmt, Finset.mem_insert, Finset.mem_union, Finset.mem_singleton]\n"
       "      at h hs\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bullet-have-inline-projection-rhs-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · have L := (Primrec.fst.comp Primrec.fst).comp\n"
       "      (Primrec.fst (α := X) (β := Y))\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bullet-have-by-indents-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem Fin.snoc_eq_cons_rotate {α : Type*} (v : Fin n → α) (a : α) :\n"
       "    @Fin.snoc _ (fun _ => α) v a = fun i => @Fin.cons _ (fun _ => α) a v (finRotate _ i) := by\n"
       "  ext ⟨i, h⟩\n"
       "  by_cases h' : i < n\n"
       "  · rw [finRotate_of_lt h', Fin.snoc, Fin.cons, dif_pos h']\n"
       "    rfl\n"
       "  · have h'' : n = i := by\n"
       "      simp only [not_lt] at h'\n")
    (goto-char (point-min))
    (forward-line 6)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-rcases-have-rhs-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem denumerable_list_aux : ∀ n : ℕ, ∃ a ∈ @decodeList α _ n, encodeList a = n\n"
       "  | 0 => by rw [decodeList]; exact ⟨_, rfl, rfl⟩\n"
       "  | succ v => by\n"
       "    rcases e : unpair v with ⟨v₁, v₂⟩\n"
       "    have h := unpair_right_le v\n"
       "    rw [e] at h\n"
       "    rcases have : v₂ < succ v := lt_succ_of_le h\n"
       "      denumerable_list_aux v₂ with\n")
    (goto-char (point-min))
    (forward-line 6)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-inline-then-body-stays-at-else-column ()
  (lean4-test-with-indent-buffer
      (concat
       "def removeNone_aux (x : α) : β :=\n"
       "  if h : (e (some x)).isSome then Option.get _ h\n"
       "  else\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-structure-field-fun-opens-body ()
  (lean4-test-with-indent-buffer
      (concat
       "def optionSubtype [DecidableEq β] (x : β) :\n"
       "    { e : Option α ≃ β // e none = x } ≃ (α ≃ { y : β // y ≠ x }) where\n"
       "  toFun e :=\n"
       "    { toFun := fun a =>\n"
       "        ⟨(e : Option α ≃ β) a, ((EquivLike.injective _).ne_iff' e.property).2 (some_ne_none _)⟩,\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-closing-simp-only-line-returns-to-clause ()
  (lean4-test-with-indent-buffer
      (concat
       "def optionSubtype [DecidableEq β] (x : β) :\n"
       "    { e : Option α ≃ β // e none = x } ≃ (α ≃ { y : β // y ≠ x }) where\n"
       "  invFun e :=\n"
       "    ⟨{  toFun := fun a => casesOn' a x (Subtype.val ∘ e),\n"
       "        invFun := fun b => if h : b = x then none else e.symm ⟨b, h⟩,\n"
       "        left_inv := fun a => by\n"
       "          cases a with\n"
       "          | none => simp\n"
       "          | some a =>\n"
       "            simp only [casesOn'_some, Function.comp_apply, Subtype.coe_eta,\n"
       "              symm_apply_apply, dite_eq_ite]\n"
       "            exact if_neg (e a).property,\n")
    (goto-char (point-min))
    (forward-line 10)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 12)))

(ert-deftest lean4-indent--newline-after-nested-paren-projection-head-indents-sibling-deeply ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have hF : Primrec₂ F :=\n"
       "    (list_foldl fst (sumInl.comp snd)\n"
       "      ((sumCasesOn fst (nat_casesOn snd (sumInr.comp <| snd.comp fst) (sumInl.comp snd).to₂).to₂\n"
       "              (sumInr.comp snd).to₂).comp\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-projection-tail-with-paren-arg-indents-next-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have h : Partrec fun a : α × ℕ =>\n"
       "    (encode (decode (α := β) a.2)).casesOn (some Option.none)\n"
       "      fun n => Part.map (f a.1) (decode (α := β) n) :=\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-paren-led-application-tail-indents-next-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  option_some_iff.1 <|\n"
       "    (cond (nat_bodd.comp <| encode_iff.2 hf)\n"
       "          (option_map (Computable.decode.comp <| nat_div2.comp <| encode_iff.2 hf) hh)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-option-map-line-stays-at-sibling-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sumCasesOn {f : α → β ⊕ γ} {g : α → β → σ} {h : α → γ → σ} (hf : Computable f)\n"
       "    (hg : Computable₂ g) (hh : Computable₂ h) :\n"
       "    @Computable _ σ _ _ fun a => Sum.casesOn (f a) (g a) (h a) :=\n"
       "  option_some_iff.1 <|\n"
       "    (cond (nat_bodd.comp <| encode_iff.2 hf)\n"
       "          (option_map (Computable.decode.comp <| nat_div2.comp <| encode_iff.2 hf) hh)\n"
       "          (option_map (Computable.decode.comp <| nat_div2.comp <| encode_iff.2 hf) hg)).of_eq\n")
    (goto-char (point-min))
    (forward-line 5)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 10)))

(ert-deftest lean4-indent--newline-after-wrapped-binder-line-does-not-overindent-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sumCasesOn {f : α → β ⊕ γ} {g : α → β → σ} {h : α → γ → σ} (hf : Computable f)\n"
       "    (hg : Computable₂ g) (hh : Computable₂ h) :\n"
       "    @Computable _ σ _ _ fun a => Sum.casesOn (f a) (g a) (h a) :=\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-wrapped-coloneq-line-does-not-overindent-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sumCasesOn {f : α → β ⊕ γ} {g : α → β → σ} {h : α → γ → σ} (hf : Computable f)\n"
       "    (hg : Computable₂ g) (hh : Computable₂ h) :\n"
       "    @Computable _ σ _ _ fun a => Sum.casesOn (f a) (g a) (h a) :=\n"
       "  option_some_iff.1 <|\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-closed-projection-tail-does-not-overindent-next-arg ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem sumCasesOn {f : α → β ⊕ γ} {g : α → β → σ} {h : α → γ → σ} (hf : Computable f)\n"
       "    (hg : Computable₂ g) (hh : Computable₂ h) :\n"
       "    @Computable _ σ _ _ fun a => Sum.casesOn (f a) (g a) (h a) :=\n"
       "  option_some_iff.1 <|\n"
       "    (cond (nat_bodd.comp <| encode_iff.2 hf)\n"
       "          (option_map (Computable.decode.comp <| nat_div2.comp <| encode_iff.2 hf) hh)\n"
       "          (option_map (Computable.decode.comp <| nat_div2.comp <| encode_iff.2 hf) hg)).of_eq\n"
       "      fun a => by\n")
    (goto-char (point-min))
    (forward-line 6)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-comma-led-list-item-keeps-wrapped-argument-indent ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem isDiscrete_tfae (X : CondensedSet.{u}) :\n"
       "    TFAE\n"
       "    [ X.IsDiscrete\n"
       "    , IsIso ((Condensed.discreteUnderlyingAdj _).counit.app X)\n"
       "    , (Condensed.discrete _).essImage X\n"
       "    , CondensedSet.LocallyConstant.functor.essImage X\n"
       "    , IsIso (CondensedSet.LocallyConstant.adjunction.counit.app X)\n"
       "    , Sheaf.IsConstant (coherentTopology Profinite)\n"
       "        ((Condensed.ProfiniteCompHaus.equivalence _).inverse.obj X)\n"
       "    ] := by\n")
    (goto-char (point-min))
    (forward-line 7)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-tfae-have-inline-application-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  tfae_have 1 ↔ 4 := Sheaf.isConstant_iff_mem_essImage _\n"
       "    LightProfinite.isTerminalPUnit (adjunction R) _\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-convert-line-indents-next-proof-step ()
  (lean4-test-with-indent-buffer
      (concat
       "def componentHom (a : Fiber (f.comap g.hom)) :\n"
       "    fiber _ a ⟶ fiber _ (Fiber.mk f (g a.preimage)) :=\n"
       "  TopCat.ofHom\n"
       "  { toFun x := ⟨g x.val, by\n"
       "      simp only [Fiber.mk, Set.mem_preimage, Set.mem_singleton_iff]\n"
       "      convert map_eq_image _ _ x\n"
       "      exact map_preimage_eq_image_map _ _ a⟩\n")
    (goto-char (point-min))
    (forward-line 5)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-inline-record-by-opens-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "def componentHom (a : Fiber (f.comap g.hom)) :\n"
       "    fiber _ a ⟶ fiber _ (Fiber.mk f (g a.preimage)) :=\n"
       "  TopCat.ofHom\n"
       "  { toFun x := ⟨g x.val, by\n"
       "      simp only [Fiber.mk, Set.mem_preimage, Set.mem_singleton_iff]\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-apply-ext-opens-named-args ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma adjunction_left_triangle [HasExplicitFiniteCoproducts.{u} P]\n"
       "    (X : Type max u w) : functorToPresheaves.{u, w}.map ((unit P hs).app X) ≫\n"
       "      ((counit P hs).app ((functor P hs).obj X)).val = 𝟙 (functorToPresheaves.obj X) := by\n"
       "  ext ⟨S⟩ (f : LocallyConstant _ X)\n"
       "  simp only [Functor.id_obj, Functor.comp_obj, FunctorToTypes.comp, NatTrans.id_app,\n"
       "    functorToPresheaves_obj_obj, types_id_apply]\n"
       "  simp only [counit, counitApp_app]\n"
       "  have := CompHausLike.preregular hs\n"
       "  apply presheaf_ext\n"
       "    (X := ((functor P hs).obj X).val) (Y := ((functor.{u, w} P hs).obj X).val)\n")
    (goto-char (point-min))
    (forward-line 8)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-apply-config-opens-args ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  apply (config := { allowSynthFailures := true }) equalizerCondition_yonedaPresheaf\n"
       "    (CompHausLike.compHausLikeToTop.{u} P) X\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bullet-intro-binders-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "noncomputable def isColimitLocallyConstantPresheaf (hc : IsLimit c) [∀ i, Epi (c.π.app i)] :\n"
       "    IsColimit <| (locallyConstantPresheaf X).mapCocone c.op := by\n"
       "  refine Types.FilteredColimit.isColimitOf _ _ ?_ ?_\n"
       "  · intro ⟨i⟩ ⟨j⟩ (fi : LocallyConstant _ _) (fj : LocallyConstant _ _)\n"
       "      (h : fi.comap (c.π.app i).hom = fj.comap (c.π.app j).hom)\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-branch-record-sibling-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem Applicative.ext {F} :\n"
       "    ∀ {A1 : Applicative F} {A2 : Applicative F}, A1 = A2\n"
       "  | { toFunctor := F1, seq := s1, pure := p1, seqLeft := sl1, seqRight := sr1 },\n"
       "    { toFunctor := F2, seq := s2, pure := p2, seqLeft := sl2, seqRight := sr2 },\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-branch-match-with-indents-inner-branch ()
  (lean4-test-with-indent-buffer
      (concat
       "protected def seq {α β : Type v} : Comp F G (α → β) → (Unit → Comp F G α) → Comp F G β\n"
       "  | Comp.mk f, g => match g () with\n"
       "    | Comp.mk x => Comp.mk <| (· <*> ·) <$> f <*> x\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-where-field-type-comma-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "class CommApplicative (m : Type u → Type v) [Applicative m] : Prop extends LawfulApplicative m where\n"
       "  commutative_prod : ∀ {α β} (a : m α) (b : m β),\n"
       "    Prod.mk <$> a <*> b = (fun (b : β) a => (a, b)) <$> b <*> a\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-where-field-forall-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "class LawfulBitraversable (t : Type u → Type u → Type u) [Bitraversable t] : Prop\n"
       "  extends LawfulBifunctor t where\n"
       "  comp_bitraverse :\n"
       "    ∀ {F G} [Applicative F] [Applicative G] [LawfulApplicative F] [LawfulApplicative G]\n"
       "      {α α' β β' γ γ'} (f : β → F γ) (f' : β' → F γ') (g : α → G β) (g' : α' → G β') (x : t α α'),\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-wrapped-coloneq-continuation-keeps-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem ωScottContinuous_uncurry :\n"
       "    ωScottContinuous (monotoneUncurry α β γ) :=\n"
       "    .of_map_ωSup_of_orderHom fun c ↦ by\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-top-level-where-body-head-opens-named-args ()
  (lean4-test-with-indent-buffer
      (concat
       "instance : LawfulMonad (ContT r m) := LawfulMonad.mk'\n"
       "  (id_map := by intros; rfl)\n")
    (goto-char (point-min))
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-inline-semicolon-simp-only-list-keeps-column ()
  (lean4-test-with-indent-buffer
      (concat
       "instance {ρ} [Monad m] [MonadCont m] [LawfulMonadCont m] : LawfulMonadCont (ReaderT ρ m) where\n"
       "  callCC_bind_right := by intros; simp only [callCC, ReaderT.callCC, ReaderT.run_bind,\n"
       "                                    callCC_bind_right]; ext; rfl\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 36)))

(ert-deftest lean4-indent--newline-after-wrapped-declaration-colon-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "def isoLocallyConstantOfIsColimit (hF : ∀ S : LightProfinite, IsColimit <|\n"
       "    F.mapCocone (coconeRightOpOfCone S.asLimitCone)) :\n"
       "      F ≅ (locallyConstantPresheaf\n"
       "        (F.obj (toLightProfinite.op.obj ⟨of PUnit.{u + 1}⟩))) :=\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-single-wrapped-binder-colon-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem equalizerCondition_yonedaPresheaf\n"
       "    [∀ (Z B : C) (π : Z ⟶ B) [EffectiveEpi π], PreservesLimit (cospan π π) G]\n"
       "    (hq : ∀ (Z B : C) (π : Z ⟶ B) [EffectiveEpi π], IsQuotientMap (G.map π)) :\n"
       "      EqualizerCondition (yonedaPresheaf G X) := by\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-wrapped-binder-colon-with-inline-fun-dedents-to-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem nat_casesOn {f : α → ℕ} {g : α → σ} {h : α → ℕ → σ} (hf : Computable f) (hg : Computable g)\n"
       "    (hh : Computable₂ h) :\n"
       "    Computable fun a => Nat.casesOn (motive := fun _ => σ) (f a) (g a) (h a) :=\n"
       "  nat_rec hf hg (hh.comp fst <| fst.comp snd).to₂\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-calc-step-proof-dedents-to-next-step ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem eq_one_iff_unique {α : Type*} : #α = 1 ↔ Subsingleton α ∧ Nonempty α :=\n"
       "  calc\n"
       "    #α = 1 ↔ #α ≤ 1 ∧ 1 ≤ #α := le_antisymm_iff\n"
       "    _ ↔ Subsingleton α ∧ Nonempty α :=\n"
       "      le_one_iff_subsingleton.and (one_le_iff_ne_zero.trans mk_ne_zero_iff)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-inside-calc-by-proof-keeps-proof-column ()
  (lean4-test-with-indent-buffer
      (concat
       "example : (a + b) * (a + b) = a * a + 2 * (a * b) + b * b :=\n"
       "  calc\n"
       "    (a + b) * (a + b) = a * a + b * a + (a * b + b * b) := by\n"
       "      rw [mul_add, add_mul, add_mul]\n"
       "    _ = a * a + (b * a + a * b) + b * b := by\n"
       "      rw [← add_assoc, add_assoc (a * a)]\n"
       "    _ = a * a + 2 * (a * b) + b * b := by\n"
       "      rw [mul_comm b a, ← two_mul]\n")
    (dolist (line '(4 6 8))
      (lean4-test--goto-line line)
      (end-of-line)
      (lean4-test--newline-and-assert "      ")
      (delete-region (line-beginning-position) (line-beginning-position 2)))))

(ert-deftest lean4-indent--newline-inside-truncated-calc-by-proof-keeps-proof-column ()
  (lean4-test-with-indent-buffer
      (concat
       "example : (a + b) * (a + b) = a * a + 2 * (a * b) + b * b :=\n"
       "  calc\n"
       "    (a + b) * (a + b) = a * a + b * a + (a * b + b * b) := by\n"
       "      rw [mul_add, add_mul, add_mul]\n")
    (lean4-test--goto-line 4)
    (end-of-line)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-proof-line-dedents-to-next-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem mk_le_iff_forall_finset_subset_card_le {α : Type u} {n : ℕ} {t : Set α} :\n"
       "    #t ≤ n ↔ ∀ s : Finset α, (s : Set α) ⊆ t → s.card ≤ n := by\n"
       "  refine ⟨fun H s hs ↦ by simpa using (mk_le_mk_of_subset hs).trans H, fun H ↦ ?_⟩\n"
       "  apply card_le_of (fun s ↦ ?_)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-before-termination-by-dedents-to-clause-column ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma mul_neg (x y : PGame) : x * -y = -(x * y) :=\n"
       "  by\n"
       "    change -(mk _ _ _ _ * _) ≡r _\n"
       "    exact (negMulRelabelling _ _).symm\n"
       "  termination_by (x, y)\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-before-deriving-dedents-to-clause-column ()
  (lean4-test-with-indent-buffer
      (concat
       "inductive Lists'.{u} (α : Type u) : Bool → Type u\n"
       "  | atom : α → Lists' α false\n"
       "  | nil : Lists' α true\n"
       "  | cons' {b} : Lists' α b → Lists' α true → Lists' α true\n"
       "  deriving DecidableEq\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-proof-intro-by-does-not-overindent-body ()
  (lean4-test-with-indent-buffer
      (concat
       "@[simp]\n"
       "theorem mem_rfind {p : ℕ →. Bool} {n : ℕ} :\n"
       "    n ∈ rfind p ↔ true ∈ p n ∧ ∀ {m : ℕ}, m < n → false ∈ p m :=\n"
       "  ⟨fun h => ⟨rfind_spec h, @rfind_min _ _ h⟩, fun ⟨h₁, h₂⟩ => by\n"
       "    let ⟨m, hm⟩ := dom_iff_mem.1 <| (@rfind_dom p).2 ⟨_, h₁, fun {m} mn => (h₂ mn).fst⟩\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-angle-fun-proofs-does-not-overindent ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem bind_decode_iff {f : α → β → Option σ} :\n"
       "    (Computable₂ fun a n => (decode (α := β) n).bind (f a)) ↔ Computable₂ f :=\n"
       "  ⟨fun hf =>\n"
       "    Nat.Partrec.of_eq\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-closed-application-line-dedents-to-next-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem bind_decode_iff {f : α → β → Option σ} :\n"
       "    (Computable₂ fun a n => (decode (α := β) n).bind (f a)) ↔ Computable₂ f :=\n"
       "  ⟨fun hf =>\n"
       "    Nat.Partrec.of_eq\n"
       "      (((Partrec.nat_iff.2\n"
       "        (Nat.Partrec.ppred.comp <| Nat.Partrec.of_primrec <| Primcodable.prim (α := β))).comp\n"
       "            snd).bind\n"
       "        (Computable.comp hf fst).to₂.partrec₂)\n"
       "      fun n => by\n")
    (goto-char (point-min))
    (forward-line 7)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-after-proof-branch-dedents-to-next-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem bind_decode_iff {f : α → β → Option σ} :\n"
       "    (Computable₂ fun a n => (decode (α := β) n).bind (f a)) ↔ Computable₂ f :=\n"
       "  ⟨fun hf =>\n"
       "    Nat.Partrec.of_eq\n"
       "      (((Partrec.nat_iff.2\n"
       "        (Nat.Partrec.ppred.comp <| Nat.Partrec.of_primrec <| Primcodable.prim (α := β))).comp\n"
       "            snd).bind\n"
       "        (Computable.comp hf fst).to₂.partrec₂)\n"
       "      fun n => by\n"
       "        simp only [decode_prod_val, decode_nat, Option.map_some, PFun.coe_val, bind_eq_bind,\n"
       "          bind_some, Part.map_bind, map_some]\n"
       "        cases decode (α := α) n.unpair.1 <;> simp\n"
       "        cases decode (α := β) n.unpair.2 <;> simp,\n"
       "    fun hf => by\n")
    (goto-char (point-min))
    (forward-line 12)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-closed-inner-application-dedents-to-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem bind_decode_iff {f : α → β → Option σ} :\n"
       "    (Computable₂ fun a n => (decode (α := β) n).bind (f a)) ↔ Computable₂ f :=\n"
       "  ⟨fun hf =>\n"
       "    Nat.Partrec.of_eq\n"
       "      (((Partrec.nat_iff.2\n"
       "        (Nat.Partrec.ppred.comp <| Nat.Partrec.of_primrec <| Primcodable.prim (α := β))).comp\n"
       "            snd).bind\n"
       "        (Computable.comp hf fst).to₂.partrec₂)\n")
    (goto-char (point-min))
    (forward-line 6)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-inner-coloneq-dedents-to-enclosing-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem bind_decode_iff {f : α → β → Option σ} :\n"
       "    (Computable₂ fun a n => (decode (α := β) n).bind (f a)) ↔ Computable₂ f :=\n"
       "  ⟨fun hf =>\n"
       "    Nat.Partrec.of_eq\n"
       "      fun n => by\n"
       "        simp only [decode_prod_val, decode_nat, Option.map_some, PFun.coe_val, bind_eq_bind,\n"
       "          bind_some, Part.map_bind, map_some]\n"
       "        cases decode (α := α) n.unpair.1 <;> simp\n"
       "        cases decode (α := β) n.unpair.2 <;> simp,\n"
       "    fun hf => by\n"
       "    have :\n"
       "      Partrec fun a : α × ℕ =>\n"
       "        (encode (decode (α := β) a.2)).casesOn (some Option.none)\n"
       "          fun n => Part.map (f a.1) (decode (α := β) n) :=\n"
       "      Partrec.nat_casesOn_right\n")
    (goto-char (point-min))
    (forward-line 13)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 6)))

(ert-deftest lean4-indent--newline-in-wrapped-variable-block-dedents-to-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "variable\n"
       "  [(coherentTopology CompHaus).WEqualsLocallyBijective A]\n"
       "  [HasSheafify (coherentTopology CompHaus) A]\n"
       "  [(coherentTopology CompHaus.{u}).HasSheafCompose (CategoryTheory.forget A)]\n"
       "  [Balanced (Sheaf (coherentTopology CompHaus) A)] in\n"
       "lemma epi_iff_locallySurjective_on_compHaus : True := by\n"
       "  trivial\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-single-line-variable-dedents-to-wrapped-sibling ()
  (lean4-test-with-indent-buffer
      (concat
       "variable [∀ X Y, FunLike (FA X Y) (CA X) (CA Y)] [ConcreteCategory.{v'} A FA]\n"
       "  [HasFunctorialSurjectiveInjectiveFactorization A]\n"
       "\n")
    (goto-char (point-min))
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 2)))

(ert-deftest lean4-indent--newline-after-bullet-exact-does-not-overindent-next-bullet ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  have pm : (p m).Dom := by\n"
       "    rcases H with ⟨n, h₁, h₂⟩\n"
       "    rcases lt_trichotomy m n with (h₃ | h₃ | h₃)\n"
       "    · exact h₂ _ h₃\n"
       "    · rw [h₃]\n")
    (goto-char (point-min))
    (forward-line 4)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 8)))

(ert-deftest lean4-indent--newline-after-operator-tail-keeps-wrapped-application-going ()
  (lean4-test-with-indent-buffer
      (concat
       "def isoLocallyConstantOfIsColimit\n"
       "    (hF : ∀ S : Profinite, IsColimit <| F.mapCocone S.asLimitCone.op) :\n"
       "    F ≅ (locallyConstantPresheaf (F.obj (toProfinite.op.obj ⟨of PUnit.{u + 1}⟩))) :=\n"
       "  (lanPresheafNatIso hF).symm ≪≫\n"
       "    lanPresheafExt (isoFinYoneda F ≪≫ (locallyConstantIsoFinYoneda F).symm) ≪≫\n"
       "      lanPresheafNatIso fun _ ↦ isColimitLocallyConstantPresheafDiagram _ _\n")
    (goto-char (point-min))
    (forward-line 4)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-refine-record-application-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma free_lightProfinite_internallyProjective_iff_tensor_condition' (P : LightProfinite.{u}) :\n"
       "    InternallyProjective ((free R).obj P.toCondensed) ↔\n"
       "      ∀ {A B : LightCondMod R} (e : A ⟶ B) [Epi e], (∀ (S : LightProfinite)\n"
       "        (g : (free R).obj ((S ⊗ P).toCondensed) ⟶ B), ∃ (S' : LightProfinite)\n"
       "          (π : S' ⟶ S) (_ : Function.Surjective π) (g' : (free R).obj (S' ⊗ P).toCondensed ⟶ A),\n"
       "            ((free R).map (lightProfiniteToLightCondSet.map (π ▷ P))) ≫ g = g' ≫ e) := by\n"
       "  rw [free_internallyProjective_iff_tensor_condition']\n"
       "  refine ⟨fun h A B e he S g ↦ ?_, fun h A B e he S g ↦ ?_⟩\n"
       "  · specialize h e S ((free R).map (μIso lightProfiniteToLightCondSet _ _).hom ≫ g)\n"
       "    obtain ⟨S', π, hπ, g', hh⟩ := h\n"
       "    refine ⟨S', π, hπ, (free R).map (μIso\n"
       "        lightProfiniteToLightCondSet _ _).inv ≫ g', ?_⟩\n")
    (goto-char (point-min))
    (forward-line 10)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-wrapped-variable-line-indents-assumptions ()
  (lean4-test-with-indent-buffer
      (concat
       "variable (P) (X : TopCat.{max u w})\n"
       "    [HasExplicitFiniteCoproducts.{0} P] [HasExplicitPullbacks P]\n")
    (goto-char (point-min))
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-havei-header-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "noncomputable def counit [HasExplicitFiniteCoproducts.{u} P] : haveI := CompHausLike.preregular hs\n"
       "    (sheafSections _ _).obj ⟨CompHausLike.of P PUnit.{u + 1}⟩ ⋙ functor.{u, w} P hs ⟶\n")
    (goto-char (point-min))
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-wrapped-top-level-binder-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "lemma incl_comap {S T : (CompHausLike P)ᵒᵖ}\n"
       "    (f : LocallyConstant S.unop (Y.obj (op (CompHausLike.of P PUnit.{u + 1}))))\n"
       "      (g : S ⟶ T) (a : Fiber (f.comap g.unop.hom)) :\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-wrapped-top-level-operator-line-keeps-going ()
  (lean4-test-with-indent-buffer
      (concat
       "noncomputable def counit [HasExplicitFiniteCoproducts.{u} P] : haveI := CompHausLike.preregular hs\n"
       "    (sheafSections _ _).obj ⟨CompHausLike.of P PUnit.{u + 1}⟩ ⋙ functor.{u, w} P hs ⟶\n"
       "        𝟭 (Sheaf (coherentTopology (CompHausLike.{u} P)) (Type (max u w))) where\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-tactic-chain-semicolon-indents-next-step ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · rcases h with (h | h) <;>\n"
       "      cases h\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-inline-tactic-chain-opens-next-step ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  cases o <;> simp only <;> rw [tr]\n"
       "    <;> simp only [id, foo]\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-rw-open-bracket-indents-entries ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  · rw [Part.mem_unique h\n"
       "        foo]\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bare-refine-can-start-deep-term ()
  (lean4-test-with-indent-buffer
      (concat
       "def trTape' : Tape Bool := by\n"
       "  refine\n"
       "      Tape.mk' foo bar\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-inside-branch-body-keeps-branch-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  cases h with\n"
       "  | branch =>\n"
       "      unfold writes at hw ⊢\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-standalone-at-target-line-dedents-to-proof-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  | write f q IH =>\n"
       "      simp only [Finset.mem_image, Finset.mem_union, Finset.mem_univ, true_and]\n"
       "        at hw ⊢\n"
       "      replace IH := IH hs fun q hq ↦ hw q (Or.inr hq)\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-top-level-attribute-line-keeps-wrapping ()
  (lean4-test-with-indent-buffer
      (concat
       "attribute [simp] stepAux.eq_1 stepAux.eq_2 stepAux.eq_3\n"
       "  stepAux.eq_4 stepAux.eq_5\n")
    (goto-char (point-min))
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-focus-intro-line-indents-branch-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  | branch =>\n"
       "    · intro _ _ _ IH₁ IH₂ ss' sub\n"
       "      unfold tr at sub\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-ordinary-tactic-in-branch-keeps-body-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  | branch =>\n"
       "    · intro _ hs\n"
       "      rw [foo] at h\n"
       "      rcases h with h\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-standalone-at-line-dedents-to-tactic-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  | branch =>\n"
       "      rw [foo] at h\n"
       "        at h\n"
       "      rcases h with h\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-inline-branch-body-keeps-branch-column ()
  (lean4-test-with-indent-buffer
      (concat
       "instance option : True := by\n"
       "    cases n with\n"
       "      | zero => rfl\n"
       "      | succ n =>\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-inline-semicolon-focus-keeps-proof-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "      cases h; · simp\n"
       "      cases h' <;> simp\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-paren-led-head-aligns-first-argument-column ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  ((casesOn' zero\n"
       "            ((casesOn' zero (Nat.Primrec.succ.comp left)).comp\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-double-paren-pipe-left-indents-deeply ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "    ((Nat.Primrec.casesOn' .zero <|\n"
       "              (Nat.Primrec.prec hf <|\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-bare-head-in-equation-branch-indents-first-arg ()
  (lean4-test-with-indent-buffer
      (concat
       "def contSupp : Cont' → Finset Λ'\n"
       "  | Cont'.cons₁ fs k =>\n"
       "    trStmts₁\n"
       "        (move₂ (fun _ => false) main aux <|)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--tab-on-blank-tactic-line-goes-to-tactic-column ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  trivial\n")
    (lean4-test--goto-eob)
    (lean4-test--open-line-below)
    (should (equal (lean4-test--line-string) ""))
    (lean4-test--tab-indent)
    (should (equal (lean4-test--line-string) "  "))
    (let ((last-command 'indent-for-tab-command))
      (lean4-test--tab-indent))
    (should (equal (lean4-test--line-string) ""))))

(lean4-define-final-line-indent-test
 lean4-indent--top-level-check-after-single-line-top-level-decl
 "example : Nat := 2

#check Nat.recOn")

(ert-deftest lean4-indent--dedents-after-double-indented-type ()
  (lean4-test-with-indent-buffer "example :\n"
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
  (should (lean4-indent--line-top-level-anchor-p "def foo := 1"))
  (should (lean4-indent--line-top-level-anchor-p "initialize_simps_projections Foo (bar → baz)"))
  (should (lean4-indent--line-top-level-anchor-p "mutual"))
  (should (lean4-indent--line-top-level-anchor-p "alias ⟨foo, _⟩ := bar"))
  (should (lean4-indent--line-top-level-anchor-p "noncomputable"))
  (should (lean4-indent--line-top-level-anchor-p "irreducible_def foo : True := by"))
  (should (lean4-indent--line-top-level-anchor-p "proof_wanted foo : True"))
  (should (lean4-indent--line-top-level-anchor-p "export OneMemClass (one_mem)"))
  (should (lean4-indent--line-top-level-anchor-p "include hf hg"))
  (should (lean4-indent--line-top-level-anchor-p "include S f in"))
  (should (lean4-indent--line-top-level-anchor-p "omit [Foo α] [Bar β] in"))
  (should (lean4-indent--line-top-level-anchor-p "unseal Foo.bar in"))
  (should (lean4-indent--line-top-level-anchor-p "suppress_compilation in"))
  (should (lean4-indent--line-top-level-anchor-p "unsuppress_compilation in"))
  (should (lean4-indent--line-top-level-anchor-p "macro_rules | `(foo) => `(bar)"))
  (should (lean4-indent--line-top-level-anchor-p "nonrec"))
  (should (lean4-indent--line-top-level-anchor-p "insert_to_additive_translation Foo Bar"))
  (should (lean4-indent--line-top-level-anchor-p "unif_hint foo (R R' : C) where"))
  (should (lean4-indent--line-top-level-anchor-p "library_note2 foo /--"))
  (should (lean4-indent--line-top-level-anchor-p "extend_docs Foo.bar after"))
  (should (lean4-indent--line-top-level-anchor-p "run_cmd Foo.bar do"))
  (should (lean4-indent--line-top-level-anchor-p "mk_iff_of_inductive_prop Foo.bar Foo.baz"))
  (should (lean4-indent--line-top-level-anchor-p "assert_not_exists Foo Bar"))
  (should (lean4-indent--line-top-level-anchor-p "elab_rules : tactic | `(tactic| foo) => do"))
  (should (lean4-indent--line-top-level-anchor-p "private"))
  (should (lean4-indent--line-top-level-anchor-p "local notation3 \"coeffs(\"p\")\" => Set.range (coeff p)"))
  (should (lean4-indent--line-top-level-anchor-p "local grind_pattern foo => True"))
  (should (lean4-indent--line-top-level-anchor-p "scoped infixl:50 \" ~> \" => Promises"))
  (should (lean4-indent--line-top-level-anchor-p "prefix:100 \"foo\" => bar"))
  (should (lean4-indent--line-top-level-anchor-p "local prefix:arg \"ι\" => Ordnode.singleton"))
  (should (lean4-indent--line-top-level-anchor-p
           "grind_pattern IsStrictlyPositive.spectrum_pos => x ∈ spectrum 𝕜 a, IsStrictlyPositive a")))

(ert-deftest lean4-indent--top-level-scoped-instance-is-declaration-head ()
  (should (lean4-indent--line-top-level-declaration-head-p "scoped instance foo : True := by"))
  (should (lean4-indent--line-top-level-anchor-p "scoped instance foo : True := by")))

(ert-deftest lean4-indent--top-level-private-local-instance-is-declaration-head ()
  (should (lean4-indent--line-top-level-declaration-head-p
           "private local instance (i : ι) : Decidable (succ i = i) := foo"))
  (should (lean4-indent--line-top-level-anchor-p
           "private local instance (i : ι) : Decidable (succ i = i) := foo")))

(ert-deftest lean4-indent--top-level-anchor-classification-is-case-sensitive ()
  (should-not (lean4-indent--line-top-level-anchor-p
               "Meta.withLocalInstances (← getLCtx).decls.toList.reduceOption do"))
  (should-not (lean4-indent--line-top-level-declaration-head-p
               "Meta.definitelyNotADeclaration")))

(ert-deftest lean4-indent--top-level-public-register-option-is-declaration-head ()
  (should (lean4-indent--line-top-level-declaration-head-p
           "public register_option linter.privateModule : Bool := {"))
  (should (lean4-indent--line-top-level-anchor-p
           "public register_option linter.privateModule : Bool := {")))

(ert-deftest lean4-indent--top-level-inline-attribute-declaration-is-head ()
  (should (lean4-indent--line-top-level-declaration-head-p
           "@[simp] lemma foo : True := by"))
  (should (lean4-indent--line-top-level-anchor-p
           "@[simp] lemma foo : True := by")))

(ert-deftest lean4-indent--top-level-nonrec-is-declaration-head ()
  (should (lean4-indent--line-top-level-declaration-head-p "nonrec theorem foo : True := by"))
  (should (lean4-indent--line-top-level-anchor-p "nonrec theorem foo : True := by")))

(ert-deftest lean4-indent--block-comment-recovery ()
  (lean4-test-with-indent-buffer
      (concat
       "def foo := by\n"
       "  /-\n"
       "  comment\n"
       "  -/\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("exact rfl" "  exact rfl")
       ("exact rfl" "  exact rfl")))))

(ert-deftest lean4-indent--mutual-indents-defs ()
  (lean4-test-with-indent-buffer
      (concat
       "mutual\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("def foo : Nat := by" "  def foo : Nat := by")
       ("def bar : Nat := by" "  def bar : Nat := by")))))

(ert-deftest lean4-indent--branch-does-not-align-to-unrelated-with ()
  (lean4-test-with-indent-buffer
      (concat
       "example : True := by\n"
       "  simp [with]\n")
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
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("trivial" "  trivial")
       ("trivial" "  trivial")))))

(ert-deftest lean4-indent--inductive-branches-align ()
  (lean4-test-with-indent-buffer
      (concat
       "inductive Foo where\n")
    (lean4-test--goto-eob)
    (lean4-test--insert-lines-and-assert
     '(("| bar" "  | bar")
       ("| baz" "  | baz")))))

(lean4-define-final-line-indent-test
 lean4-indent--iff-intro-lambda-line
 "example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
    (fun hnpq: _ =>")

(lean4-define-final-line-indent-test
 lean4-indent--de-morgan-constructor-line
 "example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
    (fun hnpq: _ =>
      ⟨(fun hp: p => (hnpq (Or.inl hp))),
       (fun hq: q => (hnpq (Or.inr hq)))⟩)")

(lean4-define-final-line-indent-test
 lean4-indent--iff-intro-second-lambda-line
 "example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
    (fun hnpq: _ =>
      ⟨(fun hp: p => (hnpq (Or.inl hp))),
       (fun hq: q => (hnpq (Or.inr hq)))⟩)
    (fun hnpnq: _ =>")

(lean4-define-final-line-indent-test
 lean4-indent--iff-intro-inner-hpq-line
 "    (fun hnpnq: _ =>
      have hnp := hnpnq.left
      have hnq := hnpnq.right
      (fun hpq: p ∨ q =>")

(lean4-define-final-line-indent-test
 lean4-indent--top-level-theorem-after-iff-intro-example
 "example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
    (fun hnpq: _ =>
      ⟨(fun hp: p => (hnpq (Or.inl hp))),
       (fun hq: q => (hnpq (Or.inr hq)))⟩)
    (fun hnpnq: _ =>
      have hnp := hnpnq.left
      have hnq := hnpnq.right
      (fun hpq: p ∨ q =>
        hpq.elim hnp hnq))

theorem mem_split {x : T} {l : List T} : x ∈ l → ∃ s t : List T, l = s ++ (x :: t) :=")

(lean4-define-final-line-indent-test
 lean4-indent--exact-smul-induction-sibling-fun-line
 "theorem smul_induction_on' {x : M} (hx : x ∈ I • N) {p : ∀ x, x ∈ I • N → Prop}
    (smul : ∀ (r : A) (hr : r ∈ I) (n : M) (hn : n ∈ N), p (r • n) (smul_mem_smul hr hn))
    (add : ∀ x hx y hy, p x hx → p y hy → p (x + y) (add_mem ‹_› ‹_›)) : p x hx := by
  refine Exists.elim ?_ fun (h : x ∈ I • N) (H : p x h) ↦ H
  exact smul_induction_on hx (fun a ha x hx ↦ ⟨_, smul _ ha _ hx⟩)
    fun x y ⟨_, hx⟩ ⟨_, hy⟩ ↦ ⟨_, add _ _ _ _ hx hy⟩")

(lean4-define-final-line-indent-test
 lean4-indent--list-rec-on-lambda-line
 "theorem mem_split {x : T} {l : List T} : x ∈ l → ∃ s t : List T, l = s ++ (x :: t) :=
  List.recOn l
    (fun H : x ∈ [] ↦ False.elim ((mem_nil_iff _).mp H))")

(lean4-define-final-line-indent-test
 lean4-indent--nested-fun-chain-line
 "    (fun y l ↦
      fun IH : x ∈ l → ∃ s t : List T, l = s ++ (x :: t) ↦
      fun H : x ∈ y :: l ↦")

(lean4-define-final-line-indent-test
 lean4-indent--nested-fun-chain-body-line
 "    (fun y l ↦
      fun IH : x ∈ l → ∃ s t : List T, l = s ++ (x :: t) ↦
      fun H : x ∈ y :: l ↦
      Or.elim (eq_or_mem_of_mem_cons H)")

(lean4-define-final-line-indent-test
 lean4-indent--orel-branch-sibling-lambda-line
 "      Or.elim (eq_or_mem_of_mem_cons H)
        (fun H1 : x = y ↦
          Exists.intro [] (Exists.intro l (by rw [H1]; rfl)))
        (fun H1 : x ∈ l ↦")

(lean4-define-final-line-indent-test
 lean4-indent--mem-split-orel-branch-sibling-lambda-line
 "theorem mem_split {x : T} {l : List T} : x ∈ l → ∃ s t : List T, l = s ++ (x :: t) :=
  List.recOn l
    (fun H : x ∈ [] ↦ False.elim ((mem_nil_iff _).mp H))
    (fun y l ↦
      fun IH : x ∈ l → ∃ s t : List T, l = s ++ (x :: t) ↦
      fun H : x ∈ y :: l ↦
      Or.elim (eq_or_mem_of_mem_cons H)
        (fun H1 : x = y ↦
          Exists.intro [] (Exists.intro l (by rw [H1]; rfl)))
        (fun H1 : x ∈ l ↦")

(ert-deftest lean4-indent--newline-after-qualified-atom-head-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  obtain ⟨n, h⟩ :=\n"
       "    Fintype.exists_ne_map_eq_of_card_lt\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-angle-literal-comma-line-indents-hanging-continuation ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem pumping_lemma_aux : True := by\n"
       "  refine\n"
       "    ⟨M.evalFrom s ((x.take m).take n), (x.take m).take n, (x.take m).drop n,\n"
       "                    x.drop m, ?_, ?_, ?_, by rfl, ?_⟩\n")
    (lean4-test--goto-eob)
    (forward-line -1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-angle-application-head-indents-first-arg ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem add_of : True := by\n"
       "  exact\n"
       "    ⟨disjoin_manyOneReducible\n"
       "        (manyOneReducible_toNat.trans OneOneReducible.disjoin_left.to_many_one)\n")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-inside-angle-application-block-keeps-sibling-depth ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem add_of : True := by\n"
       "  exact\n"
       "    ⟨disjoin_manyOneReducible\n"
       "        (manyOneReducible_toNat.trans OneOneReducible.disjoin_left.to_many_one)\n"
       "        (manyOneReducible_toNat.trans OneOneReducible.disjoin_right.to_many_one),\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-where-field-structure-body-stays-at-field-column ()
  (lean4-test-with-indent-buffer
      (concat
       "def reindex (g : σ ≃ σ') : DFA α σ ≃ DFA α σ' where\n"
       "  toFun M := {\n"
       "    step := fun s a => g (M.step (g.symm s) a)\n"
       "    start := g M.start\n"
       "    accept := g.symm ⁻¹' M.accept\n"
       "  }\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "  ")))

(ert-deftest lean4-indent--newline-after-dot-qualified-head-indents-following-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem part_iff₁ {f : ℕ →. ℕ} : True := by\n"
       "  part_iff.trans\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "    ")))

(ert-deftest lean4-indent--newline-after-local-coloneq-qualified-head-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem mem_periodicPts_iff_isPeriodicPt_factorial_card [Fintype α] :\n"
       "    x ∈ periodicPts f ↔ IsPeriodicPt f (card α)! x where\n"
       "  mp := isPeriodicPt_factorial_card_of_mem_periodicPts\n"
       "  mpr h := minimalPeriod_pos_iff_mem_periodicPts.mp\n"
       "    (IsPeriodicPt.minimalPeriod_pos (Nat.factorial_pos _) h)\n")
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 4)))

(ert-deftest lean4-indent--newline-after-classical-exact-indents-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  classical exact\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-projection-head-indents-args ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "  ((rfind <| f).bind\n"
       "    (g h)).comp₁\n")
    (lean4-test--goto-eob)
    (lean4-test--newline-and-assert "      ")))

(ert-deftest lean4-indent--newline-after-paren-projection-head-indents-deeper ()
  (lean4-test-with-indent-buffer
      (concat
       "theorem foo : True := by\n"
       "    (vector_get'.comp\n"
       "          (vector_ofFn h).of_eq\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (lean4-test--newline-lower-bound-and-assert)))

(ert-deftest lean4-indent--newline-after-paren-pipe-by-opens-proof-body ()
  (lean4-test-with-indent-buffer
      (concat
       "def prodEmbeddingDisjointEquivSigmaEmbeddingRestricted {α β γ : Type*} :\n"
       "    { f : (α ↪ γ) × (β ↪ γ) // Disjoint (Set.range f.1) (Set.range f.2) } ≃\n"
       "      Σ f : α ↪ γ, β ↪ ↥(Set.range f)ᶜ :=\n"
       "  (subtypeProdEquivSigmaSubtype fun (a : α ↪ γ) (b : β ↪ _) =>\n"
       "        Disjoint (Set.range a) (Set.range b)).trans <|\n"
       "    Equiv.sigmaCongrRight fun a =>\n"
       "      (subtypeEquivProp <| by\n"
       "            ext f\n")
    (goto-char (point-min))
    (forward-line 6)
    (end-of-line)
    (lean4-test--newline-next-line-bounds-and-assert 12)))

(provide 'lean4-indent-test)
;;; lean4-indent-test.el ends here
