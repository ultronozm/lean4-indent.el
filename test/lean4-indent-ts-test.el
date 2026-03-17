;;; lean4-indent-ts-test.el --- Tests for experimental tree-sitter Lean indentation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'lean4-syntax)
(require 'lean4-indent-ts)

(defmacro lean4-ts-test-with-indent-buffer (contents &rest body)
  "Evaluate BODY in a temporary Lean tree-sitter indentation test buffer."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (set-syntax-table lean4-syntax-table)
     (let ((treesit-extra-load-path (lean4-indent-ts--extra-load-path)))
       (treesit-parser-create 'lean))
     (setq-local indent-tabs-mode nil)
     (setq-local lean4-indent-offset 2)
     (setq-local indent-line-function #'lean4-indent-ts-line-function)
     ,@body))

(defun lean4-ts-test--line-string ()
  "Return current line contents."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lean4-ts-test--goto-eob ()
  "Move to the end of the last nonempty line."
  (goto-char (point-max))
  (when (and (bolp) (not (bobp)))
    (forward-line -1))
  (end-of-line))

(defun lean4-ts-test--reindent-final-line-and-assert-same ()
  "Reindent the final line and assert it remains unchanged."
  (lean4-ts-test--goto-eob)
  (let ((before (lean4-ts-test--line-string)))
    (funcall #'lean4-indent-ts-line-function)
    (should (equal (lean4-ts-test--line-string) before))))

(defun lean4-ts-test--goto-line (n)
  "Move point to line N (1-based)."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun lean4-ts-test--ancestor-types-at-line (n)
  "Return ancestor node types at line N, innermost first."
  (lean4-ts-test--goto-line n)
  (back-to-indentation)
  (let ((node (lean4-indent-ts--current-node))
        types)
    (while node
      (push (treesit-node-type node) types)
      (setq node (treesit-node-parent node)))
    (nreverse types)))

(ert-deftest lean4-indent-ts--available ()
  (should (lean4-indent-ts--available-p)))

(ert-deftest lean4-indent-ts--top-level-variable-after-section ()
  (lean4-ts-test-with-indent-buffer
      "section Pointwise

variable {R : Type*} {A : Type*} [CommSemiring R] [Semiring A] [Algebra R A]"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--wrapped-top-level-variable-line ()
  (lean4-ts-test-with-indent-buffer
      "variable {ι R : Type*} {S : ι → Type*} [CommSemiring R] [∀ i, Semiring (S i)] [∀ i, Algebra R (S i)]
  {s : Set ι} {t t₁ t₂ : ∀ i, Subalgebra R (S i)} {x : ∀ i, S i}"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--scoped-attribute-top-level-line ()
  (lean4-ts-test-with-indent-buffer
      "protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where
  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)

scoped[Pointwise] attribute [instance] Subalgebra.pointwiseMulAction"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--wrapped-theorem-body ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo (x : Nat) :
    Nat :=
  x"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--where-field ()
  (lean4-ts-test-with-indent-buffer
      "protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where
  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--multiline-application-arguments ()
  (lean4-ts-test-with-indent-buffer
      "example : Nat :=
  Nat.recOn n
    h0
    fun n ih => ih"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--paren-led-sibling-application-arguments ()
  (lean4-ts-test-with-indent-buffer
      "example : Nat :=
  Or.elim h
    (fun h1 => t1)
    (fun h2 => t2)"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--fun-body ()
  (lean4-ts-test-with-indent-buffer
      "example : Nat :=
  fun x =>
    x"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--declaration-have-value-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P :=
  have h : Q :=
    t
  u"
    (lean4-ts-test--goto-line 3)
    (let ((before (lean4-ts-test--line-string)))
      (funcall #'lean4-indent-ts-line-function)
      (should (equal (lean4-ts-test--line-string) before)))))

(ert-deftest lean4-indent-ts--declaration-show-body-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P :=
  show Q from
    t"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--declaration-suffices-body-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P :=
  suffices h : Q from
    t"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--match-alt-line ()
  (lean4-ts-test-with-indent-buffer
      "example : Nat :=
  match x with
  | 0 => 0"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--multiline-tactic-application-arguments ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P := by
  exact f
    (fun a => t)
    (fun b => u)"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--tactic-focus-body-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P := by
  ·
    exact t"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--tactic-case-body-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P := by
  case h =>
    exact t"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--calc-step-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : x = z := by
  calc
    x = y := by rfl"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--tactic-have-value-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P := by
  have h : Q :=
    f x"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--tactic-rewrite-config-line ()
  (lean4-ts-test-with-indent-buffer
      "theorem foo : P := by
  rw [a,
    b]"
    (lean4-ts-test--reindent-final-line-and-assert-same)))

(ert-deftest lean4-indent-ts--adjoin-ext-parse-regression ()
  (lean4-ts-test-with-indent-buffer
      "theorem adjoin_ext {s : Set A} ⦃φ₁ φ₂ : adjoin R s →ₐ[R] B⦄
    (h : ∀ x hx, φ₁ ⟨x, subset_adjoin hx⟩ = φ₂ ⟨x, subset_adjoin hx⟩) : φ₁ = φ₂ :=
  ext fun ⟨x, hx⟩ ↦ adjoin_induction h (fun _ ↦ φ₂.commutes _ ▸ φ₁.commutes _)
    (fun _ _ _ _ h₁ h₂ ↦ by convert congr_arg₂ (· + ·) h₁ h₂ <;> rw [← map_add] <;> rfl)
    (fun _ _ _ _ h₁ h₂ ↦ by convert congr_arg₂ (· * ·) h₁ h₂ <;> rw [← map_mul] <;> rfl) hx"
    (should (equal (treesit-node-type (treesit-buffer-root-node 'lean)) "module"))
    (should (member "application" (lean4-ts-test--ancestor-types-at-line 5)))))

(ert-deftest lean4-indent-ts--pointwise-where-body-parse-regression ()
  (lean4-ts-test-with-indent-buffer
      "protected def pointwiseMulAction : MulAction R' (Subalgebra R A) where
  smul a S := S.map (MulSemiringAction.toAlgHom _ _ a)
  one_smul S := (congr_arg (fun f => S.map f) (AlgHom.ext <| one_smul R')).trans S.map_id
  mul_smul _a₁ _a₂ S :=
    (congr_arg (fun f => S.map f) (AlgHom.ext <| mul_smul _ _)).trans (S.map_map _ _).symm"
    (should (equal (treesit-node-type (treesit-buffer-root-node 'lean)) "module"))
    (should (member "where_decl" (lean4-ts-test--ancestor-types-at-line 5)))
    (should (member "definition" (lean4-ts-test--ancestor-types-at-line 5)))))

(ert-deftest lean4-indent-ts--smul-induction-parse-regression ()
  (lean4-ts-test-with-indent-buffer
      "theorem smul_induction_on' {x : M} (hx : x ∈ I • N) {p : ∀ x, x ∈ I • N → Prop}
    (smul : ∀ (r : A) (hr : r ∈ I) (n : M) (hn : n ∈ N), p (r • n) (smul_mem_smul hr hn))
    (add : ∀ x hx y hy, p x hx → p y hy → p (x + y) (add_mem ‹_› ‹_›)) : p x hx := by
  refine Exists.elim ?_ fun (h : x ∈ I • N) (H : p x h) ↦ H
  exact smul_induction_on hx (fun a ha x hx ↦ ⟨_, smul _ ha _ hx⟩)
    fun x y ⟨_, hx⟩ ⟨_, hy⟩ ↦ ⟨_, add _ _ _ _ hx hy⟩"
    (should (equal (treesit-node-type (treesit-buffer-root-node 'lean)) "module"))
    (should (member "application" (lean4-ts-test--ancestor-types-at-line 6)))
    (should (member "by" (lean4-ts-test--ancestor-types-at-line 6)))))

(ert-deftest lean4-indent-ts--rank-sup-proof-parse-regression ()
  (lean4-ts-test-with-indent-buffer
      "theorem rank_sup_eq_rank_left_mul_rank_of_free :
    Module.rank R ↥(A ⊔ B) = Module.rank R A * Module.rank A (Algebra.adjoin A (B : Set S)) := by
  rcases subsingleton_or_nontrivial R with _ | _
  · haveI := Module.subsingleton R S; simp
  nontriviality S using rank_subsingleton'
  letI : Algebra A (Algebra.adjoin A (B : Set S)) := Subalgebra.algebra _
  letI : SMul A (Algebra.adjoin A (B : Set S)) := Algebra.toSMul
  haveI : IsScalarTower R A (Algebra.adjoin A (B : Set S)) :=
    IsScalarTower.of_algebraMap_eq (congrFun rfl)
  rw [rank_mul_rank R A (Algebra.adjoin A (B : Set S))]
  change _ = Module.rank R ((Algebra.adjoin A (B : Set S)).restrictScalars R)
  rw [Algebra.restrictScalars_adjoin]; rfl"
    (should (equal (treesit-node-type (treesit-buffer-root-node 'lean)) "module"))
    (should (member "tactic_apply" (lean4-ts-test--ancestor-types-at-line 11)))
    (should (member "module" (lean4-ts-test--ancestor-types-at-line 11)))))

(provide 'lean4-indent-ts-test)
;;; lean4-indent-ts-test.el ends here
