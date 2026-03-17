;;; lean4-indent-newline-test.el --- Shared newline indentation tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'lean4-syntax)
(require 'lean4-indent)
(require 'lean4-indent-ts)

(defconst lean4-indent-newline-test--cases
  '((:name top-level-proof-body
     :contents "example : True := by\n  trivial\n"
     :line 2
     :expected "  ")
    (:name top-level-check
     :contents "#check 37\n"
     :line 1
     :expected "")
    (:name section-flush-left
     :contents "section Pointwise\n"
     :line 1
     :expected "")
    (:name single-line-top-level-decl
     :contents "example : Nat := 2\n"
     :line 1
     :expected "")
    (:name tactic-proof-before-final-line
     :contents
     "theorem sb_right_inv {x : α} (hx : x ∉ sbSet f g) : g (invFun g x) = x := by\n  have : x ∈ g '' univ := by\n    contrapose! hx\n    rw [sbSet, mem_iUnion]\n    use 0\n    rw [sbAux, mem_diff]\n    constructor\n    · exact trivial\n    · assumption\n  have : ∃ y, g y = x := by\n    contrapose! hx\n    rw [sbSet, mem_iUnion]\n    use 0\n    rw [sbAux, mem_diff]\n    constructor\n    · exact trivial\n    simpa\n  rcases this with ⟨y, hy⟩\n  apply invFun_eq\n"
     :line 18
     :expected "  ")
    (:name tactic-proof-final-line
     :contents
     "theorem sb_right_inv {x : α} (hx : x ∉ sbSet f g) : g (invFun g x) = x := by\n  have : x ∈ g '' univ := by\n    contrapose! hx\n    rw [sbSet, mem_iUnion]\n    use 0\n    rw [sbAux, mem_diff]\n    constructor\n    · exact trivial\n    · assumption\n  have : ∃ y, g y = x := by\n    contrapose! hx\n    rw [sbSet, mem_iUnion]\n    use 0\n    rw [sbAux, mem_diff]\n    constructor\n    · exact trivial\n    simpa\n  rcases this with ⟨y, hy⟩\n  apply invFun_eq\n"
     :line 19
     :expected "  ")
    (:name ext-fun-theorem-body
     :contents
     "theorem adjoin_ext {s : Set A} ⦃φ₁ φ₂ : adjoin R s →ₐ[R] B⦄\n    (h : ∀ x hx, φ₁ ⟨x, subset_adjoin hx⟩ = φ₂ ⟨x, subset_adjoin hx⟩) : φ₁ = φ₂ :=\n  ext fun ⟨x, hx⟩ ↦ adjoin_induction h (fun _ ↦ φ₂.commutes _ ▸ φ₁.commutes _)\n    (fun _ _ _ _ h₁ h₂ ↦ by convert congr_arg₂ (· + ·) h₁ h₂ <;> rw [← map_add] <;> rfl)\n    (fun _ _ _ _ h₁ h₂ ↦ by convert congr_arg₂ (· * ·) h₁ h₂ <;> rw [← map_mul] <;> rfl) hx\n"
     :line 3
     :expected "  "))
  "Shared newline-and-indent cases for both Lean indenters.")

(defun lean4-indent-newline-test--with-buffer (contents indent-fn use-ts body-fn)
  "Create a temporary Lean buffer with CONTENTS and run BODY-FN.

INDENT-FN is installed as `indent-line-function'.  When USE-TS is non-nil,
create a Lean tree-sitter parser first."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (set-syntax-table lean4-syntax-table)
    (setq-local indent-tabs-mode nil)
    (setq-local lean4-indent-offset 2)
    (setq-local indent-line-function indent-fn)
    (when use-ts
      (let ((treesit-extra-load-path (lean4-indent-ts--extra-load-path)))
        (treesit-parser-create 'lean))
      (setq-local lean4-indent-ts--buffer-ready t))
    (funcall body-fn)))

(defun lean4-indent-newline-test--run-case (case indent-fn use-ts)
  "Run shared newline CASE with INDENT-FN.

USE-TS selects whether to create a tree-sitter parser."
  (ert-info ((symbol-name (plist-get case :name)))
    (lean4-indent-newline-test--with-buffer
     (plist-get case :contents)
     indent-fn
     use-ts
     (lambda ()
       (goto-char (point-min))
       (forward-line (1- (plist-get case :line)))
       (end-of-line)
       (call-interactively #'newline-and-indent)
       (should (equal (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                      (plist-get case :expected)))))))

(ert-deftest lean4-indent-newline--heuristic-suite ()
  (dolist (case lean4-indent-newline-test--cases)
    (lean4-indent-newline-test--run-case
     case #'lean4-indent-line-function nil)))

(ert-deftest lean4-indent-newline--tree-sitter-suite ()
  (dolist (case lean4-indent-newline-test--cases)
    (lean4-indent-newline-test--run-case
     case #'lean4-indent-ts-line-function t)))

(ert-deftest lean4-indent-newline--case-count ()
  (should (= (length lean4-indent-newline-test--cases) 7)))

(provide 'lean4-indent-newline-test)
;;; lean4-indent-newline-test.el ends here
