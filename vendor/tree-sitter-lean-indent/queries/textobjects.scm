; textobjects.scm — Helix textobject queries for tree-sitter-lean
;
; Enables `maf` (select around function), `mif` (inside), `mat` (around type),
; `maa` (around argument), `mac` (around comment), etc.

; ── Functions / theorems ─────────────────────────────────────────────────────
; `definition` covers def / theorem / lemma / abbrev / instance

(definition body: (_) @function.inside) @function.around

(example body: (_) @function.inside) @function.around

; Lambdas (fun / λ)
(fun body: (_) @function.inside) @function.around

; Where declarations
(where_decl body: (_) @function.inside) @function.around

; ── Types (structures / inductives) ──────────────────────────────────────────

(structure
  (structure_field) @class.inside) @class.around

(inductive
  (constructor) @class.inside) @class.around

; ── Parameters / binders ─────────────────────────────────────────────────────
; Lean binders are space-separated, not comma-separated:
;   (x : Nat) (y : Nat) {z : Prop} [inst : Decidable p]

(explicit_binder
  name: (identifier) @parameter.inside) @parameter.around

(implicit_binder
  name: (identifier) @parameter.inside) @parameter.around

(instance_binder
  name: (identifier) @parameter.inside) @parameter.around

; ── Comments ─────────────────────────────────────────────────────────────────

(comment) @comment.inside

(comment)+ @comment.around

; ── Entries (items in a list-like context) ───────────────────────────────────
; Useful for navigating constructors, match arms, calc steps, tactic subgoals.

(match_arm
  body: (_) @entry.inside) @entry.around

(constructor) @entry.around

(structure_field
  type: (_) @entry.inside) @entry.around

(calc_step) @entry.around

; Focused tactic subgoals `· tactic` and case splits `case h => tactic`
(tactic_focus) @entry.around

(tactic_case) @entry.around
