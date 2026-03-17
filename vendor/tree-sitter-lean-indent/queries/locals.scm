; locals.scm — Helix scope-aware highlighting for tree-sitter-lean

; ── Scopes ─────────────────────────────────────────────────────────────────────

[
  (module)
  (namespace)
  (section)
] @local.scope

[
  (definition)
  (structure)
  (inductive)
  (fun)
  (quantifier)
  (let)
  (have)
  (do)
  (match)
  (by)
  (for_in)
  (example)
  (where_decl)
] @local.scope

; ── Definitions ────────────────────────────────────────────────────────────────

; Binder parameters
(explicit_binder
  name: (identifier) @local.definition)

(implicit_binder
  name: (identifier) @local.definition)

(instance_binder
  name: (identifier) @local.definition)

; Fun / forall / exists bare binders
(fun
  binders: (identifier) @local.definition)

(quantifier
  binders: (identifier) @local.definition)

; Let / have bindings
(let
  pattern: (identifier) @local.definition)

(have
  name: (identifier) @local.definition)

(do_let
  pattern: (identifier) @local.definition)

(let_mut
  name: (identifier) @local.definition)

(let_bind
  name: (identifier) @local.definition)

; For-in loop variable
(for_in
  var: (identifier) @local.definition)

; Where declarations
(where_decl
  name: (identifier) @local.definition)

; Tactic let / have
(tactic_let
  pattern: (identifier) @local.definition)

(tactic_have
  name: (identifier) @local.definition)

; ── References ─────────────────────────────────────────────────────────────────

(identifier) @local.reference
