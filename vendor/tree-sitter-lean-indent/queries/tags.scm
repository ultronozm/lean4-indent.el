; tags.scm — Helix workspace-symbol / ctags queries for tree-sitter-lean
;
; Drives `Space-s` (document symbols) and `Space-S` (workspace symbols).

; ── Top-level declarations ────────────────────────────────────────────────────

(definition
  name: (identifier) @name) @definition.function

(example) @definition.function

; ── Constants & axioms ──────────────────────────────────────────────────────

(constant
  name: (identifier) @name) @definition.constant

(axiom
  name: (identifier) @name) @definition.constant

; ── Type declarations ─────────────────────────────────────────────────────────

(structure
  name: (identifier) @name) @definition.type

(inductive
  name: (identifier) @name) @definition.type

; Constructors are definition-like within inductives
(constructor
  name: (identifier) @name) @definition.struct

; Structure fields
(structure_field
  name: (identifier) @name) @definition.field

; ── Namespaces / sections ────────────────────────────────────────────────────

(namespace
  name: (identifier) @name) @definition.module

(section
  name: (identifier) @name) @definition.module
