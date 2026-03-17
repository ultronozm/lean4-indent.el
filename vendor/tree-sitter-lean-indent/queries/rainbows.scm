; rainbows.scm — Helix rainbow-bracket queries for tree-sitter-lean

[
  ; ( )
  (parenthesized)
  (tuple)
  (explicit_binder)

  ; { }
  (structure_instance)
  (implicit_binder)

  ; [ ]
  (array)
  (list)
  (tactic_config)
  (instance_binder)
  (subscript)

  ; ⟨ ⟩
  (anonymous_constructor)
] @rainbow.scope

[
  "(" ")"
  "[" "]"
  "{" "}"
  "⟨" "⟩"
] @rainbow.bracket
