# Indentation Fork Notes

This directory vendors a local fork base of `wvhulle/tree-sitter-lean`.

Why it is here:

- `lean4-indent-ts.el` needs a Lean tree-sitter grammar that is better suited to
  indentation work than the older experimental grammar.
- This vendored copy gives the indentation project a stable place to patch the
  grammar against indentation-specific regression cases.

Current status:

- Upstream base: `wvhulle/tree-sitter-lean`
- Upstream license: MIT
- This vendored copy currently preserves upstream licensing and authorship
  information; any local changes should continue to do so.

Intended scope of local grammar work:

- reliable top-level command/declaration structure
- declaration bodies and `where` declarations
- multiline applications
- `fun` bodies
- `match` alternatives
- rough tactic-block boundaries when useful for indentation

This fork is intended for indentation support, not for complete Lean parsing.
