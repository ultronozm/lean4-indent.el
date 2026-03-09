# Thoughts on simplifying `lean4-indent`

The current engine mostly works by many local rules.  A better shape would be
to derive indentation from a few structural principles and only use heuristics
where Lean surface syntax really leaves a choice open.

## Structural principles that seem real

1. Top-level declarations and commands are flush-left.

2. After `:=`, `=>`, `↦`, `then`, `else`, `where`, `by`, `calc`, etc., Lean
   expects a single term body.  So indentation should be driven by "what is the
   next piece of that one term?" rather than by ad hoc keyword checks.

3. If the previous line closed a parenthesized argument group, then the next
   parenthesized line should first try to align as a sibling argument of the
   same enclosing application.

4. Lambda chains have a stable body column:

   ```lean
   fun x =>
     fun y =>
     body
   ```

   The second `fun` is not a sibling argument; it is the body of the first
   lambda.

5. Many mathlib-style multiline applications are really just:

   - application head line
   - one argument per following indented line

   Example:

   ```lean
   List.recOn l
     arg1
     arg2
   ```

## A promising simplification

Instead of dozens of overlapping rules, `compute-indent` could be organized
around these questions:

1. What enclosing construct are we in?
   - top-level declaration
   - proof/tactic block
   - term body introduced by `:=`, `↦`, `=>`, `by`, `calc`, etc.
   - inside a delimited group `(...)`, `{...}`, `[...]`, `⟨...⟩`

2. What kind of term was on the previous line?
   - atom / simple identifier
   - parenthesized or bracketed group
   - bare application head
   - keyword-led term (`fun`, `by`, `if`, `match`, `let`, `show`, `calc`, ...)
   - closing line of a delimited group

3. Is the current line
   - another sibling argument,
   - the body of the previous term-introducer,
   - or a dedent back to an outer anchor?

That would still be heuristic in places, but much less ad hoc.

## Revised hypothesis

For tactic-free term code, a fully correct implementation may actually be
possible, provided we follow Lean's real layout/parser behavior rather than
trying to infer structure from ad hoc text patterns.

In particular, many cases that first looked heuristic may really be structural:

- after `:=`, Lean expects one term;
- a multiline application presents one argument per indented line;
- after a parenthesized argument closes, the next parenthesized line is often a
  sibling argument of the same application;
- lambda chains keep a stable body column.

So the real issue may not be "mathlib style is too vague", but rather that the
current Emacs implementation is only approximating a parser.

## Where heuristics may still be unavoidable

Even if complete tactic-free code could be indented exactly, live editing still
has incomplete states:

```lean
Or.elim h
  (fun h1 ↦
```

At that point the term is unfinished, but the editor still needs to decide how
to indent the next line.  So a robust package may still need:

- an exact path for complete / parseable syntax,
- a fallback heuristic path for incomplete code while typing.

This suggests the best long-term shape may be:

1. parse and indent structurally whenever possible;
2. use a much smaller heuristic layer only for unfinished syntax.

## Concrete next step

Try a refactor that replaces much of `compute-indent` with:

- context discovery,
- previous-line term classification,
- a small number of rule families:
  - top-level / declaration
  - term body
  - lambda body
  - sibling argument after closed group
  - operator continuation
  - fallback

This should make the engine easier to reason about and expose the genuinely
hard cases more clearly.
