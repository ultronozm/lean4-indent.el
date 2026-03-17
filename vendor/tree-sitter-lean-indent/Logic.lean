def factorial : Nat → Nat
  | 0 => 1
  | n+1 =>
    let r := factorial n
    (n + 1) * r
notation:10000 n "!" => factorial n

/-- The factorial is always positive -/
theorem factorial_pos : ∀ n, 0 < n ! := by
  intro n; induction n <;> grind [factorial]
