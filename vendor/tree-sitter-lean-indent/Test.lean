import Lean
import LeanPrism.Protocol
import LeanPrism.SemanticTableau.InfoTreeParser
import LeanPrism.SemanticTableau.DiffComputation

open Lean Elab Server

namespace LeanPrism.SemanticTableau

open LeanPrism.SemanticTableau.InfoTreeParser
open LeanPrism.SemanticTableau (TacticProofState.diff)
open LeanPrism (getBlockName getBlockKind)
open scoped LeanPrism

def hasTacticInfoAtPosition (tree : InfoTree) (fileMap : FileMap) (position : Lsp.Position) : Bool :=
  let hoverPos := fileMap.lspPosToUtf8Pos position
  !(goalsAt? tree fileMap hoverPos).isEmpty

/-- Array of (range, diagnostic info) pairs from diagnosticsRef or MessageLog. -/
abbrev DiagnosticsArray := Array (Lsp.Range × LeanPrism.DiagnosticInfo)

/-- Extract diagnostics that overlap with a given source range. -/
def extractDiagnosticsForRange (diagnostics : DiagnosticsArray) (range : Lsp.Range)
    : Array LeanPrism.DiagnosticInfo :=
  let isZeroWidth := range.start == range.«end»
  diagnostics.filterMap fun (diagRange, info) =>
    let startsInRange := diagRange.start >= range.start
    let endsInRange := if isZeroWidth
                       then diagRange.start <= range.«end»
                       else diagRange.start < range.«end»
    if startsInRange && endsInRange then
      some info
    else
      none

/-- A single node in the proof DAG: one tactic application producing one goal -/
structure ExpandedStep where
  tacticString : String
  position : Lsp.Range
  goalBefore : ParsedGoal
  goalAfter : Option ParsedGoal
  hypothesisDependencies : List String
  deriving Inhabited

/-- Expand steps so each resulting goal gets its own node -/
def expandSteps (steps : List ParsedStep) : List ExpandedStep :=
  steps.flatMap fun step =>
    if step.goalsAfter.isEmpty then
      -- Goal completed
      [{ tacticString := step.tacticString
         position := step.position
         goalBefore := step.goalBefore
         goalAfter := none
         hypothesisDependencies := step.hypothesisDependencies }]
    else
      -- One node per resulting goal
      step.goalsAfter.map fun goalAfter =>
        { tacticString := step.tacticString
          position := step.position
          goalBefore := step.goalBefore
          goalAfter := some goalAfter
          hypothesisDependencies := step.hypothesisDependencies }

def GenericDag.buildProof (steps : List ParsedStep) (cursorPos : Lsp.Position)
    (blockName : Option String := none)
    (blockKind : Option String := none)
    (diagnostics : DiagnosticsArray := #[]) : GenericDag :=
  if steps.isEmpty then { displayStyle := .proof, nodes := #[], metadata := Json.mkObj [] } else

  let expandedSteps := expandSteps steps
  let stepsArray := expandedSteps.toArray

  let (_, parentOf) : Std.HashMap MVarId Nat × Array (Option Nat) := Id.run do
    let mut goalToProducer : Std.HashMap MVarId Nat := {}
    let mut parents : Array (Option Nat) := #[]
    for h : idx in [:stepsArray.size] do
      let step := stepsArray[idx]
      let parent := goalToProducer.get? step.goalBefore.mvarId
        |>.filter (· != idx)
      parents := parents.push parent
      if let some goalAfter := step.goalAfter then
        goalToProducer := goalToProducer.insert goalAfter.mvarId idx
    return (goalToProducer, parents)

  let parentOf := parentOf

  -- Build children lists
  let childrenOf : Array (List Nat) := Id.run do
    let mut result : Array (List Nat) := stepsArray.map (fun _ => [])
    for h : childIdx in [:parentOf.size] do
      if let some parentIdx := parentOf[childIdx] then
        if parentIdx < result.size then
          result := result.modify parentIdx (childIdx :: ·)
    return result

  -- Calculate depths
  let depths : Array Nat := Id.run do
    let mut result := stepsArray.map (fun _ => 0)
    for h : idx in [:stepsArray.size] do
      let mut depth := 0
      let mut current := idx
      let mut visited : Std.HashSet Nat := {}
      while true do
        if visited.contains current then break
        visited := visited.insert current
        match parentOf[current]? with
        | some (some p) =>
          depth := depth + 1
          current := p
        | _ => break
      result := result.set! idx depth
    return result

  let nodes : Array GraphNode := stepsArray.mapIdx fun idx step =>
    let hypsBefore := step.goalBefore.hypotheses.filter (·.name.isDisplayableName)
    let hypsAfter := match step.goalAfter with
      | some g => g.hypotheses.filter (·.name.isDisplayableName)
      | none => hypsBefore

    let hypIdsBefore : Std.HashSet String := Std.HashSet.ofArray (hypsBefore.map (·.id))
    let newHypothesisIndices := Id.run do
      let mut result : Array Nat := #[]
      for h : i in [:hypsAfter.size] do
        let hyp := hypsAfter[i]!
        if !hypIdsBefore.contains hyp.id then
          result := result.push i
      return result

    let goalsAfter := match step.goalAfter with
      | some g => #[g.toGoalState]
      | none => #[]

    let rawStateBefore : TacticProofState := { goals := #[step.goalBefore.toGoalState], hypotheses := hypsBefore }
    let rawStateAfter : TacticProofState := { goals := goalsAfter, hypotheses := hypsAfter }
    let proofState := TacticProofState.diff rawStateBefore rawStateAfter

    let childIds := (childrenOf[idx]?.getD []).toArray
    let edges := childIds.map fun childId =>
      { target := childId, label := none, kind := none, attributes := Json.mkObj [] : GraphEdge }

    let nodeDiagnostics := extractDiagnosticsForRange diagnostics step.position
    let hasError := nodeDiagnostics.any fun d => d.severity == .error
    let isComplete := step.goalAfter.isNone && !hasError

    let metadata := Json.mkObj [
      ("proof_state", toJson proofState),
      ("is_complete", toJson isComplete),
      ("new_hypothesis_indices", Json.arr (newHypothesisIndices.map fun i => toJson i)),
      ("hypothesis_dependencies", Json.arr (step.hypothesisDependencies.toArray.map fun s => Json.str s)),
    ]

    {
      id := idx,
      content := .plain step.tacticString,
      kind := some "tactic",
      position := step.position.start,
      edges := edges,
      parent := parentOf[idx]?.join,
      depth := depths[idx]?.getD 0,
      metadata := metadata,
      diagnostics := nodeDiagnostics : GraphNode }

  let rootCandidates := nodes.toList.filterMap fun n =>
    if n.parent.isNone then some n.id else none
  let (root, orphans) := match rootCandidates with
    | [] => (none, #[])
    | r :: rest => (some r, rest.toArray)

  let currentNodeId : Option Nat := Id.run do
    let mut best : Option Nat := none
    let mut bestPos : Lsp.Position := ⟨0, 0⟩
    for h : i in [:nodes.size] do
      let node := nodes[i]
      let pos := node.position
      if pos.line < cursorPos.line || (pos.line == cursorPos.line && pos.character <= cursorPos.character) then
        if best.isNone || pos.line > bestPos.line || (pos.line == bestPos.line && pos.character > bestPos.character) then
          best := some node.id
          bestPos := pos
    return best

  let initialGoalType := stepsArray[0]?.map (·.goalBefore.type)
  let initialHypotheses : Array LocalBinding := stepsArray[0]?.map (fun step =>
    step.goalBefore.hypotheses.filterMap fun h =>
      if h.name.isEmpty || h.isProofTerm then none
      else some {
        name := h.name
        type := h.type
        value := h.value
        id := h.id
        bindingKind := .funParam
        isImplicit := h.isImplicit
        isInstance := h.isInstance
        navigationLocations := h.navigationLocations
      }) |>.getD #[]

  {
    displayStyle := .proof,
    nodes := nodes,
    rootNodeId := root,
    orphans := orphans,
    currentNodeId := currentNodeId,
    blockName := blockName,
    blockKind := blockKind,
    blockOutput := initialGoalType,
    blockInput := initialHypotheses,
    metadata := Json.mkObj [] }
end LeanPrism.SemanticTableau
