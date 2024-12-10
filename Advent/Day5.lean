import Std.Data.HashMap

namespace Day5

open Std (HashMap)

abbrev Rule := Nat × Nat
abbrev Manual := List Nat
abbrev RuleSet := HashMap Nat (Array Nat)

instance : ToString Ordering where
  toString := fun
    | Ordering.lt => "lt"
    | Ordering.eq => "eq"
    | Ordering.gt => "gt"

def testString := "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

def parseLines (lines : Array String) : Array Rule × Array Manual :=
  let endOfRules := lines.findIdx? (· == "")  |>.get!

  let rules := lines[:endOfRules].toArray.map
    fun line =>
      let parts := line.splitOn "|"
      (parts[0]!.toNat!, parts[1]!.toNat!)

  let manuals := lines[endOfRules+1:].toArray.map fun x => (x.splitOn "," |> List.map String.toNat!)
  (rules, manuals)

def makeRuleSet (rules : Array Rule) : RuleSet := Id.run do
  let mut ruleset := HashMap.empty
  for (pred, succ) in rules do
    match ruleset[succ]? with
    | some arr => ruleset := ruleset.insert succ (arr.push pred)
    | none => ruleset := ruleset.insert succ #[pred]
  return ruleset

def testManual (rules : RuleSet) (manual : Manual) : Bool := Id.run do
  for idx1 in [:manual.length] do
    for idx2 in [idx1:manual.length] do
      let pred := manual[idx1]!
      let succ := manual[idx2]!
      if let some arr := rules[pred]? then
        if succ ∈ arr then
          return false
        else
          continue
      else
        continue
  return true

def getInput : IO (Array Rule × Array Manual) := do
  let lines ← IO.FS.lines "./inputs/day5.txt"
  return parseLines lines

def test : IO Bool := do
  let (rules, manuals) := (parseLines <| (testString.splitOn "\n").toArray)
  let ruleset := makeRuleSet rules
  for manual in manuals do
    IO.println (testManual ruleset manual)
  return true

def getCenter (manual : Manual) : Nat :=
  manual[manual.length / 2]!

def part1 : IO String := do
  let (rules, manuals) ← getInput
  let ruleset := makeRuleSet rules
  return manuals |>.filter (testManual ruleset ·)
          |>.map getCenter
          |>.foldl (· + ·) 0
          |> ToString.toString

def order (rules : RuleSet) (n m : Nat) : Ordering :=
  if n == m then .eq else
    let nPreds? := rules[n]?
    let mPreds? := rules[m]?
    if let some nPreds := nPreds? then
      if m ∈ nPreds then .gt else
      if let some mPreds := mPreds? then
        if n ∈ mPreds then .lt else .eq
      else .eq
    else if let some mPreds := mPreds? then
      if n ∈ mPreds then .lt else .eq
    else
      .eq

def reOrder (rules : RuleSet) (manual : Manual) : Manual := Id.run do
  let mut orderedManual := #[]
  for n in manual do
    let mut added := false
    for (m, idx) in orderedManual.zipWithIndex do
      match order rules n m with
        | .eq | .lt => continue
        | .gt =>
          orderedManual := orderedManual.insertAt! idx n
          added := true
          break
    if not added then
      orderedManual := orderedManual.push n
  return orderedManual.toList.reverse

def testtest : IO Bool := do
  let (rules, manuals) := (parseLines <| (testString.splitOn "\n").toArray)
  let ruleset := makeRuleSet rules
  for manual in manuals do
    IO.println s!"{manual} : {(reOrder ruleset manual)}"
  return true

def part2 : IO String := do
  let (rules, manuals) ← getInput
  let ruleset := makeRuleSet rules
  let incorrectlyOrdered := manuals.filter fun x => not (testManual ruleset x)
  return incorrectlyOrdered |>.map (reOrder ruleset ·)
                     |>.map getCenter
                     |>.foldl (· + ·) 0
                     |> ToString.toString

