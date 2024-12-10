abbrev Report := List Nat

namespace Day2

def getInput : IO (Array Report) := do
  let lines ← IO.FS.lines "./inputs/day2.txt"
  return lines.map fun line =>
    (line.splitOn |>.map String.toNat!)

def isSafeAux (increasing : Bool) : List Nat → Bool
  | [] | [_] => true
  | n1 :: n2 :: ns =>
    if increasing then
      if n2 > n1 && n2 - n1 ≤ 3 then isSafeAux increasing (n2 :: ns) else false else
      if n1 > n2 && n1 - n2 ≤ 3 then isSafeAux increasing (n2 :: ns) else false

def isSafe (ns : List Nat) : Bool :=
  if h : ns.length ≤ 1 then true else
    let increasing := ns[0] ≤ ns[1]
    isSafeAux increasing ns

def countSafe (reports : Array Report) : Nat :=
  reports |>.filter isSafe
          |>.size

def part1 : IO String := do
  let reports ← getInput
  return s!"{countSafe reports}"

#eval part1

def isSafeIsh (report : Report) : Bool :=
  let reportLength := report.length
  List.range reportLength |>.map (fun idx => report.eraseIdx idx)
                          |>.map isSafe
                          |>.any (· = true)

def countSafeIsh (reports : Array Report) : Nat :=
  reports |>.filter isSafeIsh
          |>.size

def part2 : IO String := do
  let reports ← getInput
  return s!"{countSafeIsh reports}"

#eval part2
