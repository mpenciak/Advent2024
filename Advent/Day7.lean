namespace Day7

inductive Op | Add | Mul | Concat
deriving Inhabited, Repr

instance : ToString Op where
  toString
    | Op.Add => "+"
    | Op.Mul => "*"
    | Op.Concat => "||"

def getAllOps (len : Nat) (concat : Bool): List (List Op) :=
  match len with
  | 0 => []
  | 1 => if concat then [[Op.Add], [Op.Mul], [Op.Concat]] else [[Op.Add], [Op.Mul]]
  | len + 1 =>
    let ops := getAllOps len concat
    let ops' := ops.map (fun op => Op.Add :: op)
    let ops'' := ops.map (fun op => Op.Mul :: op)
    let ops''' := if concat then ops.map (fun op => Op.Concat :: op) else []
    ops' ++ ops'' ++ ops'''

def evaluateOps (ops : List Op) (nums : List Nat) : Nat := Id.run do
  assert! ops.length == nums.length - 1
  let mut answer := nums.head!
  for (op, num) in ops.zip nums.tail do
    answer := match op with
      | Op.Add => answer + num
      | Op.Mul => answer * num
      | Op.Concat => String.toNat! (ToString.toString answer ++ ToString.toString num)
  return answer

def reachesTarget (target : Nat) (nums : List Nat) (concat : Bool) : Bool :=
  let allOps := getAllOps (nums.length - 1) concat
  allOps.any fun ops => evaluateOps ops nums == target

def testInput := "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

def getTestInput : Array (Nat × List Nat) :=
  let lines := testInput.splitOn "\n" |>.toArray
  lines.map fun line =>
    let line := (line.splitOn ":")
    let target := line[0]!.toNat!
    let nums := line[1]!.trim.splitOn " " |>.map String.toNat!
    (target, nums)

def getInput : IO (Array (Nat × List Nat)) := do
  let lines := (← IO.FS.lines "./inputs/day7.txt") |>.map String.trim
  return lines.map fun line =>
    let line := (line.splitOn ":")
    let target := line[0]!.toNat!
    let nums := line[1]!.trim.splitOn " " |>.map String.toNat!
    (target, nums)

def part1 : IO String := do
  let input ← getInput
  let blah := input.filter (fun (target, nums) => reachesTarget target nums false)
  let answer := blah |>.foldl (fun acc (target, _) => acc + target) 0
  return s!"{answer}"

def part2 : IO String := do
  let input ← getInput
  let blah := input.filter (fun (target, nums) => reachesTarget target nums true)
  let answer := blah |>.foldl (fun acc (target, _) => acc + target) 0
  return s!"{answer}"

