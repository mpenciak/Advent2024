import Std.Data.HashSet

namespace Day6

open Std (HashSet)

abbrev Point := Int × Int

inductive Dir | N | E | S | W
  deriving Inhabited, Repr, BEq, Hashable

def Point.next (p : Point) : Dir → Point
  | .N => (p.1, p.2 - 1)
  | .E => (p.1 + 1, p.2)
  | .S => (p.1, p.2 + 1)
  | .W => (p.1 - 1, p.2)

abbrev Guard := Point × Dir

def Guard.facing (guard : Guard) : Point := guard.fst.next guard.snd

def Dir.turnRight : Dir → Dir
  | .N => .E
  | .E => .S
  | .S => .W
  | .W => .N

def Guard.turn : Guard → Guard
  | ⟨p, d⟩ => ⟨p, d.turnRight⟩

def Guard.walk : Guard → Guard
  | ⟨p, d⟩ => (p.next d, d)

instance : ToString Dir where
  toString
    | .N => "N"
    | .E => "E"
    | .S => "S"
    | .W => "W"

abbrev Map := HashSet Point

def exampleInput : String :=
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

structure Context where
  walls : Map
  dims : Nat

def parseMap (input : String) : Context × Guard := Id.run do
  let mut walls := HashSet.empty
  let lines := input.splitOn "\n"
  let dims := lines.length
  let mut guard : Point × Dir := ((0, 0), .N)
  for (row, line) in lines.enum do
    for (col, char) in line.toList.enum do
      let point := (.ofNat col, .ofNat row)
      match char with
      | '#' => walls := walls.insert point
      | '^' => guard := (point, .N)
      | 'v' => guard := (point, .S)
      | '<' => guard := (point, .W)
      | '>' => guard := (point, .E)
      | _ => ()
  return (⟨walls, dims⟩, guard)

abbrev Visited := HashSet Point

structure State where
  visited : Visited
  guard : Guard
  poses : HashSet Guard

abbrev SolutionM := ReaderT Context $ StateM State

def turn : SolutionM Guard := do
  modify fun ⟨visited, guard, poses⟩ =>
    {visited, guard := guard.turn, poses}
  return (← get).guard

def move : SolutionM Guard := do
  let mut ⟨_, guard, _⟩ ← get
  let mut nextPoint := guard.facing
  let ⟨map, _⟩ ← read
  while nextPoint ∈ map do
    guard ← turn
    nextPoint := guard.facing
  modify fun ⟨visited, guard, poses⟩ =>
    {visited, guard := guard.walk, poses}
  return (← get).guard

def step : SolutionM Unit := do
  let newGuard ← move
  modify fun ⟨visited, guard, poses⟩ =>
    {visited := visited.insert newGuard.fst, guard, poses}

def inBounds : SolutionM Bool := do
  let ⟨_, bounds⟩ ← read
  let ⟨_, ⟨(x, y) , _⟩, _⟩ ← get
  return x ≥ 0 && y ≥ 0 && x < bounds && y < bounds

def solution1 : SolutionM Unit := do
  while (← inBounds) do
    step

def SolutionM.run : Visited :=
  let (map, guard) := parseMap exampleInput
  let blah := ReaderT.run solution1 map
  let bblah := Prod.snd <$> (StateT.run blah ⟨HashSet.empty.insert guard.fst, guard,
  HashSet.empty.insert guard⟩) |>.visited
  bblah

def getInput : IO String := do
  let input ← IO.FS.readFile "./inputs/day6.txt"
  return input

def part1 : IO String := do
  let (map, guard) := parseMap (← getInput)
  let visited := Prod.snd <$> StateT.run (ReaderT.run solution1 map)
    ⟨HashSet.empty.insert guard.fst, guard, HashSet.empty.insert guard⟩ |>.visited
  return s!"{visited.toList.length - 1}" -- `-1` because I'm counting the out of bounds point lol

def looped (newGuard : Guard) : SolutionM Bool := do
  let ⟨_, _, poses⟩ ← get
  return newGuard ∈ poses

def log (newGuard : Guard) : SolutionM Unit := modify
  fun ⟨visited, guard, poses⟩ => {visited, guard, poses := poses.insert newGuard}

-- returns True is loops, False if not
def solution2 : SolutionM Bool := do
  while true do
    let newGuard ← move
    if !(← inBounds) then return false
    else if (← looped newGuard) then return true
    else
      log newGuard
      continue
  panic! "Shouldn't happen"

def part2 : IO String := do
  let mut looper : List Point := []
  let (map, guard) := parseMap (← getInput)
  let dims := map.dims
  let guardPos := guard.fst
  for r in [:dims] do
    for c in [:dims] do
      let newWall := (.ofNat c, .ofNat r)
      if newWall == guardPos then continue
      let loopMap := map.walls.insert newWall
      let context : Context := {walls := loopMap, dims := dims}
      let state : State :=
        {visited := HashSet.empty.insert guard.fst, guard, poses := HashSet.empty.insert guard}
      let loops := Prod.fst <$> StateT.run (ReaderT.run solution2 context) state |>.run
      if loops then
        looper := newWall :: looper

  return s!"{looper.length}"

