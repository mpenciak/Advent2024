import Std.Data.HashSet
import Std.Data.HashMap

namespace Day8

open Std (HashSet HashMap)

abbrev Point := Int × Int

instance : Add Point where
  add p q := (p.1 + q.1, p.2 + q.2)

instance : Sub Point where
  sub p q := (p.1 - q.1, p.2 - q.2)

instance : HMul Int Point Point where
  hMul n p := (n * p.1, n * p.2)

instance: HMul Nat Point Point where
  hMul n p := (n * p.1, n * p.2)

instance : Neg Point where
  neg p := (-p.1, -p.2)

def inDims (dims : Nat) (p : Point) : Bool :=
  p.1 >= 0 && p.1 < dims && p.2 >= 0 && p.2 < dims

def getAntipodes (p1 p2 : Point) : Point × Point :=
  let diff := p2 - p1
  (p1 + 2 * diff, p2 + 2 * (-diff))

def getResonances (p1 p2 : Point) (dims : Nat) : List Point := Id.run do
  let diff := p2 - p1
  let mut res := []
  let mut p := p1

  while inDims dims p do
    res := p :: res
    p := p + diff

  p := p1
  while inDims dims p do
    res := p :: res
    p := p - diff

  return res

def getAllPairs (points : HashSet Point) : List (Point × Point) := Id.run do
  let pointList := points.toList
  let mut res := []
  for (idx, p) in points.toList.enum do
    for idx2 in [idx + 1 : pointList.length] do
        res := (p, pointList[idx2]!) :: res
  return res

def testInput := "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

def getTestInput : Nat × HashMap Char (HashSet Point) := Id.run do
  let mut res := HashMap.empty
  let lines := testInput.splitOn "\n"
  for (y, line) in lines |>.enum do
    for (x, c) in line.trim.toList.enum do
      if c == '.' then continue
      match res[c]? with
      | some s => res := res.insert c (s.insert (.ofNat x, .ofNat y))
      | none => res := res.insert c (HashSet.empty.insert (.ofNat x, .ofNat y))
  return (lines.length, res)

def getInput : IO (Nat × HashMap Char (HashSet Point)) := do
  let lines ← IO.FS.lines "./inputs/day8.txt"
  let mut res := HashMap.empty
  for (line, y) in lines.zipWithIndex do
    for (x, c) in line.trim.toList.enum do
      if c == '.' then continue
      match res[c]? with
      | some s => res := res.insert c (s.insert (.ofNat x, .ofNat y))
      | none => res := res.insert c (HashSet.empty.insert (.ofNat x, .ofNat y))
  return (lines.size, res)

def part1 : IO String := do
  let (dims, input) ← getInput
  let mut antiPodes : HashSet Point := HashSet.empty
  for (_, points) in input.toList do
    let pairs := getAllPairs points
    for (p1, p2) in pairs do
      let (a1, a2) := getAntipodes p1 p2
      if inDims dims a1 then antiPodes := antiPodes.insert a1
      if inDims dims a2 then antiPodes := antiPodes.insert a2
  return s!"{antiPodes.size}"

def part2 : IO String := do
  let (dims, input) ← getInput
  let mut resonances := HashSet.empty
  for (_, points) in input.toList do
    let pairs := getAllPairs points
    for (p1, p2) in pairs do
      let ps := getResonances p1 p2 dims
      for p in ps do
        resonances := resonances.insert p
  return s!"{resonances.size}"

