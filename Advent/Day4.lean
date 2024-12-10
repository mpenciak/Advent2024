namespace Day4

inductive Direction | U | UR | R | DR | D | DL | L | UL
  deriving Inhabited, DecidableEq, Repr

instance : ToString Direction where
  toString d := match d with
    | .U  => "U"
    | .UR => "UR"
    | .R  => "R"
    | .DR => "DR"
    | .D  => "D"
    | .DL => "DL"
    | .L  => "L"
    | .UL => "UL"

def allDirections : List Direction := [.U, .UR, .R, .DR, .D, .DL, .L, .UL]

abbrev Puzzle := Array (Array Char)
abbrev Point := Int × Int

def getInput : IO (Array $ Array Char) := do
  let rows ← IO.FS.lines "./inputs/day4.txt"
  return rows.map (Array.mk ∘ String.toList ∘ String.trim)

def findChars (puzzle : Puzzle) (c : Char) : Array Point := Id.run do
  let mut res := #[]
  for (row, rowIdx) in puzzle.zipWithIndex do
    for (char, colIdx) in row.zipWithIndex do
      if char == c then
        res := res.push (.ofNat colIdx, .ofNat rowIdx)
  return res

def Puzzle.getChar (point : Point) (puzzle : Puzzle) : Char :=
  let (col, row) := point
  puzzle[row.toNat]![col.toNat]!

def move (p : Point) (d : Direction) (maxDist : Nat) : Option Point :=
  let (col, row) := p
  let newP@⟨x,y⟩ := match d with
    | .U  => (col, row - 1)
    | .UR => (col + 1, row - 1)
    | .R  => (col + 1, row)
    | .DR => (col + 1, row + 1)
    | .D  => (col, row + 1)
    | .DL => (col - 1, row + 1)
    | .L  => (col - 1, row)
    | .UL => (col - 1, row - 1)
  if x > maxDist || y > maxDist || x < 0 || y < 0then none else some newP

def walk (point : Point) (dir : Direction) (dist maxDist : Nat) : List Point := Id.run do
  let mut res := [point]
  for _ in [:dist] do
    let newP? := move res.head! dir maxDist
    if let some newP := newP? then res := newP :: res
  return res

def getChars (point : Point) (dir : Direction) (puzzle : Puzzle) (len : Nat) : List Char :=
  let puzzleRows := puzzle.size
  let points := walk point dir (len - 1) (puzzleRows - 1) -- 3 because XMAS
  points.map (fun p => puzzle.getChar p)

def testString := "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

def testPuzzle : Puzzle := (testString.split (· == '\n') |>.map fun x =>
x.trim.toList.toArray).toArray

def part1 : IO String := do
  let mut answer := 0
  let puzzle ← getInput
  let xs := findChars puzzle 'X'
  for x in xs do
    let chars := allDirections.map fun s => (x, s, (getChars x s puzzle 4))
    let chars := chars.filter fun (_, _, y) => (y == ['S', 'A', 'M', 'X'])
    answer := answer + chars.length
  return toString answer

def getMass (aP : Point) (puzzle : Puzzle) : List Char × List Char :=
  let maxDist := puzzle.size - 1
  let ul? := move aP .UL maxDist
  let ur? := move aP .UR maxDist
  match ul?, ur? with
    | some ul, some ur =>
      (getChars ul .DR puzzle 3, getChars ur .DL puzzle 3)
    | _, _ => ([], [])


def masCheck (l : List Char) : Bool :=
  l == ['M', 'A', 'S'] || l == ['S', 'A', 'M']

def part2 : IO String := do
  let mut answer := 0
  let puzzle ← getInput
  let as := findChars puzzle 'A'
  for a in as do
    let (chars1, chars2) := getMass a puzzle
    if masCheck chars1 && masCheck chars2 then answer := answer + 1

  return s!"{answer}"

