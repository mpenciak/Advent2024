import Std.Data.HashMap

open Std

def getInput : IO (List Nat × List Nat) := do
  let lines ← IO.FS.lines "./inputs/day1.txt"
  let mut list1 : List Nat := []
  let mut list2 : List Nat := []
  for line in lines do
    let parts := line.splitOn "   "
    let parts := parts.map fun part => part.toNat!
    list1 := parts[0]! :: list1
    list2 := parts[1]! :: list2

  return (list1, list2)

def part1 : IO String := do
  let mut (list1, list2) ← getInput
  list1 := list1.mergeSort
  list2 := list2.mergeSort
  let answer := list1 |>.zip list2
                      |>.map (fun (a, b) => (max a b) - (min a b))
                      |>.foldl (· + ·) 0

  return s!"{answer}"

def part2 : IO String := do
  let (list1, list2) ← getInput
  let mut count : HashMap Nat Nat := HashMap.empty
  for n in list2 do
    if n ∈ count then
      count := count.insert n (count[n]! + 1)
    else
      count := count.insert n 1

  let mut answer := 0
  for n in list1 do
    if n ∈ count then
      answer := answer + count[n]! * n

  return s!"{answer}"

#eval part1
#eval part2
