import Advent

def printDay (day : Nat) (part1 part2 : String) : IO Unit := do
  IO.println s!"Day {day}"
  IO.println "===================="
  IO.println s!"Part 1: {part1}"
  IO.println s!"Part 2: {part2}"
  IO.println ""

def main : IO Unit := do
  printDay 1 (← Day1.part1) (← Day1.part2)
  printDay 2 (← Day2.part1) (← Day2.part2)
  printDay 3 (← Day3.part1) (← Day3.part2)
  printDay 4 (← Day4.part1) (← Day4.part2)
  printDay 5 (← Day5.part1) (← Day5.part2)
  printDay 6 "" ""
  printDay 7 (← Day7.part1) (← Day7.part2)
  printDay 8 (← Day8.part1) (← Day8.part2)
