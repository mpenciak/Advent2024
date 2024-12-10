import Batteries.Data.List
import Batteries.Data.String
import Std.Internal.Parsec.Basic
import Std.Internal.Parsec.String


section Util

def List.flatten {α} : List (List α) → List α
  | [] => []
  | x :: xs => x ++ List.flatten xs

def List.getEvenElems {α} : List α → List α
  | [] => []
  | [x] => [x]
  | x :: _ :: xs => x :: List.getEvenElems xs

#eval [1,2,3,4,5,6].getEvenElems
#eval [1,2,3,4,5,6,7,8,9,10,11].getEvenElems

partial def Except.get! [Inhabited α] : Except ε α → α
  | .ok a => a
  | .error .. => panic! ":("

def String.splitFirst (string substring : String) : String × Option String :=
  let find := string.findSubstr? substring
  if let some find := find then
    let head := string |>.take find.startPos.byteIdx
    let tail := string |>.drop (find.startPos.byteIdx + substring.length)
    (head, tail)
  else
    (string, none)
end Util

namespace Day3

inductive Instruction
  | d
  | dnt
  | mul (n1 n2 : Nat)
deriving Inhabited, Repr

section Parsers

open Std.Internal.Parsec
open String

def parseMul : Parser Instruction := do
  skipChar '('
  let left ← digits
  skipChar ','
  let right ← digits
  skipChar ')'
  return .mul left right

def parseDo : Parser Instruction := do
  skipString "do()"
  return .d

def parseDont : Parser Instruction := do
  skipString "don't()"
  return .dnt

end Parsers

def testString := "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"


def getInput : IO String := IO.FS.readFile "./inputs/day3.txt"

def getMulValues (s : String) : Nat := s |>.splitOn "mul"
                                         |>.map parseMul.run
                                         |>.filter Except.isOk
                                         |>.map (fun x => Except.get! x)
                                         |>.map (fun inst =>
                                           match inst with
                                             | .d | .dnt => 0
                                             | .mul n1 n2 => n1 * n2
                                         )
                                         |>.foldl (· + ·) 0

#check String.extract


def part1 : IO String := do
  let input ← getInput
  return s!"{getMulValues input}"

#eval part1

def testString2 := "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

def findNextDo (s : String) : String :=
  let (_, tail) := s.splitFirst "do()"
  if let some tail := tail then
    tail
  else
    ""

partial def stripDonts (s : String) : String :=
  let (head, tail) := s.splitFirst "don't()"
  if let some tail := tail then
    head ++ stripDonts (findNextDo tail)
  else
    head

#eval stripDonts testString2

#eval testString2.splitFirst "don't()"

def part2 : IO String := do
  let input ← getInput
  let cleanedInput := stripDonts input
  return s!"{getMulValues cleanedInput}"

#eval part2


