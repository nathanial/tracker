/-
  Utility functions for string manipulation.
-/

namespace Tracker.Util

/-- Left-pad a string to a minimum length -/
def padLeft (s : String) (len : Nat) (c : Char) : String :=
  let padding := len - s.length
  if padding > 0 then
    String.mk (List.replicate padding c) ++ s
  else s

/-- Filter characters from a string -/
def filterChars (s : String) (p : Char → Bool) : String :=
  String.mk (s.toList.filter p)

/-- Find the index of the first character matching a predicate -/
def findIdx? (s : String) (p : Char → Bool) : Option Nat := Id.run do
  let chars := s.toList
  for i in [:chars.length] do
    if let some c := chars[i]? then
      if p c then return some i
  return none

/-- Trim whitespace from both ends -/
def trim (s : String) : String :=
  s.dropWhile Char.isWhitespace |>.dropRightWhile Char.isWhitespace

/-- Check if string starts with a prefix -/
def startsWith (s : String) (pre : String) : Bool :=
  s.take pre.length == pre

/-- Check if string ends with a suffix -/
def endsWith (s : String) (suf : String) : Bool :=
  s.takeRight suf.length == suf

/-- Check if string contains a substring -/
def containsSubstr (s : String) (needle : String) : Bool :=
  if needle.isEmpty then
    true
  else
    let haystack := s.toList
    let target := needle.toList
    let rec isPrefix : List Char → List Char → Bool
      | [], _ => true
      | _, [] => false
      | n :: ns, h :: hs => n == h && isPrefix ns hs
    let rec loop : List Char → Bool
      | [] => false
      | rest@(_ :: xs) =>
        if isPrefix target rest then true else loop xs
    loop haystack

end Tracker.Util
