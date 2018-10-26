-- Can run by:
-- stack runghc AddBrackets.hs
module AddBrackets where

addBrackets s = "[" ++ s ++ "]"

result = map addBrackets ["one", "two", "three"]

main = print result