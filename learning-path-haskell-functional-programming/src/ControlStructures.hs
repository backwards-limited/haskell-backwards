module ControlStructures where
  
myIf True thenFunc elseFunc = thenFunc
myIf False thenFunc elseFunc = elseFunc

main =
  let x = 5
  in print $ myIf (x == 5) "is five" "is not five"