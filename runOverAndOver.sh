#!/bin/bash
for ((count=1; true; count++)))) 
do
  echo count = $count
  cabal run Mamono -- -i "output.txt" -t 120 --output "output.txt" --plain-text "Haskell.txt"
done
