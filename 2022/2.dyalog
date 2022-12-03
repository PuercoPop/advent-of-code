⍝ truth table entries
⎕←t←9 3 ⍴ ¯1+ 1 1 1 2 1 3 2 1 2 2 2 3 3 1 3 2 3 3

A B C ← X Y Z ← 0 1 2
i←↑⍎¨⊃⎕NGET'2.input'1
⍝ Part 1
s← {3×3|1--/⍵} ⍝ Score
v← (1+⊢/) ⍝ Values
+/(v i)+(s i) ⍝ Part 1: 13526
⍝ Part 2
s←3×⊢/
v←
