A B C ← X Y Z ← 0 1 2
i←↑⍎¨⊃⎕NGET'2.input'1
s← {3×3|1--/⍵} ⍝ Score
v← (1+⊢/) ⍝ Values
+/(v i)+(s i) ⍝ Part 1: 13526
