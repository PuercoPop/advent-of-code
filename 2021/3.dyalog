p←⍎¨↑⊃⎕NGET'3.input'1
gamma←2⊥(≢p)≥⍨2×+⌿p
epsilon←2⊥(≢p)≤⍨2×+⌿p
(2⊥(≢p)≥⍨2×+⌿p){⍺×⍵}2⊥(≢p)≤⍨2×+⌿p ⍝ Part 1: 1540244
ogr←{1=≢⍵:∨⌿⍵⋄d←(≢⍵)≤2×+/⍺⌷⍉⍵⋄(1+⍺)∇(d=⍺⌷⍉⍵)⌿⍵} ⍝ Oxygen Generator Rating
csr←{1=≢⍵:∨⌿⍵⋄d←(≢⍵)≤2×+/⍺⌷⍉⍵⋄(1+⍺)∇(d≠⍺⌷⍉⍵)⌿⍵} ⍝ CO2 Scrubber rating
(2⊥1ogr p){⍺×⍵}(2⊥1csr p) ⍝ Part 2: 4203981
