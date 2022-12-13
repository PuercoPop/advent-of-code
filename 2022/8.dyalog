m←⍎¨↑⊃⎕NGET'8.input'1
⍝ We need to drop the first row and replace it with a row of ¯1s to ensure the
⍝ topmost row if 
f←{⍵>¯1⍪¯1↓⌈⍀⍵} ⍝ visiblity mask
u←f m
r←(⌽∘⍉)           f           (⍉∘⌽)m
d←(⌽∘⍉)(⌽∘⍉)      f      (⍉∘⌽)(⍉∘⌽)m
l←(⌽∘⍉)(⌽∘⍉)(⌽∘⍉) f (⍉∘⌽)(⍉∘⌽)(⍉∘⌽)m
+/+/u∨r∨d∨l
