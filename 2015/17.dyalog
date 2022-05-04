 i←⍎¨⊃⎕NGET '17.input' 1
 ex←20 15 10 5 5
 ps←{⌿∘⍵¨↓⌽⍉2⊥⍣¯1⊢¯1+⍳2*≢⍵} ⍝ Powerset. h/t aplcart.info
 +/150=+/¨ps i ⍝ Part 1 1638
 ⍝ For part two I want to the indices of the sets that match the condition and
 ⍝ use that to select those from the powerset. Then find the shortest set and
 ⍝ count all the ones that have the same length.
 ss←(150=+/¨ps i)⊆(ps i)
 ls←{⍴⊃⍵}¨ss
 +/ls=⌊/ls
