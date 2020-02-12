 i ← ⍎¨⊃⎕NGET '1.input' 1
 +/(¯2 + ⌊ i ÷ 3) ⍝ 3271994
 f ← {(¯2+⌊⍵÷3) ⌈ 0}
 fr ← {n← f⍵ ⋄ n + (∇⍣(n>0))n}
 +/ fr ¨ i ⍝ 4905116

 ⍝ Suggested on the Orchard
 ⍝ fr ← +/{∧/0=⍵:0 ⋄ (⊢+∇)f⍵}