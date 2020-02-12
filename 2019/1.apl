i ← ⍎¨ ⎕FIO[49] '1.input'
+/¯2+⌈i÷3 ⍝ 1) 3272047

f ← {0⌈¯2+⌈⍵÷3}
fr ← {n←f⍵ ⋄ n + ∇⍣(n>0)}⍝ doesn't work
+/ fr ¨ i
