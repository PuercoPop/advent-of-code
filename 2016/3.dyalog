 s←{⍎¨' '(≠⊆⊢)⍵}
 i←s¨⊃⎕nget '3.input' 1
 v←{a←1⊃⍵⋄b←2⊃⍵⋄c←3⊃⍵⋄((a+b)>c)∧((a+c)>b)∧((b+c)>a)}
 +/ v¨i ⍝ Part 1: 1050
