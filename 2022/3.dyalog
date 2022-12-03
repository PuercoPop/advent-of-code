st←⎕A,⍨¯1∘⎕C⎕A ⍝ Score table
+/{l←2÷⍨≢⍵⋄m←1,2</l<⍳≢⍵⋄st⍳⊃⊃∩/m⊂⍵}¨⊃⎕NGET'3.input'1 ⍝ Part 1
