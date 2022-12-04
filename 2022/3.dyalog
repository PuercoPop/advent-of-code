st←⎕A,⍨¯1∘⎕C⎕A ⍝ Score table
+/{l←2÷⍨≢⍵⋄m←1,2</l<⍳≢⍵⋄st⍳⊃⊃∩/m⊂⍵}¨⊃⎕NGET'3.input'1 ⍝ Part 1
+/⊃¨{∩/↑⍵⊂⍨1 0 0⍴⍨≢⍵}(⎕A,⍨⎕C ⎕A)∘⍳¨⊃⎕NGET'3.input'1 ⍝ Part 2
