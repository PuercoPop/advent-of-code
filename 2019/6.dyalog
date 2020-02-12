 I←⊃⎕NGET '6.input' 1
 (')'≠⊃i) ⊆ (⊃i) ⍝ This works on 'one line'
 p ← { ')'(≠⊆⊢) ⍵} ⍝ This doesn't work
 p ¨ I ⍝ This works with nested arrays. Not with a nested Matrix
