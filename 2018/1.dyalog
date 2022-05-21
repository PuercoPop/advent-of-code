 i←⍎¨⊃⎕NGET '1.input' 1
 +/ i ⍝ Part 1
 ⍝ This gives me a train of 0s with 1s in the repeated elements.
 ⍝ {2≤+/⍵⍷x}¨x
 ⍝ ≢ is the tally
 t←+\(i,i,i,i,i,i,i,i)
