 i←⊃⎕NGET '6.input' 1
 e←⊃⎕NGET '6.example' 1
 ⍝ '\w+'⎕S'&'¨⊃⎕NGET '6.example' 1
 p ← { ')'(≠⊆⊢) ⍵} ⍝ Split sequence on (.
 ⍝ An alternative approach is to use the ⎕S operator to select word characters,
 ⍝ \w+. '&' is a placeholder for the match.
 ⍝ a b←↓⍉↑'\w+'⎕S'&'¨⊃⎕NGET'p6.txt'1
 ⍉↑p ¨ i

⍝ jayfoad's solution 
a b←↓⍉↑'\w+'⎕S'&'¨⊃⎕NGET'p6.txt'1
p←b⍳a ⍝ parent index
+/{0,⍨1+⍵[p]}⍣≡0/⍨1+≢a ⍝ part 1
{¯2+≢⍺(∪~∩)⍵}/{3::⍬ ⋄ ⍵,∇⍵⊃p}¨b⍳'SAN' 'YOU' ⍝ part 2 
