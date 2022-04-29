split←{⍎¨('x'≠⍵)⊆⍵}
⍝ {'x'(≠⊆⊢)⍵} is equivalent
lines←split¨⊃⎕NGET '2.input' 1
ssa←{×/ ¯1↓⍵[⍋⍵]} ⍝ The area of the smallest side
area←{+/ (×/2,⍵[1 2]) (×/2,⍵[2 3]) (×/2,⍵[1 3]) (ssa ⍵)}
+/area¨lines ⍝ Part 1
ribbon←{+/(×/2,+/¯1↓⍵[⍋⍵]) (×/ ⍵)}
+/ribbon¨lines ⍝ Part 2
