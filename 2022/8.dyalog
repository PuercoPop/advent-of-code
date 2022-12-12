m←⍎¨↑⊃⎕NGET'8.example'1
2,/m ⍝ To see pairings
⍝ visiblity masks
l←2</((≢m)⍴¯1),2(⊣⌈⊢)/m ⍝ from the left
⍝ We need to drop the first row and replace it with a row of ¯1s to ensure the
⍝ topmost row if 
f←{⍵>¯1⍪¯1↓⌈⍀⍵} ⍝ visiblity mask
l←m<⌈\m
u←m>¯1⍪¯1↓⌈⍀m ⍝ Up
