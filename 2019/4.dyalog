 to ← {l←(⍺-1) ⋄ l + ⍳(⍵-l)}
 i ← (⍎¨⍕) ¨ 136818 to 685979
 asc ← {(⍋⍵)≡⍳⍴⍵}
 rep ← {∨/ (¯2≡/⊢) ⍵}
 v ← asc ∧ rep
 +/ v¨i ⍝ 1919

  ⍝ uniqueness
 ⍝ {∨/2=/⍵}

 i← ⍎¨⍕ 6848994
 rep ← {∨/ (¯2≡/⊢) ⍵}


 2,/'abcd'
 ⎕←2{⍺,'≡',⍵}/'abcd'
 ⎕←¯2{⍺,'≡',⍵}/'abcd'

So ¯2 f/ x is equivalent to 2 f⍨/ x
