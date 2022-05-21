⍝ It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
one ← {3≥+/⍵∊'aeiou'}
⍝ It contains at least one letter that appears twice in a row
⍝ like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
two ← {∨/2=/⍵}
⍝ It does not contain the strings ab, cd, pq, or xy
three ← {(1 1≢'ab'∊⍵)∨(1 1≢'cd'∊⍵)∨(1 1≢'pq'∊⍵)∨(1 1≢'xy'∊⍵)}
nice←{(one ⍵)∧(two ⍵)∧(three ⍵)}
+/nice¨⊃⎕NGET'5.input' 1 ⍝ part 1. It is wrong.
