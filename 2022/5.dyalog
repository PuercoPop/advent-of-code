⍝ For the example
n←'CDMNPZ'
g←⍉↑(0 0 0 1 0 2)(2 3 1 0 0 0)(0 0 0 0 1 0)
⍝ How do I print the top row?

⍝ Step function
⍝ move 1 from 2 to 1
⍝ We need to determine the 'topmost' element (the index) in the 2nd column.
⍝ Then 1+ the max depth in the 1st column.
