⍝ Data Representation
⍝
⍝ We use a Matrix, m, paired with a nodelist, n. The nodelist maps each letter
⍝ to a number. The matrix dimensions are the number of stacks times the length
⍝ of the nodelist, ≢n. Each stack is encoded as the depth of the element of the
⍝ nodelist at said position. 0 if the element is absent from that stack. 
⍝
⍝ Printing to topmost elemensts
⍝
⍝ To get our answer we need to print the topmost elements. To do so we need to
⍝ get a vector of the positions of the largest element of each column, then
⍝ map them to the letters using the node list.  

⍝ Step function
⍝ move 1 from 2 to 1
⍝ We need to determine the 'topmost' element (the index) in the 2nd column.
⍝ Then 1+ the max depth in the 1st column.

⍝ Lets see how it works for the for the example
n←'CDMNPZ'
g←⍉↑(0 0 0 1 0 2)(2 3 1 0 0 0)(0 0 0 0 1 0)
