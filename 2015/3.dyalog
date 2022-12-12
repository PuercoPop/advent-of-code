 ⍝ We can use complex numbers to represent a 2D grid. The real part is the x
 ⍝ coordinate and the imaginary part the y coordinate. Furthermore we can use
 ⍝ the power function as a way to rotate the vector around by strides of 90 
 ⍝ degrees. 0J1*'>^<v'⍳ will translate the input to the corresponding vector.
 ⍝
 ⍝ 0j1*1 =>  0J 1
 ⍝ 0j1*2 => ¯1J 0
 ⍝ 0j1*3 =>  0J¯1
 ⍝ 0j1*4 =>  1J 0
 ⍝ 
 ⍝ Using a scan will give us a vector of the coordinates that represent the
 ⍝ path Santa took. Note that we need to include the initial score,
≢∪+\0,0J1*'>^<v'⍳⊃⊃⎕NGET'3.input'1 ⍝ Part 1

m←⊃⊃⎕NGET'3.input'1
s←0,m/⍨2|⍳≢⊢m  ⍝ Santa
r←0,m/⍨~2|⍳≢⊢m ⍝ Robo
f←+\0J1*'>^<v'⍳⊢
≢∪(f s),(f r) ⍝ Part 2
