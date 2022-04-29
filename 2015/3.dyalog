
 in←⊃⊃⎕NGET '3.input' 1
 ix←'v^<>'
 val← 0j¯1 0j1 ¯1j0 1j0
 ⍝ (ix⍳'v')⌷val How do I avoid the parents? Is selfie of use?
 ≢∪+\{(ix⍳⍵)⌷val}¨in ⍝ Part one, w/o origin
 ≢∪+\{0j1*'>^<v'⍳⍵}¨in
 ≢∪+\0,0J1*'>^<v'⍳in

 ⍝ Notes
 ⍝ We can avoid the mapping using ix and val as the power of -1j gives
 ⍝ as the mapping.
 ⍝         0j1*1
 ⍝ 0J1
 ⍝        0j1*2
 ⍝ ¯1
 ⍝        0j1*3
 ⍝ 0J¯1
 ⍝        0j1*4
 ⍝ 1



robo
⍝ {⍵/⍨2|⍳≢⍵} select
