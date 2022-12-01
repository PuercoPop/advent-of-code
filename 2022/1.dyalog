l←⊃⎕NGET'1.input'1 ⍝lines
m←0≠≢¨l
in←+/¨⍎¨¨m⊆l
⎕←part1←⌈/in
⎕←part2←+/3↑in[⍒in]
