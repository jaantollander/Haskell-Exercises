# Logic and Codes
- https://en.wikibooks.org/wiki/Digital_Circuits/Logic_Operations
- http://mathworld.wolfram.com/Implies.html
- http://mathworld.wolfram.com/Equivalent.html


| X | Y | `and` | `or` | `nand` | `nor` | `xor` | `nxor` | `impl` | `equ` |
|:-:|:-:|:-----:|:----:|:------:|:-----:|:-----:|:------:|:------:|:-----:|
| 1 | 1 |   0   |   0  |    1   |   1   |   0   |    1   |    1   |   1   |
| 1 | 0 |   0   |   1  |    1   |   0   |   1   |    0   |    0   |   0   |
| 0 | 1 |   0   |   1  |    1   |   0   |   1   |    0   |    1   |   0   |
| 0 | 0 |   1   |   1  |    0   |   0   |   0   |    1   |    1   |   1   |

- `x and y`, \(X \wedge Y\), \(X \cdot Y\)
- `x or y`, \(X \vee Y\), \(X + Y\)
- `not x`, \(\overline{X}\), \(\lnot X\)
- `x nand y`, \(\lnot (X \wedge Y)\), \(\overline{X \cdot Y}\)
- `x nor y`, \(\lnot (X \vee Y)\), \(\overline{X + Y}\)
- `x xor y`, \(X \oplus Y\)
- `x nxor y`, \(\overline{X \oplus Y}\)
- `x impl y`, \(X \implies Y\)
- `x equ y`, \(X \equiv Y\)


Generating truth tables

`n=1`

| X |
|---|
| 1 |
| 0 |

`n=2`

| X | Y |
|---|---|
| 1 | 1 |
| 1 | 0 |
| 0 | 1 |
| 0 | 0 |

`n=3`

...
