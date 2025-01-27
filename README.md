# Talpin 1992 in Flix
An implementation of "Polymorphic type, region and effect inference" by Talpin and Jouvelot ([doi](https://doi.org/10.1017/S0956796800000393)).

## Example Usage
- Run `Talpin.Repl.main()` via VsCode and write fx `flix: let f = \x -> x; let g = \y -> f(1) + f(y); g(42)`
