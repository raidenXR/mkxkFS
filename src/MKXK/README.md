
This `.fsprj` contains functions for parsing mathematical formulas in TeX-format
Evaluating numerically these functions, or even creating random generated functions.

It also contains `Gnuplot.fs` a wrapper class for easy use of Gnuplot from F#.
And `serializers.fs` some methods for creating simple `ascii` and `.html` files.

In **test/** directory there are examples for each use case of this library.

TODO: the Expr below lack implementation in Evaluation 
- Sum
- Prod
- IntIndefinite (from CAS book p.205 The Integration Algorithm) 
- PDE
