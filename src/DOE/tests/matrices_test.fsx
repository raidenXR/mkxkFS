#load "../src/matrices.fs"
#load "../src/anova.fs"
#load "../src/modeling.fs"

open MKXK.DOE

let AssertBool b =
    if b then printfn "assert passed"
    else 
        System.Console.ForegroundColor <- System.ConsoleColor.Red
        printfn "assert failed"
        System.Console.ResetColor()

let Assert v0 v1 =
    if abs (v0 - v1) < 1e-5 then 
        printfn "assert passed" 
    else 
        System.Console.ForegroundColor <- System.ConsoleColor.Red
        printfn "assert failed, %g, %g" v0 v1
        System.Console.ResetColor()

let m0 = Matrix2(4, 3, [
    1.; 2.; 3.;
    1.; 2.; 3.;    
    1.; 2.; 3.;
    1.; 2.; 3.;
])

Assert (Matrix2.sum m0)  24.0

let m1 = Matrix2(5, 3, [
    23.0; 16.0; 18.0;
    21.0; 23.0; 22.0;
    24.0; 20.0; 25.0;
    17.0; 21.0; 21.0;
    19.0; 18.0; 20.0;    
])


AssertBool (m1[1,2] > 20.0)
m1[1,2] <- 12.
AssertBool (m1[1,2] > 20.0)

Assert (Matrix2.sumJ 0 m1) 104.
Assert (Matrix2.sumJ 1 m1) 98.
Assert (Matrix2.sumJ 2 m1) 106.
Assert (Matrix2.sumJ 0 m1 / float m1.I ) 20.8
Assert (Matrix2.sumJ 1 m1 / float m1.I ) 19.6
Assert (Matrix2.sumJ 2 m1 / float m1.I ) 21.2

let mutable xj = 0.
for j in 0..m1.J - 1 do xj <- xj + (Matrix2.sumJ j m1) * (Matrix2.sumJ j m1)
Assert xj 31656.
Assert (Matrix2.sum m1 * Matrix2.sum m1) 94864.
Assert (Oneway.MSW m1) 7.4
Assert (Oneway.MSB m1) 3.46667

let m2 = Matrix2(5, 4, [
    6.0;  14.0; -5.0;  -8.0;
    5.0;  11.0; -4.0;  -11.0;
    12.0; 0.0;  -5.0;  -5.0;
    9.0;  5.0;  -11.0; -7.0;
    10.0; 6.0;  -7.0;  -9.0;
])

Assert (Oneway.fB(m2)) 3.
Assert (Oneway.fW(m2)) 16.
Assert (Oneway.fT(m2)) 19.

Assert (Oneway.SSB(m2)) 1135.0
Assert (Oneway.SSW(m2)) 203.2
Assert (Oneway.SST(m2)) 1338.2

Assert (Oneway.MSB(m2)) 378.3333333
Assert (Oneway.MSW(m2)) 12.7

// let r = DesignMatrix.random 5 5
// printfn "%s" (string r)

// let l = DesignMatrix.latin 5 8
// printfn "%s" (string l)

// let gl = DesignMatrix.greacoLatin 5 5
// printfn "%s" (string gl)

// let dm = DesignMatrix.createDesignMatrix m2
// printfn "%s" (string dm)

// p. 294
let op_values = [|
    40; 10; 80; 10; 
    40; 180; 80; 180
|]
let op_matrix = Matrix<int>(4, 2, op_values)
let y_values = [|
    1182.4; 1139.2; 1136.5; 1209.4; 1134.6; 1159.2;
    622.4;  660.8;  631.8;  602.1;  668.6;  645.6;
    683.2;  624.0;  682.6;  699.5;  565.4;  611.2;
    496.0;  486.2;  495.6;  513.6;  450.6;  467.2;
|]
let m = Matrix<float>(4, 6, y_values)
printfn "test matrix item: %g" (m[3,3])
// let y = Matrix2(4, 6, y_values)
// let d = DesignMatrix.design(op_matrix)

// open Modeling
// Assert (bi d y 0) 731.98
// Assert (bi d y 1) -170.28
// Assert (bi d y 2) -167.38
// Assert (bij d y 1 2) 90.58

// let factors = [|731.98; -170.28; -167.38; 90.58|]
// let factors_res = polynomialLinear op_matrix y 2

// for (f, frs) in Array.zip factors factors_res do
//     printfn "%g   %g" f frs



let rm = DesignMatrix.random 13 8
printfn "%s" (string rm)

let lm = DesignMatrix.latin 12
printfn "%s" (string lm)

let glm = DesignMatrix.greacoLatin 12
printfn "%s" (string glm)
