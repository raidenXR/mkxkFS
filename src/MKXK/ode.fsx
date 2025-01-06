open System


module Int =
    let trapezoid () = ignore

    let simpson () = ignore

    let gauss () = ignore


module Diff =
    let forward () = ignore

    let central () = ignore

    let backward () = ignore    


module ODE =
    /// rk2.java solves an ODE with RHS given by the method f() using a second-
    /// order Runge–Kutta algorithm. Note that the method f(), which you will need to change for
    /// each problem, is kept separate from the algorithm, which is best not to change</summary>
    let rk2 (f: float * array<float> * array<float> -> unit) = 
        let mutable h = 0.
        let mutable t = 0.
        let mutable a = 0.
        let mutable b = 10.
        let y = Array.zeroCreate<float> 2
        let ydumb = Array.zeroCreate<float> 2
        let freturn = Array.zeroCreate<float> 2
        let k1 = Array.zeroCreate<float> 2
        let k2 = Array.zeroCreate<float> 2
        let mutable i = 0
        let mutable n = 100

        y[0] <- 3.
        y[1] <- -5.
        h <- (b - a) / (float n)
        t <- a

        while t < b do
            if ((t + h) > b) then h <- b - t
            f (t, y, freturn)
            k1[0] <- h * freturn[0]
            k1[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + k1[i] / 2.
            f(t + h / 2., ydumb, freturn)
            k2[0] <- h * freturn[0]
            k2[1] <- h * freturn[1]
            for i in 0..1 do y[i] <- y[i] + k2[i]
            t <- t + h 
        y
        
// Latex macros example to implement
// https://tex.stackexchange.com/questions/29179/partial-derivative-macro

    /// rk4.java solves an ODE with RHS given by the method f() using a fourth-
    /// order Runge–Kutta algorithm. Note that the method f(), which you will need to change for
    /// each problem, is kept separate from the algorithm, which is best not to change.
    let rk4 f =
        let mutable h = 0.
        let mutable t = 0.
        let mutable a = 0.
        let mutable b = 10.
        let y = [|3.; -5.|]
        let ydumb = Array.zeroCreate<float> 2
        let freturn = Array.zeroCreate<float> 2
        let k1 = Array.zeroCreate<float> 2
        let k2 = Array.zeroCreate<float> 2
        let k3 = Array.zeroCreate<float> 2
        let k4 = Array.zeroCreate<float> 2
        let mutable i = 0
        let mutable n = 100

        h <- (b - a) / (float n)
        t <- a
        
        while t < b do
            if ((t + h) > b) then h <- b - t
            f (t, y, freturn)
            k1[0] <- h * freturn[0]
            k1[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + k1[i] / 2.
            f(t + h / 2., ydumb, freturn)
            k2[0] <- h * freturn[0]
            k2[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + k2[i] / 2.
            f(t + h / 2., ydumb, freturn)
            k3[0] <- h * freturn[0]
            k3[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + k3[i]
            f(t + h, ydumb, freturn)
            k4[0] <- h * freturn[0]
            k4[1] <- h * freturn[1]
            for i in 0..1 do y[i] <- y[i] + (k1[i] + 2. * (k2[i] + k3[i]) + k4[i]) / 6.
            t <- t + h
        y


    /// rk45.java solves an ODE with RHS given by the method f() using a Runge–
    /// Kutta algorithm with adaptive step size that may yield fifth-order precision. Note that the
    /// method f(), which you will need to change for each problem, is kept separate from the al-
    /// gorithm, which is best not to change.
    let rk45 f =
        let mutable h = 0.
        let mutable t = 0.
        let mutable s = 0.
        let mutable s1 = 0.
        let mutable hmin = 0.
        let mutable hmax = 0.
        let mutable E = 0.
        let mutable Eexact = 0.
        let mutable error = 0.
        let mutable a = 0.
        let mutable b = 10.
        let y = [|3.; -5.|]
        let ydumb = Array.zeroCreate<float> 2
        let freturn = Array.zeroCreate<float> 2
        let err = Array.zeroCreate<float> 2
        let k1 = Array.zeroCreate<float> 2
        let k2 = Array.zeroCreate<float> 2
        let k3 = Array.zeroCreate<float> 2
        let k4 = Array.zeroCreate<float> 2
        let k5 = Array.zeroCreate<float> 2
        let k6 = Array.zeroCreate<float> 2
        let mutable i = 0
        let mutable j = 0
        let mutable n = 100
        let mutable flops = 0
        let mutable Tol = 1.0e-8
        let mutable n = 20
        let mutable sum = 0.

        h <- (b - a) / (float n)
        hmin <- h / 64.
        hmax <- h * 64.
        t <- a
        
        while t < b do
            if ((t + h) > b) then h <- b - t
            f (t, y, freturn)
            k1[0] <- h * freturn[0]
            k1[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + k1[i] / 4.
            f(t + h / 4., ydumb, freturn)
            k2[0] <- h * freturn[0]
            k2[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + 3. * k1[i] / 32. + 9. * k2[i] / 32.
            f(t + 3. * h / 8., ydumb, freturn)
            k3[0] <- h * freturn[0]
            k3[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + 1932. * k1[i] / 2197. - 7200. * k2[i] / 2197. + 7296. * k3[i] / 2197.
            f(t + 12. * h / 13., ydumb, freturn)
            k4[0] <- h * freturn[0]
            k4[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] + 439. * k1[i] / 216. - 8. * k2[i] + 3680. * k3[i] / 513. - 845. * k4[i] / 4104.    
            f(t + h, ydumb, freturn)
            k5[0] <- h * freturn[0]
            k5[1] <- h * freturn[1]
            for i in 0..1 do ydumb[i] <- y[i] - 8. * k1[i] / 27. + 2. * k2[i] - 3544. * k3[i] / 2565. + 1859. * k4[i] / 4104. - 11. * k5[i] / 40.
            f(t + h  /2., ydumb, freturn)
            k6[0] <- h * freturn[0]
            k6[1] <- h * freturn[1]
            for i in 0..1 do err[i] <- abs (k1[i] / 360. - 128. * k3[i] / 4275. - 2197. * k4[i] / 75240. + k5[i] / 50. + 2. * k6[i] / 55.)            
            if err[0] < Tol || err[1] < Tol || h <= 2. * hmin then
                for i in 0..1 do y[i] <- y[i] + 25. * k1[i] / 216. + 1408. * k3[i] / 2565. + 2197. * k4[i] / 4104. - k5[i] / 5.
                t <- t + h
                j <- j + 1
            if err[0] = 0 || err[1] = 0 then s <- 0
            else s <- 0.84 * Math.Pow(Tol * h / err[0], 0.25)

            if s < 0.75 && h > 2. * hmin then h <- h / 2.
            elif s > 1.5 && 2. * h < hmax then h <- h * 2.
            flops <- flops + 1
            E <- Math.Pow(y[0], 6.) + 0.5 * y[1] * y[1]
            Eexact <- 1.
            error <- abs ((E - Eexact) / Eexact)
            sum <- sum + error
        y
            

module PDE =

    let PDEsolver () = failwith "Not specific generic algorithm of solving PDEs in the book..."
