namespace MKXK.DOE

module Oneway =
    let SSB (m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let x = Matrix2.sum m
        let mutable acc = 0.
        for j in 0..m.J - 1 do 
            let xj = Matrix2.sumJ j m
            acc <- acc + xj * xj
        acc / I - (x * x) / (I * J)

    let SSW (m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let x = Matrix2.sum m
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do
                let xij = m[i,j]
                acc <- acc + xij * xij
        let mutable xj2 = 0.
        for j in 0..m.J - 1 do
            let xj = Matrix2.sumJ j m
            xj2 <- xj2 + xj * xj
        acc - xj2 / I

    let SST (m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let x = Matrix2.sum m
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do
                let xij = m[i,j]
                acc <- acc + xij * xij
        acc - (x * x) / (I * J)

    let fB (m:Matrix2) = float m.J - 1.

    let fW (m:Matrix2) = float (m.J * (m.I - 1))

    let fT (m:Matrix2) = float (m.J * m.I - 1)

    let MSB (m:Matrix2) = SSB m / fB m

    let MSW (m:Matrix2) = SSW m / fW m


module Twoway =
    /// degrees of freedom between columns
    let fC (m:Matrix2) = float (m.J - 1)

    /// degrees of freedom between rows
    let fR (m:Matrix2) = float (m.I - 1)

    /// degrees of freedom residual variance - error
    let fE (m:Matrix2) = float ((m.I - 1) * (m.J - 1))

    /// degrees of freedom total
    let fT (m:Matrix2) = float (m.I * m.J - 1)

    /// sum of sqaure definition between columns
    let SSC(m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let xbar = Matrix2.sum m / (I * J)
        let mutable acc = 0.
        for j in 0..m.J - 1 do
            let xbarj = (Matrix2.sumJ j m) / I
            acc <- acc + (xbarj - xbar) * (xbarj - xbar)
        acc * I

    /// sum of squares-definition between rows
    let SSR (m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let xbar = Matrix2.sum m / (I * J)
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            let xbari = Matrix2.sumI i m / J
            acc <- acc + (xbari - xbar) * (xbari - xbar)
        J * acc
        
    /// sum of squares-definition residual varriance-error
    let SSE (m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let xbar = Matrix2.sum m / (I * J)
        let mutable sum = 0.0
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do
                let xij = m[i,j]
                let xbari = Matrix2.sumI i m / J
                let xbarj = Matrix2.sumJ j m / I
                sum <- sum + (xij - xbari - xbarj + xbar) * (xij - xbari - xbarj + xbar)                
        sum
    
    /// Sum of square-deginition total
    let SST (m:Matrix2) =
        let I = float m.I
        let J = float m.J
        let xbar = Matrix2.sum m / (I * J)
        let mutable sum = 0.0
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do
                let xij = m[i,j]
                sum <- sum + (xij - xbar) * (xij - xbar)        
        sum

    /// mean squares between columns
    let MSC (matrix: Matrix2) = SSC(matrix) / fC(matrix)
    

    /// mean squares between rows
    let MSR (matrix: Matrix2) = SSR(matrix) / fR(matrix)    

    /// mean squares residual varriance-error
    let MSE (matrix: Matrix2) = SSE(matrix) / fE(matrix)

    /// test statistic (F) between columns
    let FB (matrix: Matrix2) = MSC(matrix) / MSE(matrix)

    /// test statistic (F) between rows
    let FR(matrix: Matrix2) =  MSR(matrix) / MSE(matrix)
    

module Interactions =
    /// degrees of freedom between columns
    let fC (matrix: Matrix3) = float (matrix.J - 1)

    /// degrees of freedom between rows
    let fR (matrix: Matrix3) = float (matrix.I - 1)    

    /// degrees of freddom interactions columns - rows
    let fCR (matrix: Matrix3) = float ((matrix.J - 1) * (matrix.I - 1))

    /// degrees of freedom error
    let fE (matrix: Matrix3) = float (matrix.I * matrix.J * (matrix.K - 1))

    // degrees of freedom total
    let fT (matrix: Matrix3) = float (matrix.I * matrix.J * matrix.K - 1)

    /// sum of squares-definition
    let SSC (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let mutable sum = 0.0
        let xbar = Matrix3.sum matrix / (I * J * K)
        for j in 0..matrix.J - 1 do 
            let xbarj = Matrix3.sumJ j matrix / (I * K)
            sum <- sum + (xbarj - xbar) * (xbarj - xbar)
        I * K * sum

    /// sum of squares-definition between columns
    let SSR (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let mutable sum = 0.0
        let bar = Matrix3.sum matrix / (I * J * K)
        for i in 0..matrix.I - 1 do
            let xbari = Matrix3.sumI i matrix / (J * K)
            sum <- sum + (xbari - bar) * (xbari - bar)
        J * K * sum    

    /// sum of square-definition iteraction columns-rows
    let SSCR (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let mutable sum = 0.0
        let xbar = Matrix3.sum matrix / (I * J * K)
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                let xbarij = Matrix3.sumIJ i j matrix / (K)
                let xbari = Matrix3.sumI i matrix / (J * K)
                let xbarj = Matrix3.sumJ j matrix / (I * K)
                sum <- sum + (xbarij - xbari - xbarj + xbar)
        sum

    /// sum of squares-definition error
    let SSE (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let mutable sum = 0.0
        let xbar = Matrix3.sum matrix
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    let xijk = matrix[i, j, k]
                    let xbari = Matrix3.sumI i matrix / (J * K)
                    let xbarj = Matrix3.sumJ j matrix / (I * K)
                    sum <- sum + (xijk - xbari - xbarj + xbar) * (xijk - xbari - xbarj + xbar)        
        sum    

    /// sum of squares-definition
    let SST (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let mutable sum = 0.0
        let xbar = Matrix3.sum matrix / (I * J * K)
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    let xijk = matrix[i, j, k]
                    sum <- sum + (xijk - xbar) * (xijk - xbar)
        sum  
    
    /// mean squares between columns
    let MSC (matrix: Matrix3) = SSC(matrix) / float (matrix.J - 1)

    /// mean squares between rows
    let MSR (matrix: Matrix3) = SSR(matrix) / float (matrix.I - 1)

    /// mean squares iteraction columns-rows
    let MSCR (matrix: Matrix3) = SSCR(matrix) / float ((matrix.I - 1) * (matrix.J - 1))    

    /// test statistic between columns
    let FC (matrix: Matrix3) =
        // _ = matrix;
        // return MSC(matrix) / MSE(matrix) no MSE in table 1.24;
        failwith "not implemented"

    /// test statistic between rows
    let FR (matrix: Matrix3) = failwith "not implemented"

    /// test statistic iteraction columns-rows
    let FCR (matrix: Matrix3) = failwith "not implemented"


module Threeways =
    /// degrees of freedom between columns
    let fC (matrix: Matrix3) = float (matrix.J - 1)

    /// degrees of freeedom between rows
    let fR (matrix: Matrix3) = float (matrix.I - 1)

    /// degrees of freedom between layers
    let fL (matrix: Matrix3) = float (matrix.K - 1)

    /// degrees of freedom iteraction C x R
    let fCR (matrix: Matrix3) = float ((matrix.J - 1) * (matrix.I - 1))

    /// degrees of freedom iteraction C x L
    let fCL (matrix: Matrix3) = float ((matrix.J - 1) * (matrix.K - 1))
    
    /// degrees of freedom iteraction R x L
    let fRL (matrix: Matrix3) = float ((matrix.I - 1) * (matrix.K - 1))
    
    /// degrees of freedom iteraction C x R x L
    let fCRL (matrix: Matrix3) = float ((matrix.I - 1) * (matrix.J - 1) * (matrix.K - 1))
    
    /// sum of squares definition between columns
    let SSC (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for j in 0..matrix.J - 1 do
            let xbarj = Matrix3.sumJ j matrix / (I * K)
            sum <- sum + (xbarj - xbar) * (xbarj - xbar)
        I * K * sum

    /// sum of squares definition between rows
    let SSR (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            let xbari = Matrix3.sumI i matrix / (J * K)
            sum <- sum + (xbari - xbar) * (xbari - xbar)        
        J * K * sum

    /// sum of squares definition between layers
    let SSL (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for k in 0..matrix.K - 1 do
            let xbark = Matrix3.sumK k matrix / (I * J)
            sum <- sum + (xbark - xbar) * (xbark - xbar)        
        I * J * sum

    /// sum of squares definition iteraction C x R
    let SSCR (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                let xbarij = Matrix3.sumIJ i j matrix / K
                let xbari = Matrix3.sumI i matrix / (J * K)
                sum <- sum + (xbarij - xbari - xbar) * (xbarij - xbari - xbar)
        K * sum;

    /// sum of squares definition iteraction C x L
    let SSCL (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for j in 0..matrix.J - 1 do
            for k in 0..matrix.K - 1 do
                let xbarjk = Matrix3.sumJK j k matrix / I
                let xbarj = Matrix3.sumJ j matrix / (I * K)
                let xbark = Matrix3.sumK k matrix / (I * J)
                sum <- sum + (xbarjk - xbarj - xbark - xbar) * (xbarjk - xbarj - xbark - xbar)
        I * sum

    /// sum of squares definition iteraction R x L
    let SSRL (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for k in 0..matrix.K - 1 do
                let xbarik = Matrix3.sumIK i k matrix / J
                let xbari = Matrix3.sumI k matrix / (J * K)
                let xbark = Matrix3.sumK k matrix / (I * J)
                sum <- sum + (xbarik - xbari - xbark - xbar) * (xbarik - xbari - xbark - xbar)
        J * sum

    /// sum of squares definition iteraction C x R x L
    let SSCRL (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let xbar = Matrix3.sum matrix / (I * J * K)
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    let xijk = matrix[i,j,k]
                    let xbarij = Matrix3.sumIJ i j matrix / K
                    let xbarik = Matrix3.sumIK i k matrix / J
                    let xbarjk = Matrix3.sumJK j k matrix / I
                    let xbari = Matrix3.sumI k matrix / (J * K)
                    let xbarj = Matrix3.sumJ j matrix / (I * K)
                    let xbark = Matrix3.sumK k matrix / (I * J)
                    sum <- sum + (xijk - xbarij - xbarik - xbarjk + xbari + xbarj + xbark + xbar) *
                        (xijk - xbarij - xbarik - xbarjk + xbari + xbarj + xbark + xbar)
        sum

    let MSC (matrix: Matrix3) = SSC(matrix) / fC(matrix)

    let MSR(matrix: Matrix3) = SSR(matrix) / fR(matrix)

    let MSL(matrix: Matrix3) = SSL(matrix) / fL(matrix)

    let MSCR (matrix: Matrix3) = SSCR(matrix) / fCR(matrix)

    let MSCL (matrix: Matrix3) = SSCL(matrix) / fCL(matrix)

    let MSRL (matrix: Matrix3) = SSRL(matrix) / fRL(matrix)

    let MSCRL (matrix: Matrix3) = SSCRL(matrix) / fCRL(matrix)

    let FC (matrix: Matrix3) = failwith "not implemented"
    
    let FR (matrix: Matrix3) = failwith "not implemented"
    
    let FL (matrix: Matrix3) = failwith "not implemented"
    
    let FCR (matrix: Matrix3) = failwith "not implemented"
    
    let FCL (matrix: Matrix3) = failwith "not implemented"
    
    let FRL (matrix: Matrix3) = failwith "not implemented"
    
    let FCRL (matrix: Matrix3) = failwith "not Implemented"


module ThreewayReplications =
    /// degrees of freedom between columns
    let fC (matrix: Matrix4) = float (matrix.J - 1)

    /// degrees of freeedom between rows
    let fR(matrix: Matrix4) = float (matrix.I - 1)

    /// degrees of freedom between layers
    let fL (matrix: Matrix4) = float (matrix.K - 1)

    /// degrees of freedom iteraction C x R
    let fCR (matrix: Matrix4) = float ((matrix.J - 1) * (matrix.I - 1))

    /// degrees of freedom iteraction C x L
    let fCL (matrix: Matrix4) = float ((matrix.J - 1) * (matrix.K - 1))

    /// degrees of freedom iteraction R x L
    let fRL (matrix: Matrix4) = float ((matrix.I - 1) * (matrix.K - 1))

    /// degrees of freedom iteraction C x R x L
    let fCRL (matrix: Matrix4) = float ((matrix.I - 1) * (matrix.J - 1) * (matrix.K - 1))

    /// degrees of freedom error
    let fE (matrix: Matrix4) = float (matrix.N - (matrix.I * matrix.J * matrix.K))

    /// degrees of freedom total
    let fT (matrix: Matrix4) = float (matrix.N - 1)


    let FC (matrix: Matrix4) =
        // return MSC(matrix) / MSE(matrix);
        failwith "not implemented"   

    let FR (matrix: Matrix4) =
        // return MSR(matrix) / MSE(matrix);
        failwith "not implemented"

    let FL (matrix: Matrix4) =
        // return MSL(matrix) / MSE(matrix);
        failwith "not implemented"

    let FCR (matrix: Matrix4) =
        // return MSCR(matrix) / MSE(matrix);
        failwith "not implemented"

    let FCL (matrix: Matrix4) =
        // return MSCL(matrix) / MSE(matrix);
        failwith "not implemented"

    let FRL (matrix: Matrix4) =
        // return MSRL(matrix) / MSE(matrix);
        failwith "not implemented"
    
    let FCRL (matrix: Matrix4) =
        // return MSCRL(matrix) / MSE(matrix);
        failwith "not implemented"

    /// sum of sauares definition between columns
    let SSC (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix4.sumJ j matrix
            sum <- sum + xj * xj    
        sum / (I * J * K) - (x * x) / N    

    /// sum of sauares definition between rows
    let SSR (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix4.sumI i matrix
            sum <- sum + xi * xi
        sum / (J * K * L) - (x * x) / N

    /// sum of sauares definition between layers
    let SSL (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable sum  = 0.0
        for k in 0..matrix.K - 1 do
            let xk = Matrix4.sumK k matrix
            sum <- sum + xk * xk
        sum / (I * J * L) - (x * x) / N

    /// sum of sauares definition iteraction C x R
    let SSCR (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable t0 = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                let xij = Matrix4.sumIJ i j matrix
                t0 <- t0 + xij * xij              
        t0 <- t0 / (K * L)

        let mutable t1 = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix4.sumI i matrix
            t1 <- t1 + xi * xi
        t1 <- t1 / (J * K * L)

        let mutable t2 = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix4.sumJ j matrix
            t2 <- t2 + xj * xj
        t2 <- t2 / (I * K * L)
        t0 - t1 - t2 + (x * x) / N

    /// sum of sauares definition iteraction C x L
    let SSCL (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable t0 = 0.0
        for j in 0..matrix.J - 1 do
            for k in 0..matrix.K - 1 do
                let xjk = Matrix4.sumJK j k matrix
                t0 <- t0 + xjk * xjk    
        t0 <- t0 / (I * L)

        let mutable t1 = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix4.sumJ j matrix
            t1 <- t1 + xj * xj
        t1 <- t1 / (I * K * L)

        let mutable t2 = 0.0
        for k in 0..matrix.K - 1 do
            let xk = Matrix4.sumK k matrix
            t2 <- t2 + xk * xk
        t2 <- t2 / (I * J * L)
        t0 - t1 - t2 + (x * x) / N    


    /// sum of sauares definition iteraction R x L
    let SSRL (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable t0  = 0.0
        for i in 0..matrix.I - 1 do
            for k in 0..matrix.K - 1 do
                let xik = Matrix4.sumIK i k matrix
                t0 <- t0 + xik * xik       
        t0 <- t0 / (J * L)

        let mutable t1 = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix4.sumI i matrix
            t1 <- t1 + xi * xi
        t1 <- t1 / (J * K * L)

        let mutable t2 = 0.0
        for k in 0..matrix.K - 1 do
            let xk = Matrix4.sumK k matrix
            t2 <- t2 + xk * xk
        t2 <- t2 / (I * J * L)
        t0 - t1 - t2 + (x * x) / N
    
    /// sum of squares definition iteraction C x R x L
    let SSCRL (matrix: Matrix4) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = float (matrix.L)
        let N = I * J * K * L
        let x = Matrix4.sum matrix
        let mutable t0 = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do 
                for k in 0..matrix.K - 1 do 
                    let xijk = Matrix4.sumIJK i j k matrix
                    t0 <- t0 + xijk * xijk
        t0 <- t0 / L

        let mutable t1 = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                let xij = Matrix4.sumIJ i j matrix
                t1 <- t1 + xij * xij
        t1 <- t1 / K * L

        let mutable t2 = 0.0
        for i in 0..matrix.I - 1 do
            for k in 0..matrix.K - 1 do
                let xik = Matrix4.sumIK i k matrix
                t2 <- t2 + xik * xik
        t2 <- t2 / J * L

        let mutable t3 = 0.0
        for j in 0..matrix.J - 1 do
            for k in 0..matrix.K - 1 do
                let xjk = Matrix4.sumJK j k matrix
                t3 <- t3 + xjk * xjk
        t3 <- t3 / I * L

        let mutable t4 = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix4.sumI i matrix
            t4 <- t4 + xi * xi
        t4 <- t4 / J * K * L

        let mutable t5 = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix4.sumJ j matrix
            t5 <- t5 + xj * xj
        t5 <- t5 / I * K * L

        let mutable t6 = 0.0
        for k in 0..matrix.K - 1 do
            let xk = Matrix4.sumK k matrix
            t6 <- t6 + xk * xk
        t6 <- t6 / I * J * L
        t0 - t1 - t2 - t3 + t4 + t5 + t6 - (x * x) / N

    /// sum of squares definition error
    let SSE (matrix: Matrix4) =
        let mutable t0 = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    for l in 0..matrix.L - 1 do
                        let xijkl = matrix[i,j,k,l]
                        t0 <- t0 + xijkl * xijkl

        let mutable t1 = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    let xijk = Matrix4.sumIJK i j k matrix
                    t1 <- t1 + xijk * xijk
        t0 - t1 / float (matrix.L)

    /// sum of squares definition total
    let SST (matrix: Matrix4) =
        let x = Matrix4.sum matrix
        let mutable t0 = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    for l in 0..matrix.L - 1 do
                        let xijkl = matrix[i,j,k,l]
                        t0 <- t0 + xijkl * xijkl
        t0 - (x * x) / float (matrix.I * matrix.J * matrix.K * matrix.L)

    let MSC (matrix: Matrix4) = SSC(matrix) / fC(matrix)

    let MSR (matrix: Matrix4) = SSR(matrix) / fR(matrix)

    let MSL (matrix: Matrix4) = SSL(matrix) / float(fL(matrix))

    let MSCR (matrix: Matrix4) = SSCR(matrix) / float (fCR(matrix))

    let MSCL (matrix: Matrix4) = SSCL(matrix) / float (fCL(matrix))

    let MSRL (matrix: Matrix4) = SSRL(matrix) / float (fRL(matrix))

    let MSCRL (matrix: Matrix4) = SSCRL(matrix) / float (fCRL(matrix))

    let MSE (matrix: Matrix4) = SSE(matrix) / float(fE(matrix)) 

// module LatinSquare =

module GreacoLatinSquare =
    let fR (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square. must be n" else float (matrix.I - 1)

    let fC (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square. must be n" else float (matrix.I - 1)

    let fL (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square. must be n" else float (matrix.I - 1)

    let fG (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square. must be n" else float (matrix.I - 1)

    let fE (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square, must be n" else float ((matrix.I - 1) * (matrix.J - 3))

    let SSR (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square. must be n"

        let n = float matrix.I
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix4.sumI i matrix
            sum <- sum + xi * xi
        sum / n - x * x / (n * n)

    let MSR (matrix: Matrix4) = SSR(matrix) / fR(matrix)

    let SSC (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "not a square, must be n"

        let n = float matrix.I
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix4.sumJ j matrix
            sum <- sum + xj * xj
        sum / n - x * x / (n * n)

    let MSC (matrix: Matrix4) = SSC(matrix) / fC(matrix)

    let SSL (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "must be a square, must be n"

        let n = float matrix.I
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for k in 0..matrix.K - 1 do
            let xk = Matrix4.sumK k matrix
            sum <- sum + xk * xk
        sum / n - x * x / (n * n)

    let MSL (matrix: Matrix4) = SSL(matrix) / fL(matrix)

    let SSG (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "must be a square, must be n"

        let n = float matrix.I
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for l in 0..matrix.L - 1 do
            let xl = Matrix4.sumL l matrix
            sum <- sum + xl * xl
        sum / n - x * x / (n * n)

    let MSG (matrix: Matrix4) = SSG(matrix) / fG(matrix)

    let SSE (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "must be a sqaure, must be n"

        let n = float matrix.I
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    for l in 0..matrix.L - 1 do
                        let xijkl = matrix[i,j,k,l]
                        sum <- sum + xijkl * xijkl
        sum - (x * x) / (n * n) - SSR(matrix) - SSC(matrix) - SSL(matrix) - SSG(matrix)

    let MSE (matrix: Matrix4) = SSE(matrix) / fE(matrix)

    let SST (matrix: Matrix4) =
        if (matrix.I <> matrix.J) then failwith "must be square, not n"

        let n = float matrix.I
        let x = Matrix4.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    for l in 0..matrix.L - 1 do
                        let xijkl = matrix[i, j, k, l]
                        sum <- sum + xijkl * xijkl
        sum - (x * x) / (n * n)

/// Note: SS1+SS2=SS3+SS4; I=J; K=L.
module YoudensSquare = 
    let f1 (matrix: Matrix3) = float (matrix.I - 1)

    let f2 (matrix: Matrix3) = float (matrix.I - 1)

    let f3 (matrix: Matrix3) = float (matrix.J - 1)

    let f4 (matrix: Matrix3) = float (matrix.J - 1)

    let f5 (matrix: Matrix3) = float (matrix.K - 1)

    let fE (matrix: Matrix3) =
        let I = matrix.I
        let J = matrix.J
        let K = matrix.K
        float (I * K - J - I - K + 2)

    let fT (matrix: Matrix3) =
        let I = matrix.I
        let K = matrix.K
        float (I * K - 1)

    let SS1 (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix3.sumJ j matrix
            sum <- sum + xj * xj
        sum / K - x * x / (J * K)

    let MS1 (matrix: Matrix3) = SS1(matrix) / f1(matrix)

    let SS2 (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix3.sumI i matrix

            let mutable f_2 = 0.0
            for j in 0..matrix.J - 1 do
                let xj = Matrix3.sumJ j matrix
                f_2 <- f_2 + xj
            sum <- sum + K * xi - f_2
        (I - 1.) / (J * K * K * (K - 1.)) * (sum * sum)

    let MS2 (matrix: Matrix3) = SS2(matrix) / f2(matrix)

    let SS3 (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = K
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            let xi = Matrix3.sumI i matrix
            sum <- sum + xi * xi
        sum / L - x * x / (I * L)

    let MS3 (matrix: Matrix3) = SS3(matrix) / f3(matrix)

    let SS4 (matrix: Matrix3) = 
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = K
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for j in 0..matrix.J - 1 do
            let xj = Matrix3.sumJ j matrix
            
            let mutable f_2 = 0.0
            for i in 0..matrix.I - 1 do
                let xi = Matrix3.sumI i matrix
                f_2 <- f_2 + xi
            sum <- sum + L * xj - f_2
        (J - 1.) / (J * K * K * (K - 1.)) * (sum * sum) 

    let MS4 (matrix: Matrix3) = SS4(matrix) / f4(matrix)

    let SS5 (matrix: Matrix3) = 
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let L = K
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for k in 0..matrix.K - 1 do
            let xk = Matrix3.sumI k matrix
            sum <- sum + xk * xk
        sum / I - x * x / (J * K)

    let MS5 (matrix: Matrix3) =  SS5(matrix) / f5(matrix)

    let SSE (matrix: Matrix3) =
        let I = float (matrix.I)
        let J = float (matrix.J)
        let K = float (matrix.K)
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    let xijk = matrix[i, j, k]
                    sum <- sum + xijk * xijk
        sum - (x * x) / (J * K) - SS1(matrix) - SS2(matrix) - SS5(matrix)

    let MSE (matrix: Matrix3) = SSE(matrix) / fE(matrix)

    let SST (matrix: Matrix3) =
        let J = float (matrix.J)
        let K = float (matrix.K)
        let x = Matrix3.sum matrix
        let mutable sum = 0.0
        for i in 0..matrix.I - 1 do
            for j in 0..matrix.J - 1 do
                for k in 0..matrix.K - 1 do
                    let xijk = matrix[i, j, k]
                    sum <- sum + xijk * xijk
        sum - (x * x) / (J * K)

    let MST (matrix: Matrix3) = SST(matrix) / fT(matrix)
