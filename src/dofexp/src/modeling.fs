namespace Dofexp

module Modeling =

    type Designs =
        | FUFE
        | FRFE

    type Polynomial = {
        b0: float
        bi: array<float>
        bij: array<float>
    }

    /// p. 271 (2.61)
    /// factor iteractions
    let C k M =
        let mutable a = 1
        let mutable b = 1

        let mutable A = 1
        while A <= M do a <- a * (k - A + 1); A <- A + 1

        let mutable B = 1
        while B <= M do b <- b * B; B <- B + 1 

        a / b // @divFloor(a, b)
        
    let private sgn i j (m:Matrix<byte>) = if m[i, j] < 128uy then -1. else 1.
    
    let private avg i (m:Matrix2) = 
        let mutable acc = 0.
        for j in 0..m.J - 1 do acc <- acc + float m[i,j]
        acc / float (m.J)
        
    /// computes the factor for the i-th row
    let bi (m:Matrix<byte>) (responses:Matrix2) (n:int) =            
        let N = float m.I
        let mutable b = 0.

        if n = 0 then
            for u in 0..m.I - 1 do 
                let yu = avg u responses
                b <- b + yu
            b / N
        else
            for u in 0..m.I - 1 do
                let yu = avg u responses
                b <- b + (sgn u (n - 1) m) * yu
            b / N

    let bij (m:Matrix<byte>) (responses:Matrix2) (n0:int) (n1:int) =
        let N = float m.I
        let mutable b = 0.
        
        for u in 0..m.I - 1 do
            let yu = avg u responses
            b <- b + (sgn u (n0 - 1) m) * (sgn u (n1 - 1) m) * yu
        b / N
        
        
    /// y is responses of the design points experiments
    let polynomialLinear (op_matrix:Matrix<'T>) (y:Matrix2) (deg:int) =
        let reactions = C op_matrix.J deg
        let factors = Array.zeroCreate<float> (op_matrix.J + reactions + 1) // + 1 == b0
        let entries = Array.zeroCreate<byte> (op_matrix.Entries.Length)
        let dmatrix = DesignMatrix.createDesignMatrix(op_matrix)

        let mutable idx = 0
        while idx <= op_matrix.J do factors[idx] <- bi dmatrix y idx; idx <- idx + 1

        for n0 in 0..dmatrix.J - 1 do
            for n1 in 0..dmatrix.J - 1 do
                if n0 = n1 then ()
                if idx < factors.Length then
                    factors[idx] <- bij dmatrix y (n0 + 1) (n1 + 1)
                idx <- idx + 1
        factors
