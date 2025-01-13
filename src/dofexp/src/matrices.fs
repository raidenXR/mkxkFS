namespace Dofexp
open System
open System.Numerics
open System.Globalization
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


type Matrix<'T when 'T: equality>(entries:seq<'T>, I:int, J:int) =
    let entries = Array.ofSeq entries
    member x.I = I
    
    member x.J = J

    member x.N = I * J

    member x.Entries = entries
    
    member x.Item  
        with get(i:int, j:int) = entries[i * J + j]
        and set(i:int, j:int) value = entries[i * J + j] <- value

    member x.Item
        with get(idx:int) = entries[idx]
        and set(idx:int) value = entries[idx] <- value

    /// checks if two rows have the same entries
    member x.CheckEql(a:int, b:int) =
        let mutable res = false
        for j in 0..J - 1 do
            res <- res || (entries[a * J + j] =  entries[b * J + j])
        res


type Matrix2(I:int, J:int, entries:seq<float>) =
    let entries = 
        if Seq.length entries <> I * J then failwith "entries must have I * J length"
        else Array.ofSeq entries

    member x.I = I

    member x.J = J

    member x.N = I * J

    member x.Item
        with inline get(i:int, j:int) = entries[i * J + j]
        and inline set(i:int, j:int) value = entries[i * J + j] <- value

    member x.Item
        with inline get(idx:int) = entries[idx]
        and inline set(idx:int) value = entries[idx] <- value


type Matrix2 with
    static member ZZ() = ()

// type Matrix<byte> with
//     static member ZZ() = ()

type Matrix3(entries:seq<float>, I:int, J:int, K:int) =
    let entries = 
        if Seq.length entries <> I * J * K then failwith "entries must have I * J length"
        else Array.ofSeq entries

    member x.I = I

    member x.J = J

    member x.K = K

    member x.N = I * J * K

    member x.Item
        with get(i:int, j:int, k:int) = entries[i * J + j * K + k]
        and set(i:int, j:int, k:int) value = entries[i * J + j * K + k] <- value

    member x.Item
        with get(idx:int) = entries[idx]
        and set(idx:int) value = entries[idx] <- value



type Matrix4(entries:seq<float>, I:int, J:int, K:int, L:int) =
    let entries = 
        if Seq.length entries <> I * J * K * L then failwith "entries must have I * J length"
        else Array.ofSeq entries

    member x.I = I

    member x.J = J

    member x.K = K

    member x.L = L

    member x.N = I * J * K * L

    member x.Item
        with get(i:int, j:int, k:int, l:int) = entries[i * J + j * K + k * L + l]
        and set(i:int, j:int, k:int, l:int) value = entries[i * J + j * K + k * L + l] <- value

    member x.Item
        with get(idx:int) = entries[idx]
        and set(idx:int) value = entries[idx] <- value



type DesignMatrix =
    | Random of Matrix<byte>
    | Latin of Matrix<byte>
    | GreacoLatin of Matrix<uint16>
    | DesignMatrix of Matrix<byte>

    override x.ToString() =
        match x with
        | Random m -> 
            let sb = System.Text.StringBuilder(1024)
            for i in 0..m.I - 1 do
                for j in 0..m.J - 1 do
                    let c = if m[i,j] < 128uy then '-' else '+'
                    ignore (sb.Append (c.ToString().PadLeft(4)))
                ignore (sb.AppendLine "")
            string sb
        | Latin m -> 
            let sb = System.Text.StringBuilder(1024)
            for i in 0..m.I - 1 do
                for j in 0..m.J - 1 do
                    let dx = byte (255 / m.J)
                    let mutable a = dx
                    let mutable A = byte 'A'
                    while a < m[i,j] do A <- A + 1uy; a <- a + dx
                    let cfmt = char A
                    ignore (sb.Append (cfmt.ToString().PadLeft(4)))
                ignore (sb.AppendLine "")
            string sb                    
        | GreacoLatin m -> 
            if m.I <> m.J then failwith "must be a N x N matrix - square"
            let sb = System.Text.StringBuilder()
            for i in 0..m.I - 1 do
                for j in 0..m.J - 1 do
                    let ptr0 = byte (m[i,j] &&& 0x00ffus)
                    let ptr1 = byte (m[i,j] >>> 8)
                    let dx = byte (255 / m.J)
                    let mutable a = dx
                    let mutable b = dx
                    let mutable A = byte 'A'
                    let mutable B = 1uy
                    while a < ptr0 do A <- A + 1uy; a <- a + dx 
                    while b < ptr1 do B <- B + 1uy; b <- b + dx 
                    let Afmt = char A
                    ignore (sb.Append ($"{Afmt}{B}".PadLeft(5)))
                    // ignore (sb.Append ($"{ptr0}-{ptr1}  "))
                ignore (sb.AppendLine "")
            string sb            
        | DesignMatrix m ->
            let sb = System.Text.StringBuilder(1024)
            for i in 0..m.I - 1 do
                for j in 0..m.J - 1 do
                    let c = if m[i,j] > 0uy then 1 else 0
                    ignore (sb.Append (c.ToString().PadLeft(4)))
                ignore (sb.AppendLine "")
            string sb


    /// create a random square design matrix
    static member createRandom I J =
        let r = System.Random()
        let m = Matrix<byte>(Array.zeroCreate<byte>(I * J), I, J)
        r.NextBytes(m.Entries)
        Random m

    /// create a Latin square design matrix
    static member createLatin I J =
        if I <> J then failwith "must be a N x N matrix - square"
        let r = System.Random()
        let m = Matrix<byte>(Array.zeroCreate<byte>(I * J), I, J)
        r.NextBytes(m.Entries)
        Latin m

    /// create a Greaco-Latin square design matrix
    static member createGreacoLatin I J =
        if I <> J then failwith "must be a N x N matrix - square"
        let r = System.Random()
        let m = Matrix<uint16>(Array.zeroCreate<uint16>(I * J), I, J)        
        let s = Span(m.Entries) 
        r.NextBytes (MemoryMarshal.AsBytes s)
        GreacoLatin m

    static member createDesignMatrix (m:Matrix<'T>) = 
        let d = Matrix<byte>(Array.zeroCreate<byte>(m.I * m.J), m.I, m.J)
        for j in 0..m.J - 1 do
            let mutable _max = m[0,j]
            for i in 0..m.I - 1 do 
                _max <- max _max m[i,j]
            for i in 0..m.I - 1 do 
                d[i,j] <- if _max = m[i,j] then 255uy else 0uy
        d            
        

module Matrix2 =
    let sum (m:Matrix2) =
        let mutable acc = 0.
        for x in 0..m.N - 1 do acc <- acc + m[x]
        acc

    let sumJ (j:int) (m:Matrix2) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do acc <- acc + m[i, j]
        acc

    let sumI (i:int) (m:Matrix2) =
        let mutable acc = 0.
        for j in 0..m.J - 1 do acc <- acc + m[i, j]
        acc



module Matrix3 =
    let sum (m:Matrix3) =
        let mutable acc = 0.
        for x in 0..m.N - 1 do acc <- acc + m[x]
        acc

    let sumJ (j:int) (m:Matrix3) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for k in 0..m.K - 1 do acc <- acc + m[i,j,k]
        acc

    let sumI (i:int) (m:Matrix3) =
        let mutable acc = 0.
        for j in 0..m.J - 1 do
            for k in 0..m.K - 1 do acc <- acc + m[i,j,k]
        acc

    let sumK (k:int) (m:Matrix3) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do acc <- acc + m[i,j,k]
        acc

    let sumIJ (i:int) (j:int) (m:Matrix3) =
        let mutable acc = 0.
        for k in 0..m.K - 1 do acc <- acc + m[i,j,k]
        acc

    let sumJK (j:int) (k:int) (m:Matrix3) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do acc <- acc + m[i,j,k]
        acc

    let sumIK (i:int) (k:int) (m:Matrix3) =
        let mutable acc = 0.
        for j in 0..m.J - 1 do acc <- acc + m[i,j,k]
        acc



module Matrix4 =
    let sum (m:Matrix4) =
        let mutable acc = 0.
        for x in 0..m.N - 1 do acc <- acc + m[x]
        acc

    let sumJ (j:int) (m:Matrix4) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for k in 0..m.K - 1 do
                for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]
        acc

    let sumI (i:int) (m:Matrix4) =
        let mutable acc = 0.
        for j in 0..m.J - 1 do
            for k in 0..m.K - 1 do
                for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]
        acc

    let sumK (k:int) (m:Matrix4) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do
                for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]
        acc

    let sumL (l:int) (m:Matrix4) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for j in 0..m.J - 1 do
                for k in 0..m.K - 1 do acc <- acc + m[i,j,k,l]
        acc

    let sumIJ i j (m:Matrix4) =
        let mutable acc = 0.
        for k in 0..m.K - 1 do
            for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]    
        acc

    let sumJK j k (m:Matrix4) =
        let mutable acc = 0.
        for i in 0..m.I - 1 do
            for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]
        acc

    let sumIK i k (m:Matrix4) =
        let mutable acc = 0.
        for j in 0..m.J - 1 do
            for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]
        acc

    let sumIJK i j k (m:Matrix4) =
        let mutable acc = 0.
        for l in 0..m.L - 1 do acc <- acc + m[i,j,k,l]
        acc
