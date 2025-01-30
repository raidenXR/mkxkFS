namespace MKXK.DOE
open System
open System.Numerics
open System.Globalization
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type IMatrix =
    abstract member I: int
    abstract member J: int

type Matrix<'T when 'T: equality>(I:int, J:int, entries:array<'T>) =

    new(I:int, J:int) = Matrix<'T>(I, J, Array.zeroCreate<'T>(I * J))

    interface IMatrix with
        member this.I = I
        member this.J = J

    member x.I = I
    
    member x.J = J

    member x.N = I * J

    member x.Entries = entries
    
    member x.Item  
        with get(i:int, j:int) = entries[(i - 1) * J + (j - 1)]
        and set(i:int, j:int) value = entries[(i - 1) * J + (j - 1)] <- value

    member x.Item
        with get(idx:int) = entries[idx]
        and set(idx:int) value = entries[idx] <- value

    /// checks if two rows have the same entries
    member x.CheckEql(a:int, b:int) =
        let mutable res = false
        for j in 0..J - 1 do
            res <- res || (entries[a * J + j] =  entries[b * J + j])
        res

    member x.Values = entries

    static member zeroCreate (I:int, J:int) =
        let values = Array.zeroCreate (I * J)
        Matrix<'T>(I, J, values)


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


// module Factors =
//     let increment (factors:array<float>) (lb:array<float>) (ub:array<float>) =
//         for i = 1 to factors.Length - 1 do
//             factors[0] <- factors[0] + 1
//             if factors[i - 1] > ub[i - 1] then
//                 factors[i - 1] <- lb[i - 1]
//                 factors[i] <- factorsi] + 1

module Fib =
    let fib3 n = 
        match n with 
        | 0 -> 0
        | n -> 
            let mutable last = 0
            let mutable next = 1
            for i in 1 .. (n - 1) do
                let temp = last + next
                last <- next
                next <- temp
            next

type DesignMatrix =
    | Random of Matrix<byte>
    | Latin of Matrix<byte>
    | GreacoLatin of Matrix<uint16>
    | DesignMatrix of Matrix<float>    

    override x.ToString() =
        match x with
        | Random m -> 
            let sb = System.Text.StringBuilder(1024)
            for i in 1..m.I do
                ignore (sb.Append $"[{i}]: ")
                for j in 1..m.J do
                    let c = if m[i,j] = 0uy then '-' else '+'
                    ignore (sb.Append (c.ToString().PadLeft(4)))
                ignore (sb.AppendLine "")
            string sb
        | Latin m -> 
            if m.I <> m.J then failwith "must be a N x N matrix - square"
            let sb = System.Text.StringBuilder(1024)
            for i in 1..m.I do
                ignore (sb.Append $"[{i}]: ")
                for j in 1..m.J do
                    let c = char (m[i,j]) + 'A'
                    ignore (sb.Append (c.ToString().PadLeft(4)))
                ignore (sb.AppendLine "")
            string sb                    
        | GreacoLatin m -> 
            if m.I <> m.J then failwith "must be a N x N matrix - square"
            let sb = System.Text.StringBuilder()
            for i in 1..m.I do
                ignore (sb.Append $"[{i}]: ")
                for j in 1..m.J do
                    let ptr0 = byte (m[i,j] >>> 8)
                    let ptr1 = byte (m[i,j] &&& 0x00ffus)
                    let A = char (ptr0) + 'A'
                    let B = ptr1
                    ignore (sb.Append ($"{A}{B}".PadLeft(5)))
                ignore (sb.AppendLine "")
            string sb            
        | DesignMatrix m ->
            let sb = System.Text.StringBuilder(1024)
            for i in 1..m.I do
                for j in 1..m.J do
                    let c = if m[i,j] > 0 then 1 else 0
                    ignore (sb.Append (c.ToString().PadLeft(4)))
                ignore (sb.AppendLine "")
            string sb


    static member private create<'T> (I:int) (J:int) = 
        match Marshal.SizeOf(typedefof<'T>) with
        | 1 -> 
            let buffer = Array.init<byte> (I * J) (fun i -> byte i)
            Random.Shared.Shuffle buffer
            Matrix<byte>(I, J, buffer) :> IMatrix
        | 2 -> 
            let buffer = Array.init<uint16> (I * J) (fun i -> uint16 i)
            Random.Shared.Shuffle buffer
            Matrix<uint16>(I, J, buffer) :> IMatrix
        | 4 -> 
            let buffer = Array.init<int32> (I * J) (fun i -> int32 i)
            Random.Shared.Shuffle buffer
            Matrix<int32>(I, J, buffer) :> IMatrix
        | 8 -> 
            let buffer = Array.init<float> (I * J) (fun i -> float i)
            Random.Shared.Shuffle buffer
            Matrix<float>(I, J, buffer) :> IMatrix
        | _ -> failwith "not appropriate 'T, it must be numeric"
        // let d = Matrix<byte>(Array.zeroCreate<byte>(m.I * m.J), m.I, m.J)
        // for j in 0..m.J - 1 do
        //     let mutable _max = m[0,j]
        //     for i in 0..m.I - 1 do 
        //         _max <- max _max m[i,j]
        //     for i in 0..m.I - 1 do 
        //         d[i,j] <- if _max = m[i,j] then 255uy else 0uy
        // d            

    /// create a random square design matrix
    static member random I J =
        let I_total = (pown J (J - 1)) - 1
        let m = Matrix<byte>(I_total, J)

        let values = Array.zeroCreate<byte> J

        for i in 0..I_total - 1 do
            for j in 1..J - 1 do
                if values[j - 1] > 1uy then
                    values[j - 1] <- 0uy
                    values[j] <- values[j] + 1uy                    
            System.Buffer.BlockCopy(values, 0, m.Entries, i * J, J)
            values[0] <- values[0] + 1uy
        Random.Shared.Shuffle(m.Entries)
        Random (Matrix<byte>(I, J, m.Entries[0..I * J - 1]))
        

    /// create a Latin square design matrix
    static member latin M =
        let I = M
        let J = M
        let m = Matrix<byte>(I, J)
        for i in 1..I do
            for j in 1..J do
                let a = if i + j > J + 1 then i + j - J else i + j 
                m[i,j] <- byte (a - 2)
        Latin m

    /// create a Greaco-Latin square design matrix
    static member greacoLatin M =
        let I = M
        let J = M
        let m = Matrix<uint16>(I, J)
        let v = [|for i in 1..J -> i|]
        for i in 1..I do
            Random.Shared.Shuffle v
            for j in 1..J do
                let a = if i + j > J + 1 then i + j - J else i + j 
                let b = v[j - 1]
                m[i,j] <- uint16 (((a - 2) <<< 8) ||| b)        
        GreacoLatin m

    static member design (m:Matrix<'T>) =
        let d = Matrix<byte>(m.I, m.J, Array.zeroCreate<byte>(m.I * m.J))
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
