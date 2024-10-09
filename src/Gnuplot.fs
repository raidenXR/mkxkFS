namespace MKXK

module Plotting =
    open System
    open System.Text
    open System.IO
    open System.Diagnostics
    open System.Runtime.InteropServices

    type Gnuplot(keep_log: bool, keep_process_alive: bool, path: option<string>) = 
        let sb = StringBuilder(1024)        
        let mutable running = false
        let mutable process': Process = null

        do
            match path with | Some p -> sb.AppendLine ("set output '" + p + "'") |> ignore | None -> ()

        
        /// close the process and dispose resources
        let pclose () =
            process'.StandardInput.Close()
            process'.WaitForExit()
            process'.Close()
            process'.Dispose()            
            

        member _.writeln (str: string) = 
            match running with 
            | true -> process'.StandardInput.WriteLine str
            | false -> sb.AppendLine str |> ignore

        member _.Running = running

        member _.run =
            let pstr = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then "gnuplot.exe"
                       elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then "gnuplot"
                       elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then "gnuplot"
                       else failwith "not supported OS"
            let info = ProcessStartInfo(FileName = pstr, UseShellExecute = false, RedirectStandardInput = true)
            process' <- new Process(StartInfo = info)
            process'.Start() |> ignore
            process'.StandardInput.WriteLine (string sb)            

            if not keep_process_alive then                     
                pclose ()

            match keep_log, path with
                | true, Some p -> 
                    use fs = File.CreateText (p[0..p.Length - (4 + 1)] + ".gnu")
                    fs.WriteLine (string sb)
                | _, _ -> ()


        member _.close =
            if keep_process_alive then 
                match keep_log, path with
                    | true, Some p -> 
                        use fs = File.CreateText (p[0..p.Length - (4 + 1)] + ".gnu")
                        fs.WriteLine (string sb)  // FIX this to include all the text written in stdin
                    | _, _ -> ()

                pclose ()




    let (|>>) (plt:Gnuplot) (str:string) = plt.writeln str; plt


    module Gnuplot =
        let run (gnu:Gnuplot) = gnu.run; gnu

        let close (gnu:Gnuplot) = gnu.close

        let datablockX (x:array<float>) (tag:string) (gnu:Gnuplot) = 
            gnu.writeln $"\n${tag} << EOD"
            let l = Array.length x
            for i in 0..l-1 do gnu.writeln $"{x[i]}"
            gnu.writeln "EOD\n"
            gnu


        let datablockXY (x:array<float>) (y:array<float>) (tag:string) (gnu:Gnuplot) =
            gnu.writeln $"\n${tag} << EOD"
            let l = Array.length x
            for i in 0..l-1 do gnu.writeln $"{x[i]}  {y[i]}"
            gnu.writeln "EOD\n"
            gnu
        

        let datablockXYZ (x:array<float>) (y:array<float>) (z:array<float>) (tag:string) (gnu:Gnuplot) =
            gnu.writeln $"\n${tag} << EOD"
            let l = Array.length x
            for i in 0..l-1 do gnu.writeln $"{x[i]}  {y[i]}  {z[i]}"
            gnu.writeln "EOD\n"
            gnu

            
        let datablockXYZW (x:array<float>) (y:array<float>) (z:array<float>) (w:array<float>) (tag:string) (gnu:Gnuplot) =
            gnu.writeln $"\n${tag} << EOD"
            let l = Array.length x
            for i in 0..l-1 do gnu.writeln $"{x[i]}  {y[i]}  {z[i]}  {w[i]}"
            gnu.writeln "EOD\n"
            gnu
