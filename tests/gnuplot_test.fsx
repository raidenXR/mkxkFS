#load "../src/Gnuplot.fs"

open MKXK
open Plotting
open System.IO

// if not (Directory.Exists("tests_outputs")) 
//     then Directory.CreateDirectory("tests_outputs") |> ignore
    
if not (Directory.Exists(Path.Combine("tests_output", "images"))) 
    then Directory.CreateDirectory(Path.Combine("tests_output", "images")) |> ignore

let r = System.Random()
// let rblk = [for i in 0..100 -> ]

let gnu0 = Gnuplot(false, false, Some "tests_output/images/image0.png")
gnu0 
|>> "set terminal png size 640,480"
// |>> "set output 'tests_output/images/image0.png'"
|> Gnuplot.datablockXY [|2.;4.;8.;9.|] [|0.;12.;34.;67.|] "points"
|>> "plot $points with linespoints"
|> Gnuplot.run
|> Gnuplot.close

// pause the thread to view the preocess before closing
// System.Threading.Thread.Sleep(10_000)

let gnu1 = Gnuplot(false, false, Some "tests_output/images/image1.png")
gnu1
|>> "set terminal pngcairo  transparent enhanced font 'arial,10' fontscale 1.0 size 600, 400" 
|>> "set dummy u, v"
|>> "unset key"
|>> "set wall y0  fc  rgb 'bisque'  fillstyle  transparent solid 0.50 border lt -1"
|>> "set wall x0  fc  rgb 'forest-green'  fillstyle  transparent solid 0.50 border lt -1"
|>> "set wall z0  fc  rgb 'slategrey'  fillstyle  transparent solid 0.50 border lt -1"
|>> "set parametric"
|>> "set view map scale 1"
|>> "set isosamples 75, 25"
|>> "set style data lines"
|>> "set xyplane relative 0"
|>> "set xtics border in scale 0.5,0.5 nomirror norotate  autojustify"
|>> "set xtics  norangelimit -4.00000,2 ,4.00000"
|>> "set ytics border in scale 0.5,0.5 nomirror norotate  autojustify"
|>> "set ytics  norangelimit -4.00000,2 ,4.00000"
|>> "set ztics border in scale 0.5,0.5 nomirror norotate  center"
|>> "set ztics  norangelimit 0.5"
|>> "set cbtics border in scale 0.5,0.5 nomirror norotate  autojustify"
|>> "set rtics border in scale 0.5,0.5 nomirror norotate  autojustify"
|>> "set title 'set view map'" 
|>> "set urange [ 0.00000 : 31.4159 ] noreverse nowriteback"
|>> "set vrange [ 0.00000 : 6.28319 ] noreverse nowriteback"
|>> "set xlabel 'X-axis'" 
|>> "set xrange [ -5.00000 : 5.00000 ] noreverse nowriteback"
|>> "set x2range [ * : * ] noreverse writeback"
|>> "set ylabel 'Y-axis'" 
|>> "set yrange [ -5.00000 : 5.00000 ] noreverse nowriteback"
|>> "set y2range [ * : * ] noreverse writeback"
|>> "set zlabel 'Z-axis'" 
|>> "set zrange [ * : * ] noreverse writeback"
|>> "set cbrange [ * : * ] noreverse writeback"
|>> "set rrange [ * : * ] noreverse writeback"
|>> "set pm3d implicit at s"
|>> "set pm3d depthorder" 
|>> "set pm3d interpolate 1,1 flush begin noftriangles border linecolor rgb 'black'  linewidth 0.500 dashtype solid corners2color mean"
|>> "set palette cubehelix start 0.5 cycles -1.5 saturation 1"
|>> "set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault"
|>> "unset colorbox"
|>> "NO_ANIMATION = 1"
|>> "splot (1-0.1*u*cos(v))*cos(u),(1-0.1*u*cos(v))*sin(u),0.1*(sin(v)+u/1.7-10) with pm3d"
|> Gnuplot.run


let gnu2 = Gnuplot(false, false, Some "tests_output/images/image2.png")
gnu2
|>> "set terminal pngcairo  transparent enhanced font 'arial,10' fontscale 1.0 size 600, 400" 
|>> "set label 1 'plot for [n=2:10] sin(x*n)/n' at graph 0.95, 0.92, 0 right norotate back nopoint"
|>> "set style data lines"
|>> "set title 'Iteration within plot command'" 
|>> "set xrange [ 0.00000 : 3.00000 ] noreverse nowriteback"
|>> "set x2range [ * : * ] noreverse writeback"
|>> "set yrange [ * : * ] noreverse writeback"
|>> "set y2range [ * : * ] noreverse writeback"
|>> "set zrange [ * : * ] noreverse writeback"
|>> "set cbrange [ * : * ] noreverse writeback"
|>> "set rrange [ * : * ] noreverse writeback"
|>> "set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault"
|>> "NO_ANIMATION = 1"
|>> "n = 10"
|>> "plot for [n=2:10] sin(x*n)/n notitle lw (13-n)/2"
|> Gnuplot.run

let gnu3 = Gnuplot(false, false, Some "tests_output/images/image3.png")
gnu3
|>> "set terminal pngcairo  transparent enhanced font 'arial,10' fontscale 1.0 size 600, 400" 
|>> "set border 4095 front lt black linewidth 1.000 dashtype solid"
|>> "set samples 25, 25"
|>> "set isosamples 20, 20"
|>> "set style data lines"
|>> "set xyplane relative 0"
|>> "set title 'pm3d at b (bottom)'" 
|>> "set xlabel 'x'" 
|>> "set xrange [ -15.0000 : 15.0000 ] noreverse nowriteback"
|>> "set x2range [ * : * ] noreverse writeback"
|>> "set ylabel 'y'" 
|>> "set yrange [ -15.0000 : 15.0000 ] noreverse nowriteback"
|>> "set y2range [ * : * ] noreverse writeback"
|>> "set zrange [ -0.250000 : 1.00000 ] noreverse nowriteback"
|>> "set cblabel 'colour gradient' "
|>> "set cbrange [ * : * ] noreverse writeback"
|>> "set rrange [ * : * ] noreverse writeback"
|>> "set pm3d implicit at b"
|>> "set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault"
|>> "NO_ANIMATION = 1"
|>> "splot sin(sqrt(x**2+y**2))/sqrt(x**2+y**2)"
|> Gnuplot.run    


let gnu4 = Gnuplot(false, false, Some "tests_output/images/image4.png")
gnu4
|>> "set terminal pngcairo  transparent enhanced font 'arial,10' fontscale 1.0 size 600, 400 "
|>> "set key at screen 1, 0.9 right top vertical Right noreverse enhanced autotitle nobox"
|>> "set style textbox  opaque margins  0.5,  0.5 fc  bgnd noborder linewidth  1.0"
|>> "set view 60, 30, 1, 1.1"
|>> "set samples 20, 20"
|>> "set isosamples 21, 21"
|>> "set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover"
|>> "set contour both"
|>> "set cntrlabel  format '%8.3g' font ',7' start 5 interval 20"
|>> "set cntrparam levels 10"
|>> "set style data lines"
|>> "set title 'contours on both base and surface' "
|>> "set xlabel 'X axis' "
|>> "set xrange [ * : * ] noreverse writeback"
|>> "set x2range [ * : * ] noreverse writeback"
|>> "set ylabel 'Y axis' "
|>> "set yrange [ * : * ] noreverse writeback"
|>> "set y2range [ * : * ] noreverse writeback"
|>> "set zlabel 'Z ' "
|>> "set zlabel  offset character 1, 0, 0 font '' textcolor lt -1 norotate"
|>> "set zrange [ * : * ] noreverse writeback"
|>> "set cbrange [ * : * ] noreverse writeback"
|>> "set rrange [ * : * ] noreverse writeback"
|>> "set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault"
|>> "NO_ANIMATION = 1"
|>> "splot x**2-y**2 with lines, x**2-y**2 with labels boxed notitle"
|> Gnuplot.run
