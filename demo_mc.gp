set terminal pngcairo background rgb 'black'
set xlabel 'x-axis' tc rgb 'white'
set ylabel 'y-axis' tc rgb 'white'
set border lc rgb 'white'
set key tc rgb 'white'

set linetype 1 lc rgb 'white'
set output 'demo_mc.png'
load "/Users/jun/lib/ScientificColourMaps7/roma/roma.pal"
set palette negative
set xrange [0:1]
set yrange [0:1]
plot 'demo_mc.ext' u 1:2:3 w p pt 7 lc rgb 'gray40' notitle, 'demo_mc.int' u 1:2:3 w p pt 7 palette notitle