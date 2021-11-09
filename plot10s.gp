set terminal pngcairo background rgb 'black'
set xlabel 'Viscosity (Pa-s)' tc rgb 'white'
set ylabel 'Logarithmic Permeability (m^2)' tc rgb 'white'
set border lc rgb 'white'
set key tc rgb 'white'

set linetype 1 lc rgb 'white'
set output 'plot10s.png'
set pm3d map
load "/Users/jun/lib/ScientificColourMaps7/roma/roma.pal"
set palette negative
set xrange [0.1:10.1]
set yrange [-4:-1]
splot '10s.out' u 1:2:3