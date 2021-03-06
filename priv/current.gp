
## Use terminal definition as you like (via -t option)
# set terminal dumb                # character terminal
# set terminal dumb 79 49 enhanced # character terminal portrait
# set terminal x11                 # X-Window
# set terminal x11 persist         # X-Window, remain alive after gnuplot exits
# set terminal aqua                # AquaTerm


## title, key and axis
set title "Throughput ops/sec"
set autoscale
set yrange [0:]
set grid
set xlabel "Elapsed [sec]"
set ylabel "ops/sec"
set key inside bottom

## data file
set datafile separator ','

## plot
plot \
    "results/current/summary.csv" using 1:($3/$2) with \
        linespoints pointsize 2 linewidth 1 \
        title "total" \
        ,\
    1/0 notitle # dummy
