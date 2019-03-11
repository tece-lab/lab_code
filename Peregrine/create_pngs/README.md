# `create_png`

Creating a PNG (or any other image file) on Peregrine is harder then
expected.

Consider this R code:

```
png(filename = "my.png")
ape::plot.phylo(ape::rcoal(4))
dev.off()
```

Running this, as a file named `png.R`, on Peregrine:

```
Rscript png.R 
```

Results in:

```
Error in .External2(C_X11, paste0("png::", filename), g$width, g$height,  : 
  unable to start device PNG
Calls: png
In addition: Warning message:
In png(filename = "my.png") : unable to open connection to X11 display ''
Execution halted
```

X11 (or: the X Window System, or simply X) is a windowing system 
for bitmap displays, common on Unix-like operating systems, but not
loaded by default on Peregrine.

Solution: use the bash script `run_r_script`:

```
sbatch run_r_script png.R
```

`run_r_script` uses `xvfb-run` to use the X11 Virtual Frame Buffer.
