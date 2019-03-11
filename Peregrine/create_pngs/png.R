# Saves an image as a PNG
png(filename = "my.png")
ape::plot.phylo(ape::rcoal(4))
dev.off()
