#!/usr/bin/env Rscript --vanilla

# Jennie R. Fagen
# University of Florida


USAGE = "HOW TO RUN

lf=<length_factor> tf=<text_factor> lim=<x/y limit> ./simple_pca2.r file_you_want_to_pca.csv

Where <length_factor>, etc.. is a number, ie 5. Those options are optional and have default values (10)

File should be comma-separated.

The output will always be myplot.pdf\n\n
"
# Get arguments
args <- commandArgs(TRUE)
data_table = args[1]
plot_name  = "myplot.pdf"

# Get environmental variables if provided
length_factor = as.integer(Sys.getenv('lf'), 10) # loadings length scale factor
text_scale    = as.integer(Sys.getenv('tf'), 10)    # text scale factor
lim           = as.integer(Sys.getenv('lim'), 10)          # x/y limit

# Read data
mydata = read.table(
  file      = data_table,
  header    = TRUE,
  row.names = 1,
  sep       = ","
)

# Compute le PCA
mydata.pca = prcomp(
  mydata,
  retx   = TRUE,
  center = TRUE,
  scale. = TRUE
)

# standard deviation, loadings and scores
sd       = mydata.pca$sdev
loadings = mydata.pca$rotation
scores   = mydata.pca$x

rownames(loadings) = colnames(mydata)

# PLOT PCA
quartz(height=7, width=7) > 
  plot(scores[,1], scores[,2],
       xlab="PCA 1", ylab="PCA 2",
       type="n",
       xlim=c(-lim,lim),
       ylim=c(-lim,lim)
      )

# PLOT LOADINGS
arrows(
  0,
  0,
  loadings[,1]*length_factor,
  loadings[,2]*length_factor,
  length = 0.1,
  angle = 20,
  col = "red"
) 

# Plot loading labels
text(
  loadings[,1]*length_factor*1.2,
  loadings[,2]*length_factor*1.2,
  rownames(loadings),
  col="red",
  cex=0.7
)

# Plot variables labels
text(
  scores[,1]*text_scale,
  scores[,2]*text_scale,
  rownames(scores),
  col="blue",
  cex=0.7
)

# SAVE PDF
dev.copy(pdf, plotname)
dev.off()
