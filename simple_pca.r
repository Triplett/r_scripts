#!/usr/bin/env Rscript --vanilla

# Jennie R. Fagen
# University of Florida

args <- commandArgs(TRUE)
data_table = args[1]


mydata = read.table(file=data_table, header=TRUE, row.names=1, sep="\t")

mydata.pca = prcomp (mydata, retx=TRUE, center=TRUE, scale.=TRUE)

mydata.pca

sd = mydata.pca$sdev
loadings = mydata.pca$rotation
rownames(loadings) = colnames(mydata)
scores = mydata.pca$x

quartz(height=7, width=7) > plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2",
  type="n", xlim=c(min(scores[,1:2]), max(scores[,1:2])),
  ylim=c(min(scores[,1:2]), max(scores[,1:2])))

arrows(0,0,loadings[,1]*20,loadings[,2]*20, length=0.1,
angle=20, col="red") 

# note that this scaling factor *n may need to be changed, # depending on the data set

text(loadings[,1]*20*1.2,loadings[,2]*20*1.2, rownames(loadings), col="red", cex=0.7)
# 1.2 scaling insures that labels are plotted just beyond # the arrows

text(scores[,1],scores[,2], rownames(scores), col="blue", cex=0.7)
