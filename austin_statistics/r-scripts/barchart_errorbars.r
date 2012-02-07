# Some code to define what error bars are...
error.bar <- function(x, y, upper, lower=upper, length=0.1, ...){
  if (length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
  stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


# Loading data
your_data <- read.csv('data.csv', sep='\t', header=T)
your_data = as.matrix(your_data)

print(your_data)

# Calculating standard deviation, instead
# load your standard deviation from another file
errorbars <- read.csv('errorbars.csv', sep='\t', header=F)
errorbars = as.matrix(errorbars)

print(errorbars)
error_bars <- barplot(your_data, names.arg=1:5,ylim=c(0,15), axis.lty=1, xlab="Replicates", ylab="Value (arbitrary units)")

error.bar(barx,your_data, 1.96*error_bars/10)
