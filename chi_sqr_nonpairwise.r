#!/usr/bin/env rscript

# Nabanita's Chi-Square (unpaired) script (OF DOOM!)
# Lovingly re-arranged and made not-sucky by Austin & Lexi.

# To get the p-value corresponding to each O.T.U by the method of Fisher's combining.

# USAGE:
# ./chisqr_nonpaired.r input output

# SAMPLE INPUT:
#
# sample.csv:
# ,a1,a2,a3,a4,a5,b1,b2,b3,b4,b5
# A,10,9,13,15,12,100,120,114,109,127
# B,12,11,19,5,0,20,11,14,7,9
# C,100,98,109,115,125,10,18,15,12,14
#
# where a1,... b5 are treatments (samples)
# and A..C are OTUs
# Numbers are reads per treatment:otu

# NOTE:
# you must edit the last line to suit your data:

# pvalue_notpairwise takes an argument called nob
# nob is a matrix of how you want the data to be split
# Say you have data with 10 samples and you want to compare half to half (case/control)

# Then you would use nob=c(5,5)
# Say you have data with 10 samples and you want to compare 5 groups each with two samples
# Then you would use nob=c(2,2,2,2,2)


# get args
args   <- commandArgs(TRUE)
input  = args[1]
output.file <- args[2]

# open output
data        <- read.csv(input,header=T)

pvalue_notpairwise <- function(data, nob, fdrcutoff, output.file) {


  O.T.U. <- as.character(data[ ,1])       # List of OTU names
  n      <- length(O.T.U.)                # Number of OTUs
  A      <- data[ , -1]                   # Data w/o OTU names
  m      <- length(nob)                   # Length of nob (what is nob?)
  B      <- matrix(0, nrow = n, ncol = m) # Some n*m matrix of 0
  tot    <- matrix(0, nrow=1,ncol=m)      # total of B?
  boot   <- 50000                         # Number of bootstraps
  
  z      <- choose(m,2)                   # m choose 2
  
  pvalue_pairs <- matrix(0, nrow=n, ncol = z) # Pvalue for each pair
  number_table <- matrix(0, nrow=n, ncol = 1)	# Another mysterious matrix?
  pvalue <- matrix(0, nrow=n, ncol = 1)       # Another mysterious matrix?

  # B   = Sum of values for 1st and 2nd section of dataset
  # tot = sum of B by column (total reads for each dataset)
  
  # for i in length(otus)
  for (i in 1: n) {
      # for j in length(nob)
      for (j in 2: m) {
          B[i, 1] <- sum(A[i, 1: nob[1]])
          B[i, j] <- sum(A[i, nob[j - 1] + 1: nob[j]])
          tot[, 1] <- sum(B[, 1])
          tot[, j] <- sum(B[, j])
      }
  }
  
  # for k in length(otus)
  for (k in 1:n) {    
    # for i in [length(nob) -1]
    for (i in 1: (m - 1)) {
      # for j in 
        for (j in (i + 1) : m) {
          # WHAT THE FUCK
          x <- matrix(c(B[k, i], B[k, j], (tot[i] - B[k, i]), (tot[j] - B[k, j])), nrow = 2, ncol = 2, byrow = T )
        }
        
        for (l in 1: z) {
          if (B[k, i] + B[k, i] > 0) {
                if (B[k, i] > 25 && B[k, i] > 25) {
                    pvalue_pairs[k, l] = chisq.test(x) $p.value
                }
                else {
                    pvalue_pairs[k, l] = chisq.test(x, simulate.p.value = TRUE, B = boot)$p.value
                }
            }
            else {
                pvalue_pairs[k, l] <- 10
            }
        }
    }

    # Combining all p values
  
    df1 <- 2*z-2*(sum(as.numeric(pvalue_pairs[k,] == 10)))
  
    # number of table
    number_table[k] = df1/2
    pvalue_temp <-0
    
    if (df1 > 2) {
      pvalue_temp <- -2*sum(log(pvalue_pairs[pvalue_pairs!=10]))
      pvalue[k]   <- pchisq(pvalue_temp,df1,lower.tail =F)
    }
  
    else {
      if (df1 == 0) {
        pvalue[k] <- 10
      }
      if (df1 == 2) {
        pvalue[k] <- pvalue_pairs
      }
    }
  }

 # START FDR CORRECTION

  No   <- seq(1,n,by=1)
  out1 <- data.frame(No,O.T.U.,pvalue=round(pvalue,4),  pvalue_pairs=round(pvalue_pairs,4), number_table=number_table)
  A    <- order(out1[,3]) 
  out1 <- out1[A,]
  s    <- length(pvalue[pvalue!=10])
  a    <- seq(1,s,by=1)
  b    <- array(10, dim=(n-s))

  fdr1    <- (a*fdrcutoff)/s
  qvalues <- c(fdr1,b)
  out     <- data.frame(out1, qvalues)
  for (i in 1: n) {
      for (j in 3: (3 + z + 1)) {
          if (out[i, j] == 10) {
              out[i, j] < -"NA"
          }
      }
  }

  write.table(out,output.file,append = FALSE, quote = TRUE, sep = ", ",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}

pvalue_notpairwise (data, nob=c(5,5),fdrcutoff=0.05,output.file)