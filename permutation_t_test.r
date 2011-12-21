#!/usr/bin/env Rscript

# Perform t-test with permutation bootstraps to get the t-score distribution.

# Original written by George Casella
# Modified by Austin G. Davis-Richardson

args <- commandArgs(TRUE)
input = args[1]

data <- read.table(input, sep = "\t", header = T)

# case/control
pop_b_index = 22 # because R is fucking stupid.
samples = 45
bootstraps = 5000

for (i in 2:length(data)) {
  F1 = data[1:pop_b_index, i]
  F2 = data[pop_b_index+1:pop_b_index, i]
  F = data[1:samples, i]

  Tperm = array(0, dim = c(1,bootstraps))

  for(j in 1:bootstraps) {
    temp = sample(F)
  	tout = t.test(temp[1:pop_b_index],temp[pop_b_index + 1:samples])
  	Tperm[j] = abs(tout[[1]])
  }

  tcalc = t.test(F1,F2)
  tcalc = abs(tcalc[[1]])

  m = mean(Tperm > tcalc)
  
  print(i)
  print(m)
}