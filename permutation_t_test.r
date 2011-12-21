#!/usr/bin/env Rscript

# Perform t-test with permutation bootstraps to get the t-score distribution.

# Original written by George Casella
# Modified by Austin G. Davis-Richardson

# USAGE:
# ./permutation_t_test.r input.csv
# you must edit pop_b_index, samples and bootstraps manually.

args <- commandArgs(TRUE)
input = args[1]

data <- read.table(input, sep = "\t", header = T)

pop_b_index = 22 # start of population B
samples     = 45 # total number of samples
bootstraps  = 1000000

for (i in 3:length(data)) {
  
  # Get data
  F1 = data[1:pop_b_index, i] # sample A
  F2 = data[pop_b_index+1:pop_b_index, i] # sample B
  F  = data[1:samples, i] # the whole thing

  # t-score for the permutation
  Tperm = array(0, dim = c(1,bootstraps))

  # Perform bootstraps, add t-score to array of t-scores
  for(j in 1:bootstraps) {
    temp = sample(F)
  	tout = t.test(temp[1:pop_b_index],temp[pop_b_index + 1:samples])
  	Tperm[j] = abs(tout[[1]])
  }

  # get the true t-score
  tcalc = t.test(F1,F2)
  tcalc = abs(tcalc[[1]])

  # average the number of permutated t-scores that are greater than the original t-score
  m = mean(Tperm > tcalc)
  
  # print the row number
  print(i)
  
  # print the corrected t-score
  print(m)
}
