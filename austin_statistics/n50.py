#!/usr/bin/env python

# calculate N50 from a fasta file

# N50 = contig length so that half of the contigs are longer and half of the
# contigs are shorter

# Also, maybe calculate a histograph of the reads.

# I plan on using this to optimize genome assemblies.

import sys
import os

sequence, lengths = [], []
with open(sys.argv[1]) as handle:
  for line in handle:
    if line.startswith('>'):
      lengths.append(len(''.join(sequence)))
      sequence = []
    else:
      sequence += line.strip()
      
      
n50 = sorted(lengths)[len(lengths)/2] # approximately?

print "N50 = %s" % n50

with open('all_lengths.txt', 'w') as handle:
  handle.write('\n'.join(str(i) for i in lengths))
  
histogram_calculator = """
data <- read.csv('all_lengths.txt', header=F)
hist(as.matrix(data), main='N50 = %s', xlab='length', ylab='frequency')
abline(v=%s, col='red')
""" % (n50, n50)

with open('histogram.r', 'w') as handle:
  handle.write(histogram_calculator)
  
os.system('r --slave < histogram.r')
os.system('rm histogram.r')
os.system('open Rplots.pdf')