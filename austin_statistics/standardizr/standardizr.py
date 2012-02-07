#!/usr/bin/env python

import sys
from math import sqrt, log

## Load Data
filename = sys.argv[1]
with open(filename) as handle:
    header = handle.next().split()
    
    data = {}
    samples = header
    for sample in samples:
        data[sample] = {}
    
    for line in handle:
        line = line.split()
        row_header = line[0]
        row_numbers = [ int(i) for i in line[1:] ]
        for s, n in zip(samples, row_numbers):
            data[s][row_header] = n

## Calculations

def y(x):
    return log(x+1, 2)
    
def z(x, mean, stdev):
    return (y(x) - mean)/stdev

for sample in data:
    numbers = data[sample].values()
    n = len(numbers)
    
    # Mean for Y of Column
    mean = sum(y(x) for x in numbers)/float(n)
    
    # SAMPLE STANDARD DEVATION
    stdev = sqrt( sum( (y(x) - mean )**2 for x in numbers ) /(n-1))

    for row_header in data[sample]:
        data[sample][row_header] = z(data[sample][row_header], mean, stdev)
        
print '\t',
print '\t'.join(header).strip()

keys = data.values()[0].keys()

for key in keys:
    print "%s\t" % key,

    print "\t".join(str(data[sample][key]) for sample in data)