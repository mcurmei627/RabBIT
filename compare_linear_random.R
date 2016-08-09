setwd("C:\\Users\\t-micurm\\Documents\\RabBIT");
source("Functions.R")

# this script tests the performnce in terms of cache misses of two strategies.

# strategy number 1, which we believe to be currently in practice
# for multi-term queries we first 'accumulate the rows for each term in order'

# strategy number 2, which I think is going to have a much better performance
# on the multi term queries the rows are returned in some arbitrary order

# In order to compare the performance we will use MC simulations
# The limitation of this approach is the fact that currently this approach
# does not take in account the fact that in multi-term queries the terms are
# correlated - for example natural occuring queries have terms that are statistically
# likely to be both in the same document
# for now we assume that the term are independent

# GLOBAL VARIABLES
D = 0.1 # - overall bit density - includes noise, local signal and global signal
K1 = 30 # - assume the k's are equal for both words
K2 = 30

N = 512000 #- the number of documents (to be read in 1000 cache lines)

# create the K1 rows corresponding to the term matrix for term1
# create the K2 rows corresponding to the term matrix for term2
ttable1 = matrix(0, nrow=K1, ncol=N)
ttable2 = matrix(0, nrow=K2, ncol=N)


# first select the signal levels
s1 = 0.002
s2 = 0.001

# create the signal in the matrix
set.seed(1000) # for tractable results
sig1 = which(runif(N) < s1) # local signal
sig2 = which(runif(N) < s2)
sig = intersect(sig1, sig2) # global signal

# add signal columns
ttable1[,sig1] = 1
ttable2[,sig2] = 1

# add noise bits
table1 = apply(ttable1, 1, function(x) make_noise(x, D)) #?? FIX for some weird reason I need to transpose
table1 = t(table1)
table2 = apply(ttable2, 1, function(x) make_noise(x, D))
table2 = t(table2)

image(table2[, 1:200])

############################
# APPROACH 1 - linear intersections
table = rbind(table1, table2)


cache_misses1 = count_cache_misses(table)


# APPROACH 2: Random order of intersection
r_order = sample(1:(K1+K2))
r_table = table[r_order,]

cache_misses2 = count_cache_misses(r_table)


# -------------------------------------------------------------------------------------------------------------
# Rewriting the above
# -------------------------------------------------------------------------------------------------------------


source("Functions.R")

N = 512000
D = 0.1
Q = 100

S1 = 10^(-2)
S2 = 10^(-2)
S3 = 10^(-4)
S4 = 10^(-2)

K1 = ceiling((log(S1) - log(Q))/(log(D-S1)))
K2 = ceiling((log(S2) - log(Q))/(log(D-S2)))
K3 = ceiling((log(S3) - log(Q))/(log(D-S3)))
K4 = ceiling((log(S4) - log(Q))/(log(D-S4)))

ttable1 = make_term_table(K1, N, S1, D)
ttable2 = make_term_table(K2, N, S2, D)
ttable3 = make_term_table(K3, N, S3, D)
ttable4 = make_term_table(K4, N, S4, D)

ttable = rbind(ttable1, ttable2, ttable3, ttable4)

linear_vs_random(ttable, verbose = TRUE) # make this print nice statistics like number of cache misses and percentages

# we get something around 5%-20% improvement
# which is an overstatement of the randomization
# benefit, because search queries are usually correlated
# and this simulation assumes independence between query terms






