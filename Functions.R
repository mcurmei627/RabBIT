# compute the expected number of cache misses
# based on the derived formula
expected_cm = function(n, s, d) {
  index = seq(1, n);
  prob_arr = 1-(1-s)^{512}*(1-(d-s)^index)^512
  return (sum(prob_arr))
}

# %row% is an array with 0 and non-zero entries
# %d% is the bit density  - the number of bits set to 1 because of noise and signals in the entire array
make_noise = function(row, d) {
  r_row = row
  n = length(r_row)
  s = sum(r_row != 0)/n
  thold = d-s
  empty_idx = which(r_row == 0)
  noise = as.numeric(runif(length(empty_idx)) < thold)
  r_row[empty_idx] = noise
  return (r_row)
}

# term table per single posting
make_term_table = function(K, N, s, d){
  # K - number of rows
  # N - number of documents
  # s - signal (local signal)
  # d - bit density (signal and noise)
  table = matrix(0, nrow=K, ncol=N)

  # position of the match documents
  sig = which(runif(N) < s)

  # add signal columns
  table[,sig] = 1

  # add noise bits
  table = apply(table, 1, function(x) make_noise(x, D)) #?? FIX for some weird reason I need to transpose
  table = t(table)

  return(table)
}

# returns an array with the number of misses per cache line
count_cache_misses = function(table) {

  # an array to keep track of the cache misses
  N = ncol(table)
  cache_misses = rep(NA, N/512)

  for (i in 1:length(cache_misses)) {
    l_idx = (i-1)*512 + 1
    r_idx = i*512
    accumulator = table[1,l_idx:r_idx] # initially the accumulator is the first rows
    for (j in 1:nrow(table) ) {
      cache_line =  table[j,l_idx:r_idx]
      accumulator = bitwAnd(accumulator, cache_line)
      if (sum(accumulator)==0) {
        cache_misses[i] = j
        break
      }
      if (j == nrow(table)) {
        cache_misses[i] =j
      }
    }
  }
  return(cache_misses)
}

# given a term table returns the ratio between nr of cache misses
# when using linear vs random (multiple terms)
linear_vs_random = function(table) {

  cache_misses1 = mean(count_cache_misses(table)) # <- linear procedure

  K = nrow(table)
  r_order = sample(1:K)
  r_table = table[r_order,] # <- randomize the table
  cache_misses2 = mean(count_cache_misses(r_table)) # <- random procedure

  return(cache_misses1/cache_misses2)
}
