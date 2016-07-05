# FUNCTIONS #
#-------------------------------------------------------------------------------------#
collapse_rank = function(row) {
  # given a row that is of even length and collapse it such that
  #  0  x  0 =  0
  #  0  x  1 =  1
  #  0  x -1 = -1
  #  1  x  1 =  1
  #  1  x -1 =  1
  # -1  x -1 = -1
  
  r_row = row
  
  # hacky way to do this fast
  # replace all the 1 by 3, sum the rows
  # and if the result is >1 set to 1, and if < -1 then set to -1
  r_row[which(r_row == 1)] = 3
  
  new_width = length(r_row)/2
  r_row = r_row[1:new_width] + r_row[-(1:new_width)]
  r_row[which(r_row > 1)] = 1
  r_row[which(r_row < -1)] = -1
  
  return (r_row)
}

#------------------------------------------------------------------------------#

make_noise = function(row, d) {
  # %row% is an array with 0 and non-zero entries
  # %d% is the noise level - the number of bits set to 1 because of noise in the entire array
  r_row = row
  n = length(r_row)
  s = sum(r_row != 0)
  thold = d*n/(n-s)
  empty_idx = which(r_row == 0)
  noise = -as.numeric(runif(n-s) < thold)
  r_row[empty_idx] = noise
  return (r_row)
}

#-------------------------------------------------------------------------------#

max_rank = 10
s_0 = 0.05
d_0 = 0.1

# create MonteCarlo simulation
data_mat = runif(2^max_rank*10000)

# set the signal bits to 
data_mat = as.numeric(data_mat<=s_0)
data_mat = matrix(data_mat, nrow = 10000, ncol = 2^max_rank)


# make some noise, by setting the  
d = 0.1
init_data_mat = apply(data_mat, 1, function(x) make_noise(x, d))

# fold iterativeoy the matrix in 2
# aka increase the rank by 1 at each turn


mat = init_data_mat

s_vals = rep(NA, max_rank) # signal ratio through the folding
d_vals = rep(NA, max_rank) # noise ration through the folding
for (i in 1:max_rank) {
  mat = apply(mat, 1, collapse_rank)
  s[i] = mean(mat == 1)
  d[i] = mean(mat == -1)
  s
}
