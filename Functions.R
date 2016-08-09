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
linear_vs_random = function(table, verbose = getOption(TRUE, FALSE)) {

  # returns an array with the number of misses per cache line
  cache_misses1 = count_cache_misses(table) # <- linear procedure

  K = nrow(table)
  r_order = sample(1:K)
  r_table = table[r_order,] # <- randomize the table
  cache_misses2 = count_cache_misses(r_table) # <- random procedure

  ratio = mean(cache_misses1)/mean(cache_misses2)
  if (verbose == FALSE) {
    return(ratio)
  }
  print("The linear approach has on average ")
  print(ratio)
  print(" times more cache misses than the randomized approach")
  p1 = qplot(cache_misses1, geom="histogram",
        main = "Linear intersections",
        xlab = "Number of cache misses per cache line",
        fill = I("blue"),
        col = I("red"))

  p2 = qplot(cache_misses2, geom="histogram",
        main = "Random intersections",
        xlab = "Number of cache misses per cache line",
        fill = I("blue"),
        col = I("red"))
  multiplot(p1, p2, cols = 1)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

