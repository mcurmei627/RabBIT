library(plotly) #library for fancy interactive plots

setwd("/Users/admin/src/RabBIT/");
source("Functions.R")

d = seq(0, 1, 0.001);
s = seq(0, 1, 0.001);

mat = matrix(NA, length(s), length(d))
n = 30
for (i in 1:length(s)) {
  for (j in 1:length(d)) {
    if (s[i] + d[j] <= 1) {
      mat[i, j] = expected_cm(n, s[i], d[j]);
    }
  }
}


plot_ly(x = s, y = d, z = mat, type = "surface")
