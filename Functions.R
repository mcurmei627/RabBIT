expected_cm = function(n, s, d) {
  index = seq(1, n);
  prob_arr = 1-(1-s)*(1-d^index)^512
  return (sum(prob_arr))
}