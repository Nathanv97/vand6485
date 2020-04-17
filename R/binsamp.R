#'Binomial Sampling fucntion
#'
#'takes number of times to run the sample, iter, and samples n times each at p probability of success.
#'
#'@param iter Number
#'@param n Number
#'@param p Number
#'
#'
#'@examples
#'binsamp(1000, 10, 0.5)
#'
#'@export
binsamp = function(iter, n, p){

  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)

  succ = c()
  for( i in 1:iter){

    sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))

    succ[i] = sum(sam.mat[, i])
  }

  succ.tab = table(factor(succ, levels = 0:n))

  barplot(succ.tab / (iter), col = rainbow(n + 1), main = "Binomial simulation", xlab = "Number of successes")
  succ.tab / iter
}
