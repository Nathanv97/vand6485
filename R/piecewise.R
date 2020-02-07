#'Piecewise regression
#'
#'takes the x value and plots a line until it hits the change point defined by xk,
#'then plots a different line to make a piecewise regression graph.
#'
#'@param x Number
#'@param xk Number
#'@param coef Numeric Vector
#'
#'@return Numeric
#'
#'@examples
#'curve(myf(x, xk, coef), add = TRUE, lwd = 2, col = "Blue")
#'
#'@export
piecewise <- function(x, xk, coef){
  pf = coef[1] + coef[2] * (x) + coef[3] * (x - xk) * (x - xk > 0)
  return(pf)
}
