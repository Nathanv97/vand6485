#'Plot Normal Curve and Probability
#'
#'takes mu and sigma and plots the Normal curve with a shaded area between the curve and x axis from
#'negative infinity to x=a. It also calculates the probability, P(X<=a)
#'
#'@param mu Number
#'@param sigma Number
#'@param a Number
#'
#'
#'@examples
#'myncurve(3, 5, 4)
#'
#'@export
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma, a, length=1000)
  ycurve=dnorm(xcurve,mean=mu, sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  prob
}
