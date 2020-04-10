#'myci
#'
#'takes a sample and returns a 95% ci for the mu
#'
#'@param x Vector
#'
#'@examples
#'myci(x=rnorm(30,10,12))
#'
#'@export
myci=function(x){
  t=qt(0.975,length(x)-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(25)
  ci[2]=mean(x)+t*sd(x)/sqrt(25)
  ci
}
