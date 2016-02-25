#' Extract pD and DIC
#' 
#' A convenience function for extracting the effective number of parameters (pD)
#' and the Deviance Information Criterion (DIC) from an \code{rjags} object.
#' 
#' @param x An \code{rjags} or \code{rjags.parallel} object.
#' @return A list with two elements: \code{pD}, and \code{DIC}. If these were
#'   not computed for model \code{x}, elements will have value \code{NA}.
#' @seealso \code{\link{jagsresults}} for subsetting summaries of \code{rjags}, 
#'   \code{rjags.parallel}, and \code{mcmc.list} objects.
#' @export
#' @examples
#' ## Data
#' N <- 100
#' temp <- runif(N)
#' rain <- runif(N)
#' wind <- runif(N)
#' a <- 0.13
#' beta.temp <- 1.3
#' beta.rain <- 0.86
#' beta.wind <- -0.44
#' sd <- 0.16
#' y <- rnorm(N, a + beta.temp*temp + beta.rain*rain + beta.wind*wind, sd)
#' dat <- list(N=N, temp=temp, rain=rain, wind=wind, y=y)
#' 
#' ### jags example
#' library(R2jags)
#' 
#' ## Model
#' M <- function() {
#'   for (i in 1:N) {
#'     y[i] ~ dnorm(y.hat[i], sd^-2)
#'     y.hat[i] <- a + beta.temp*temp[i] + beta.rain*rain[i] + beta.wind*wind[i]
#'     resid[i] <- y[i] - y.hat[i]
#'   }
#'   sd ~ dunif(0, 100)
#'   a ~ dnorm(0, 0.0001)
#'   beta.temp ~ dnorm(0, 0.0001)
#'   beta.rain ~ dnorm(0, 0.0001)
#'   beta.wind ~ dnorm(0, 0.0001)
#' }
#' 
#' ## Fit model
#' jagsfit <- jags(dat, inits=NULL, 
#'                 parameters.to.save=c('a', 'beta.temp', 'beta.rain', 
#'                                      'beta.wind', 'sd', 'resid'), 
#'                 model.file=M, n.iter=10000)
#' 
#' ## Extract pD and DIC
#' pd_dic(jagsfit)
#' 
pd_dic <- function(x) {
  if(!any(is(x) %in% c('rjags.parallel', 'rjags')))
    stop('x must be an mcmc.list or rjags  object.')
  if(!x$DIC) list(pD=NA, DIC=NA) else list(pd=x$BUGSoutput$pD, DIC=x$BUGSoutput$DIC)
}
