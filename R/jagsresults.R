#' Print results for specified rjags or mcmc.list parameters
#' 
#' Prints results for \code{rjags} or \code{mcmc.list} parameters, which are 
#' specified with a regular expression. Pattern matching can be exact or 
#' approximate.
#' 
#' @param x The \code{rjags} or \code{mcmc.list} object for which results will 
#'   be printed.
#' @param params Character vector. The parameters for which results will be 
#'   printed (unless \code{invert} is \code{FALSE}, in which case results for 
#'   all parameters other than those given in \code{params} will be returned).
#'   If \code{exact} is \code{FALSE}, then parameters whose names contain any of
#'   \code{params} will be returned. If \code{exact} is \code{TRUE} only those
#'   parameters that match \code{params} exactly will be returned. If 
#'   \code{regex} is \code{TRUE}, \code{param} should be a character string 
#'   giving the pattern to be matched.
#' @param invert Logical. If \code{invert} is \code{TRUE}, only those parameters
#'   that do not match elements of \code{params} will be returned.
#' @param exact Logical. If \code{exact} is \code{TRUE}, only parameters whose 
#'   names match elements of \code{param} exactly will be returned. If 
#'   \code{exact = FALSE}, parameters that have names containing the strings 
#'   given by \code{param} will be returned. Ignored if \code{regex} is 
#'   \code{TRUE}.
#' @param regex Logical. If \code{regex} is \code{TRUE}, then \code{param} is 
#'   expected to be a single string giving a text pattern to be matched. 
#'   Parameters with names matching the pattern will be returned (unless 
#'   \code{invert} is \code{TRUE}, which results in all parameters that do not 
#'   match the pattern being returned). Text pattern matching uses regular 
#'   expressions (\code{\link{regex}}).
#' @param probs A numeric vector of probabilities within range [0, 1],
#'   representing the sample quantiles to be calculated and returned.
#' @param ... Additional arguments accepted by grep, e.g. \code{perl} is 
#'   \code{TRUE}, for example to allow look-around pattern matching.
#' @return A matrix with one row for each parameter that matches \code{param}, 
#'   and one column for each of \code{mean}, \code{sd}, percentiles \code{2.5}, 
#'   \code{25}, \code{50}, \code{75}, and \code{97.5}. In addition, if \code{x} 
#'   is an \code{rjags} object, columns for \code{Rhat} and \code{neff} are 
#'   returned.
#' @author John Baumgartner, \email{johnbaums@@gmail.com}
#' @author James Camac, \email{james.camac@@gmail.com}
#' @seealso \code{\link{rhats}} for simplified rhat output, 
#'   \code{\link{rearray}} for recovering array structure of vector-valued 
#'   parameters.
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
#' ## Output
#' # model summary
#' jagsfit
#' 
#' # results for beta.rain only
#' jagsresults(x=jagsfit, param='beta.rain')
#' 
#' # results for 'a' and 'sd' only
# jagsresults(x=jagsfit, param=c('a', 'sd'))
# jagsresults(x=jagsfit, param=c('a', 'sd'), 
#             probs=c(0.01, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
#' 
#' # results for all parameters including the string 'beta'
#' jagsresults(x=jagsfit, param='beta', exact=FALSE)
#' 
#' # results for all parameters not including the string 'beta'
#' jagsresults(x=jagsfit, param='beta', exact=FALSE, invert=TRUE)
#' 
#' # results for all parameters beginning with 'b' or including 'sd'.
#' jagsresults(x=jagsfit, param='^b|sd', regex=TRUE)
#' 
#' # results for all parameters not beginning with 'beta'.
#' # note this is equivalent to using param='^beta' with invert=TRUE and regex=TRUE
#' jagsresults(x=jagsfit, param='^(?!beta)', regex=TRUE, perl=TRUE)
#' 
#' ## mcmc.list example
#' library(rjags)
#' simgrowth$model$recompile()
#' simgrowth_mcmclist <- 
#'   coda.samples(simgrowth$model, 
#'                setdiff(simgrowth$parameters.to.save, 'deviance'), 1000)
#' is(simgrowth_mcmclist)
#' jagsresults(simgrowth_mcmclist, c('b[3]', 'log.r[2,1]'))
#' jagsresults(simgrowth_mcmclist, c('lambda[3,1]', 'sd.'), exact=FALSE)
#' jagsresults(simgrowth_mcmclist, c('lambda\\[3,1\\]|sd\\.'), regex=TRUE)
jagsresults <- function(x, params, invert = FALSE, exact = TRUE, regex = FALSE, 
                        probs=c(0.025, 0.25, 0.5, 0.75, 0.975), ...) {
  if(!regex) {
    params <- paste0(gsub('(?=\\.|\\[|\\])', '\\1\\\\', params, perl=TRUE),
                     '(\\[.*\\])?', collapse='|')
    if (exact) params <- paste("^", 
                               gsub("\\|", "$|^", params), 
                               "$", sep = "")
  } else {
    if(exact) warning('exact=TRUE ignored when regex=TRUE')
  }
  switch(is(x)[1], 
         rjags={
           nm <- dimnames(x$BUGSoutput$sims.array)[[3]]
           i <- grep(params, nm, invert=invert, ...)
           if(length(i) == 0) stop('No parameters match params', call.=FALSE)
           samp <- x$BUGSoutput$sims.array[, , i, drop=FALSE] 
           rhat_neff <- x$BUGSoutput$summary[i, c('Rhat', 'n.eff'), drop=FALSE]
           return(cbind(t(apply(
             samp, 3, function(x) 
               c(mean=mean(x), sd=sd(x), quantile(x, probs=probs)))), rhat_neff))
         },
         mcmc.list={
           nm <- colnames(x[[1]])
           i <- grep(params, nm, invert=invert, ...)
           if(length(i) == 0) stop('No parameters match params', call.=FALSE)
           t(apply(do.call(rbind, x), 2, function(z) {
             c(mean=mean(z), sd=sd(z), quantile(z, probs))
           }))[i, , drop=FALSE]
         },
         {
           stop('x must be an mcmc.list or rjags  object.')
         }
  )
}