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
#'   all parameters other than those given in \code{params} will be returned). If
#'   \code{exact} is \code{FALSE}, then parameters whose names contain any of 
#'   \code{params} will be returned. If \code{exact} is \code{TRUE} only 
#'   those parameters that match \code{params} exactly will be returned. If 
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
#' jagsresults(x=jagsfit, param=c('a', 'sd'))
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
jagsresults <- function (x, params, invert = FALSE, exact = TRUE, regex = FALSE, 
                         ...) 
{
  if (is(x, 'rjags')) {
    if (!regex) {
      params <- paste(params, collapse = "|")
      if (exact) 
        params <- paste("^", gsub("\\|", "\\$\\|\\^", params), 
                        "$", sep = "")
      rows <- grep(params, gsub("\\[[0-9,]+\\]", "", row.names(x$BUGSoutput$summary)), 
                   invert = invert)
    }
    else {
      rows <- grep(params, row.names(x$BUGSoutput$summary), invert = invert, ...)
    }
    return(x$BUGSoutput$summary[rows, , drop = FALSE])
  } else if (is(x, 'mcmc.list')) {
    
    if (!regex) {
      params <- paste(params, collapse = "|")
      if (exact) 
        params <- paste("^", gsub("\\|", "\\$\\|\\^", params), 
                        "$", sep = "")
      rows <- grep(params, gsub("\\[[0-9,]+\\]", "", colnames(x[[1]])), 
                   invert = invert)
    }
    else {
      rows <- grep(params, colnames(x[[1]]), invert = invert, ...)
    }
    SUMM <- t(apply(do.call(rbind, x), 2, function(z) {
      c(mean=mean(z), sd=sd(z), quantile(z, c(0.025, 0.25, 0.5, 0.75, 0.975)))
    }))
    return(SUMM[rows, , drop = FALSE])
    
  } else stop('x must be either an "rjags" or "mcmc.list"')
}