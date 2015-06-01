#' jagstools: Convenience functions for rjags and mcmc.list objects
#' 
#' The jagstools package provides functions that simplify access to summaries of
#' model parameter samples stored in \code{rjags} and \code{mcmc.list} objects. 
#' \code{rhats} prints mean, sd and rhat for each parameter of an \code{rjags}
#' object, with optional sorting. \code{jagsresults} prints parameter estimates
#' etc. for specified parameters using regex pattern matching. \code{rearray}
#' recovers array structure for multidimensional parameters
#' 
#' @docType package
#' @name jagstools
NULL