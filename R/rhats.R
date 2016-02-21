#' Sort rjags summary by Rhat.
#' 
#' Print sorted rhat for all parameters in an \code{rjags} object
#' 
#' @param x The \code{rjags} or \code{rjags.parallel} object containing the
#'   results to be printed.
#' @param asc Logical. If \code{asc} is \code{TRUE}, results will be sorted by 
#'   rhat (ascending). If \code{asc} \code{FALSE}, results will be be returned in
#'   the order in which they are stored in the \code{rjags} object.
#' @return A matrix with one row per parameter, containing parameters' Rhat
#'   values.
#' @seealso \code{\link{jagsresults}}
#' @export
#' @examples
#' data(simgrowth)
#' rhats(simgrowth)
rhats <- function(x, asc=TRUE) {
    if (asc) {
      x$BUGSoutput$summary[order(x$BUGSoutput$summary[, 'Rhat']), 'Rhat', drop=FALSE]
    }
    else {
      x$BUGSoutput$summary[, 'Rhat', drop=FALSE]
    }
  }