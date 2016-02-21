#' Recover array structure for multidimensional nodes
#' 
#' Converts summaries of multidimensional nodes of \code{rjags} or 
#' \code{mcmc.list} objects to a list of arrays.
#' 
#' @param x The \code{rjags}, \code{rjags.parallel}, or \code{mcmc.list} object
#'   for which array structure will be recovered.
#' @param param Character vector of exact names of multidimensional parameters 
#'   (if \code{regex} is \code{FALSE}, the default), or a regular expression
#'   pattern that will be compared to all available multidimensional parameters
#'   (if \code{regex} is \code{TRUE}). If left unspecified, arrays will be
#'   returned for all multidimensional parameters in \code{x}.
#' @param fields A vector (character or integer) that indicates the summary 
#'   fields that should be included as array slices. Valid fields are:
#'   \code{'mean'}, \code{'sd'}, \code{'Rhat'}, \code{'n.eff'}, and a range of
#'   quantiles. By default, these quantiles include \code{'2.5\%'}, 
#'   \code{'25\%'}, \code{'50\%'}, \code{'75\%'}, and \code{'97.5\%'}, but if a
#'   vector of alternative quantiles is passed via \code{probs} (see examples),
#'   then those quantiles' names may be used. Note that 'Rhat' and 'n.eff' are
#'   only available if \code{x} is an \code{rjags} or \code{rjags.parallel}
#'   object. If left unspecified, all available fields are returned.
#' @param regex Logical. If \code{regex} is \code{TRUE}, then \code{param} 
#'   should be a  single string giving a text pattern to be matched. Parameters 
#'   with names matching the pattern will be returned (unless the match is
#'   inverted by passing \code{invert=TRUE} via \code{...}, which results in all
#'   parameters that do \emph{not} match the pattern being returned). Text
#'   pattern matching uses regular expressions (\code{\link{regex}}).
#' @param ... Additional arguments passed to \code{\link{jagsresults}}.
#' @return A list of arrays (one per multidimensional model parameter specified
#'   in \code{param}) containing node summaries for the requested \code{fields}.
#'   Fields (e.g. 'mean', 'sd', 'Rhat', etc.) will be included as the final
#'   (right-most) dimension.
#' @seealso \code{\link{jagsresults}}
#' @export
#' @examples
#' data(simgrowth)
#' a <- rearray(simgrowth)
#' str(a)
#' a2 <- rearray(simgrowth, param='lambda', fields=c('mean', '50%', 'n.eff'))
#' str(a2)
#' a3 <- rearray(simgrowth, param='log.r', probs=seq(0, 1, 0.1))
#' str(a3)
rearray <- function(x, param, fields, regex=FALSE, ...) {
  if(missing(param)) param <- '.*\\[\\d+(,\\d+)+\\]'
  results <- jagsresults(x, param, regex=regex, ...)
  if(!length(results))
    stop(sprintf('No arrays found in object %s', deparse(substitute(x))))
  if(missing(fields)) fields <- colnames(results)
  if(!(all(fields %in% colnames(results)))) {
    stop(sprintf("fields must be unspecified, or a character vector of ",
                 "summary column names, which may include: %s", 
                 toString(colnames(results))))
  }
  results <- results[, fields, drop=FALSE]
  splt_results <- split(as.data.frame(results), sapply(
    strsplit(row.names(results), '\\[|,|\\]'), function(x) x[1]))
  # row_matches adapted from https://stat.ethz.ch/pipermail/r-help/2000-July/007393.html
  row_matches <- function(y, X) {
    i <- seq(nrow(X))
    j <- 0
    while(length(i) && (j <- j + 1) <= ncol(X)) 
      i <- i[X[i, j] == y[j]]
    i
  }  
  lapply(splt_results, function(x) {
    ind <- do.call(rbind, strsplit(row.names(x), '\\[|,|\\]'))[, -1, drop=FALSE]
    ind <- apply(ind, 2, as.numeric)
    a <- array(dim=c(apply(ind, 2, max), ncol(x)))
    arrind <- arrayInd(seq_along(a), dim(a))
    invisible(lapply(1:nrow(ind), function(i) {
      a[row_matches(ind[i,], arrind[, -ncol(arrind)])] <<- unlist(x[i,])
    }))   
    dimnames(a)[[length(dim(a))]] <- as.list(colnames(x))
    return(drop(a))
  })
}
