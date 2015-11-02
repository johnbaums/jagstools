#' Recover array structure for multidimensional nodes
#' 
#' Converts summaries of multidimensional nodes of \code{rjags} or 
#' \code{mcmc.list} objects to a list of arrays.
#' 
#' @param x The \code{rjags}, \code{rjags.parallel}, or \code{mcmc.list} object
#'   for which array structure will be recovered.
#' @param param Character vector. The multidimensional parameters that will be 
#'   given an array structure. Default is \code{'all'}, which returns arrays for
#'   all multidimensional parameters in \code{x}.
#' @param fields A vector (character or integer) that indicates the summary 
#'   fields that should be included as array slices. Valid fields are: 'mean', 
#'   'sd', '2.5\%', '25\%', '50\%', '75\%', '97.5\%', 'Rhat', and 'n.eff'. If 
#'   supplied as an integer vector, numbers should indicate the column numbers 
#'   (i.e. positions in the series given above). Note that 'Rhat' and 'n.eff' 
#'   are only available if \code{x} is an \code{rjags} object. Default is 
#'   \code{'all'}, which returns all available fields as array slices.
#' @return A list of arrays (one per multidimensional model parameter specified
#'   in \code{param}) containing node summaries for the requested \code{fields}.
#'   Fields (e.g. mean, sd, Rhat, etc.) will be included as the last
#'   (right-most) dimension.
#' @author John Baumgartner, \email{johnbaums@@gmail.com}
#' @seealso \code{\link{jagsresults}}
#' @export
#' @examples
#' data(simgrowth)
#' a <- rearray(simgrowth)
#' str(a)
#' a2 <- rearray(simgrowth, param='lambda', fields=c('mean', '50\%', 'n.eff'))
#' str(a2)
#' # or...
#' a2 <- rearray(simgrowth, param='lambda', fields=c(1, 2, 9))
#' str(a2)
rearray <- function(x, param='all', fields='all') {
  if(identical(param, 'all')) param <- '.*'
  results <- jagsresults(x, paste(paste(param, '\\[[0-9]+(,[0-9]+)+\\]', sep=''), 
                                  collapse='|'), regex=TRUE, exact=FALSE)
  if(!length(results)) stop(sprintf('No arrays found in object %s', 
                                    deparse(substitute(x))))
  if(identical(fields, 'all')) fields <- colnames(results)
  if(!(all(fields %in% colnames(results))) & !(all(fields %in% seq_len(ncol(results))))) {
    stop(sprintf("fields must be either 'all', or a character vector of jags ',
                 'summary column names, which may include: %s", 
                 toString(colnames(results))))
  }
  results <- results[, fields, drop=FALSE]
  splt_results <- split(as.data.frame(results), sapply(strsplit(row.names(results), '\\[|,|\\]'), function(x) x[1]))
  # row.matches adapted from https://stat.ethz.ch/pipermail/r-help/2000-July/007393.html
  row.matches <- function(y, X) {
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
      a[row.matches(ind[i,], arrind[, -ncol(arrind)])] <<- unlist(x[i,])
    }))   
    dimnames(a)[[length(dim(a))]] <- as.list(colnames(x))
    return(drop(a))
  })
}