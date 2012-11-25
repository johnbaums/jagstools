# x = the jags object
# param = the exact name of the parameter to be converted back to an array 
# (can be a vector of param names, in which case a list of arrays will be returned,
# can also be 'all', in which case all parameters with dimensions will be returned)
# fields = the names of jags summary columns to include as array slices (last index)
rearray <- function(x, param='all', fields='all') {
  if(identical(param, 'all')) param <- '.*'
  results <- jagsresults(x, paste(paste(param, '\\[[0-9]+(,[0-9]+)+\\]', sep=''), 
                                  collapse='|'), regex=TRUE)
  if(!length(results)) stop(sprintf('No arrays found in object %s', deparse(substitute(x))))
  if(identical(fields, 'all')) fields <- colnames(results)
  if(!(all(fields %in% colnames(results))) & !(all(fields %in% seq_len(ncol(results))))) {
    stop(sprintf("fields must be either 'all', or a character vector of jags summary column names, which may include: %s", 
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
      #a[which(apply(arrind[, -ncol(arrind)], 1, function(y) all(y==ind[i,])))] <<- unlist(x[i,]) # slow
    }))   
    dimnames(a)[[length(dim(a))]] <- colnames(x)
    return(a)
  })
}