jagsresults <-
  function(x, params, invert=FALSE, exact=TRUE, regex=FALSE, ...) {
    if (!regex) {
      rows <- unique(c(sapply(params, function(z) {
        if (exact) {
          grep(paste('^', z, '$', sep=''), 
            gsub('\\[[0-9]+\\]', '', row.names(x$BUGSoutput$summary)), 
               invert=invert)
        } else {
          grep(z, row.names(x$BUGSoutput$summary), invert=invert)
        }
      }), recursive=TRUE))
    } else {
      rows <- grep(params, gsub('\\[[0-9]+\\]', '', 
        row.names(x$BUGSoutput$summary)), invert=invert, ...)
    }
    return(x$BUGSoutput$summary[rows,, drop=FALSE])
  }