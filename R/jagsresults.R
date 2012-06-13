jagsresults <-
  function(x, params, invert=FALSE, exact=TRUE, regex=FALSE, ...) {
    if (!regex) {
      rows <- unique(c(sapply(params, function(z) {
        if (exact) {
          grep(paste('^', z, '$', sep=''), 
            gsub('\\[[0-9]+\\]', '', row.names(jagsfit$BUGSoutput$summary)), 
               invert=invert)
        } else {
          grep(z, row.names(jagsfit$BUGSoutput$summary), invert=invert)
        }
      }), recursive=TRUE))
    } else {
      rows <- grep(params, gsub('\\[[0-9]+\\]', '', 
        row.names(jagsfit$BUGSoutput$summary)), invert=invert, ...)
    }
    return(jagsfit$BUGSoutput$summary[rows,, drop=FALSE])
  }