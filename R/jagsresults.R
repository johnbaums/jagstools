jagsresults <-
  function(x, params, not=FALSE, exact=TRUE, regex=FALSE, ...) {
    if (!regex) {
      rows <- unique(c(sapply(params, function(z) {
        if (exact) {
          grep(paste('^', z, '$', sep=''), 
            gsub('\\[[0-9]+\\]', '', row.names(jagsfit$BUGSoutput$summary)))
        } else {
          grep(z, row.names(jagsfit$BUGSoutput$summary))
        }
      }), recursive=TRUE))
    } else {
      rows <- grep(params, gsub('\\[[0-9]+\\]', '', 
        row.names(jagsfit$BUGSoutput$summary)), ...)
    }
    if (not) {
      return(jagsfit$BUGSoutput$summary[-rows,, drop=FALSE])
    } 
    return(jagsfit$BUGSoutput$summary[rows,, drop=FALSE])
  }