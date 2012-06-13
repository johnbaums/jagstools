jagsresults <-
  function(x, params, invert=FALSE, exact=TRUE, regex=FALSE, ...) {
    if (!regex) {
      params <- paste(params, collapse='|')
      if (exact) params <- paste('^', gsub('\\|', '\\$\\|\\^', params), '$', sep='')
      rows <- grep(params, gsub('\\[[0-9]+\\]', '', row.names(x$BUGSoutput$summary)),
        invert=invert)
    } else {
      rows <- grep(params, gsub('\\[[0-9]+\\]', '', 
        row.names(x$BUGSoutput$summary)), invert=invert, ...)
    }
    return(x$BUGSoutput$summary[rows,, drop=FALSE])
  }