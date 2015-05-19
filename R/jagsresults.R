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
  } else if (is(x, "stanfit")) {
    x <- summary(x)
    if (!regex) {
      params <- paste(params, collapse = "|")
      if (exact) 
        params <- paste("^", gsub("\\|", "\\$\\|\\^", 
                                  params), "$", sep = "")
      rows <- grep(params, gsub("\\[[0-9,]+\\]", "", row.names(x$summary)), 
                   invert = invert)
    }
    else {
      rows <- grep(params, row.names(x$summary), invert = invert, ...)
    }
    return(x$summary[rows, , drop = FALSE])
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
    
  } else stop('x must be either an "rjags", "mcmc.list" or "stanfit" object.')
}