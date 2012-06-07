jagsresults <-
function(param, exact=TRUE, x=out) {
# print jags results for specified param (or vector of params).
  # exact = F returns all params that contain the pattern given in param.
  if (length(param > 1)) {
pat <- vector()
    for (i in 1:length(param)) {
      if (exact) {
        pat[i] <- paste('^', param[i], '\\[|^', param[i], '$', sep='')
      } else {
        pat[i] <- param[i]
      }
}
pat <- paste(c(pat), collapse='|')
}else {
pat <- paste('^', param, '\\[|^', param, '$', sep='')
}
x$BUGSoutput$summary[grep(pat, row.names(x$BUGSoutput$summary)),]
}

