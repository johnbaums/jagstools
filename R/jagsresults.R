jagsresults <-
function(x, param, exact=TRUE) {
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

