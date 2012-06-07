rhats <-
function(x=out, asc=TRUE) {
# print param names and rhat values in ascending order of rhat
if (asc) {
  x$BUGSoutput$summary[order(x$BUGSoutput$summary[, 'Rhat']), 'Rhat', drop=FALSE]
}
else {
x$BUGSoutput$summary[, 'Rhat', drop=FALSE]
}
}