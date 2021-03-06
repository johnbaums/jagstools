% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rearray.R
\name{rearray}
\alias{rearray}
\title{Recover array structure for multidimensional nodes}
\usage{
rearray(x, param, fields, regex = FALSE, ...)
}
\arguments{
\item{x}{The \code{rjags}, \code{rjags.parallel}, or \code{mcmc.list} object
for which array structure will be recovered.}

\item{param}{Character vector of exact names of multidimensional parameters 
(if \code{regex} is \code{FALSE}, the default), or a regular expression
pattern that will be compared to all available multidimensional parameters
(if \code{regex} is \code{TRUE}). If left unspecified, arrays will be
returned for all multidimensional parameters in \code{x}.}

\item{fields}{A vector (character or integer) that indicates the summary 
fields that should be included as array slices. Valid fields are:
\code{'mean'}, \code{'sd'}, \code{'Rhat'}, \code{'n.eff'}, and a range of
quantiles. By default, these quantiles include \code{'2.5\%'}, 
\code{'25\%'}, \code{'50\%'}, \code{'75\%'}, and \code{'97.5\%'}, but if a
vector of alternative quantiles is passed via \code{probs} (see examples),
then those quantiles' names may be used. Note that 'Rhat' and 'n.eff' are
only available if \code{x} is an \code{rjags} or \code{rjags.parallel}
object. If left unspecified, all available fields are returned.}

\item{regex}{Logical. If \code{regex} is \code{TRUE}, then \code{param} 
should be a  single string giving a text pattern to be matched. Parameters 
with names matching the pattern will be returned (unless the match is
inverted by passing \code{invert=TRUE} via \code{...}, which results in all
parameters that do \emph{not} match the pattern being returned). Text
pattern matching uses regular expressions (\code{\link{regex}}).}

\item{...}{Additional arguments passed to \code{\link{jagsresults}}.}
}
\value{
A list of arrays (one per multidimensional model parameter specified
  in \code{param}) containing node summaries for the requested \code{fields}.
  Fields (e.g. 'mean', 'sd', 'Rhat', etc.) will be included as the final
  (right-most) dimension.
}
\description{
Converts summaries of multidimensional nodes of \code{rjags} or 
\code{mcmc.list} objects to a list of arrays.
}
\examples{
data(simgrowth)
a <- rearray(simgrowth)
str(a)
a2 <- rearray(simgrowth, param='lambda', fields=c('mean', '50\%', 'n.eff'))
str(a2)
a3 <- rearray(simgrowth, param='log.r', probs=seq(0, 1, 0.1))
str(a3)
}
\seealso{
\code{\link{jagsresults}}
}

