% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhats.R
\name{rhats}
\alias{rhats}
\title{Sort rjags summary by Rhat.}
\usage{
rhats(x, asc = TRUE)
}
\arguments{
\item{x}{The \code{rjags} or \code{rjags.parallel} object containing the
results to be printed.}

\item{asc}{Logical. If \code{asc} is \code{TRUE}, results will be sorted by 
rhat (ascending). If \code{asc} \code{FALSE}, results will be be returned in
the order in which they are stored in the \code{rjags} object.}
}
\value{
A matrix with one row per parameter, containing parameters' Rhat
  values.
}
\description{
Print sorted rhat for all parameters in an \code{rjags} object
}
\examples{
data(simgrowth)
rhats(simgrowth)
}
\seealso{
\code{\link{jagsresults}}
}

