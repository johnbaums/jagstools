\name{rearray}
\alias{rearray}
\title{
  Recover array structure for multidimensional nodes.
}
\description{
  Converts summaries of multidimensional nodes of rjags or mcmc.list objects to a list of arrays.
}
\usage{
  rearray(x, param = "all", fields = "all") 
}
\arguments{
  \item{x}{
    The rjags or mcmc.list object for which array structure will be recovered.
  }
  \item{param}{
    Character vector. The multidimensional parameters that will be given an array structure. Default is \code{'all'}, which returns arrays for all multidimensional parameters in \code{x}.
  }
  \item{fields}{
    A vector (character or integer) that indicates the summary fields that should be included as array slices. Valid fields are: 'mean', 'sd', '2.5\%', '25\%', '50\%', '75\%', '97.5\%', 'Rhat', and 'n.eff'. If supplied as an integer vector, numbers should indicate the column numbers (i.e. positions in the series given above). Note that 'Rhat' and 'n.eff' are only available if \code{x} is an \code{rjags} object. Default is \code{'all'}, which returns all available fields as array slices.
  }
}
\details{
}
\value{
  A list of arrays (one per multidimensional model parameter specified in \code{param}) containing node summaries for the requested \code{fields}. Fields (e.g. mean, sd, Rhat, etc.) will be included as the last (right-most) dimension.
}
\references{
}
\author{
  John Baumgartner
}
\note{
}

\seealso{
  \code{\link{jagsresults}}
}
\examples{
  data(simgrowth)
  a <- rearray(simgrowth)
  str(a)
  a2 <- rearray(simgrowth, param='lambda', fields=c('mean', '50\%', 'n.eff'))
  a2 <- rearray(simgrowth, param='lambda', fields=c(1, 2, 9)) # equivalent to above
  str(a2)
}