\name{jagsresults}
\alias{jagsresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print results for specified rjags parameters.
}
\description{
Prints results for rjags parameters, which are specified with a regular expression. Pattern matching can be exact or approximate.
}
\usage{
jagsresults(x, param, exact = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
Character string. The parameters for which results will be printed. This can be either the exact name of a parameter, in which case only that parameter will be returned, or a text pattern to be matched (in which case exact=FALSE must be specified). Text pattern matching uses regular expressions (\code{\link{regex}}).
}
  \item{exact}{
Logical. If \code{exact = TRUE}, only parameters whose names match \code{param} exactly will be returned. If \code{exact = FALSE}, any parameters matching the pattern given by \code{param} will be returned. Default: \code{exact = TRUE}.
}
  \item{x}{
The name of the rjags object for which results will be printed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with one row for each parameter that matches \code{param}, and one column for each of mean, sd, percentiles 2.5, 25, 50, 75, and 97.5, Rhat, and neff.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John Baumgartner
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rhats}}
}
\examples{
# data
N <- 100
temp <- runif(N)
rain <- runif(N)
wind <- runif(N)
a <- 0.13
beta.temp <- 1.3
beta.rain <- 0.86
beta.wind <- -0.44
sd <- 0.16
y <- rnorm(N, a + beta.temp*temp + beta.rain*rain + beta.wind*wind, sd)

# model
M <- function() {
  for (i in 1:N) {
    y[i] ~ dnorm(y.hat[i], sd^-2)
    y.hat[i] <- a + beta.temp*temp[i] + beta.rain*rain[i] + beta.wind*wind[i]
    #resid[i] <- y[i] - y.hat[i]
  }
  sd ~ dunif(0, 100)
  a ~ dnorm(0, 0.0001)
  beta.temp ~ dnorm(0, 0.0001)
  beta.rain ~ dnorm(0, 0.0001)
  beta.wind ~ dnorm(0, 0.0001)
}

# fit model
jagsfit <- jags(data=list(N=N, temp=temp, rain=rain, wind=wind, y=y), 
                inits=NULL, parameters.to.save=c('a', 'beta.temp', 'beta.rain', 
                'beta.wind', 'sd'), model.file=M, n.iter=10000)

# output
jagsfit
jagsresults(x=jagsfit, param='beta.rain')
jagsresults(x=jagsfit, param='beta', exact=FALSE)
}