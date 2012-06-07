\name{rhats}
\alias{rhats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sort rjags summary by Rhat.
}
\description{
Print sorted rhat for all parameters in an rjags object.
}
\usage{
rhats(x, asc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The rjags object containing the results to be printed.
}
  \item{asc}{
Logical. If \code{asc = TRUE}, results will be sorted by rhat (ascending). If \code{asc = FALSE}, results will be be returned in the order in which they are stored in the rjags object.  Default is \code{asc = TRUE}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with one row per parameter, containing the parameter's Rhat.
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
\code{\link{jagsresults}}
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
                
rhats(x=jagsfit)
}