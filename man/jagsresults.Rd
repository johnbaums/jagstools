\name{jagsresults}
\alias{jagsresults}
\title{
Print results for specified rjags, stanfit or mcmc.list parameters.
}
\description{
Prints results for rjags, stanfit or mcmc.list parameters, which are specified with a regular expression. Pattern matching can be exact or approximate.
}
\usage{
jagsresults(x, params, invert=FALSE, exact=TRUE, regex=FALSE, ...)
}
\arguments{
\item{x}{
The rjags, stanfit or mcmc.list object for which results will be printed.
}
\item{param}{
Character vector. The parameters for which results will be printed (unless \code{invert = FALSE}, in which case results for all parameters other than those given in \code{params} will be returned. If \code{exact = FALSE}, then parameters whose names contain any of \code{params} will be returned, where if \code{exact = TRUE} only those parameters that match \code{params} exactly will be returned. If \code{regex = TRUE}, \code{param} should be a character string giving the pattern to be matched.
}
\item{invert}{
Logical. If \code{invert = TRUE}, only those parameters that do not match elements of \code{params} will be returned. Default: \code{invert = FALSE}.
}
\item{exact}{
Logical. If \code{exact = TRUE}, only parameters whose names match elements of \code{param} exactly will be returned. If \code{exact = FALSE}, parameters that have names containing the strings given by \code{param} will be returned. Ignored if \code{regex = TRUE}. Default: \code{exact = TRUE}.
}
\item{regex}{
Logical. If \code{regex = TRUE}, then \code{param} is expected to be a single string giving a text pattern to be matched. Parameters with names matching the pattern will be returned (unless \code{invert = TRUE}, which results in all parameters that do not match the pattern being returned). Default: \code{regex = FALSE}. Text pattern matching uses regular expressions (\code{\link{regex}}).
}
\item{...}{
Additional arguments that are accepted by grep, e.g. \code{perl = TRUE}, for example to allow look-around pattern matching.
}
}
\details{
}
\value{
A matrix with one row for each parameter that matches \code{param}, and one column for each of \code{mean}, \code{sd}, percentiles \code{2.5}, \code{25}, \code{50}, \code{75}, and \code{97.5}. In addition, if \code{x} is an \code{rjags} object, columns for \code{Rhat} and \code{neff} are returned. If \code{x} is a \code{stanfit} object, columns for \code{Rhat}, \code{neff} and \code{se_mean} are returned.
}
\references{
}
\author{
John Baumgartner <johnbaums@gmail.com>
James Camac <james.camac@gmail.com>
}
\note{
}
\seealso{
\code{\link{rhats} \link{rearray}}
}
\examples{
## Data
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
dat <- list(N=N, temp=temp, rain=rain, wind=wind, y=y)

### jags example
library(R2jags)

## Model
M <- function() {
  for (i in 1:N) {
    y[i] ~ dnorm(y.hat[i], sd^-2)
    y.hat[i] <- a + beta.temp*temp[i] + beta.rain*rain[i] + beta.wind*wind[i]
    resid[i] <- y[i] - y.hat[i]
  }
  sd ~ dunif(0, 100)
  a ~ dnorm(0, 0.0001)
  beta.temp ~ dnorm(0, 0.0001)
  beta.rain ~ dnorm(0, 0.0001)
  beta.wind ~ dnorm(0, 0.0001)
}

## Fit model
jagsfit <- jags(dat, inits=NULL, 
                parameters.to.save=c('a', 'beta.temp', 'beta.rain', 
                                     'beta.wind', 'sd', 'resid'), 
                                     model.file=M, n.iter=10000)

## Output
# model summary
jagsfit

# results for beta.rain only
jagsresults(x=jagsfit, param='beta.rain')

# results for 'a' and 'sd' only
jagsresults(x=jagsfit, param=c('a', 'sd'))

# results for all parameters including the string 'beta'
jagsresults(x=jagsfit, param='beta', exact=FALSE)

# results for all parameters not including the string 'beta'
jagsresults(x=jagsfit, param='beta', exact=FALSE, invert=TRUE)

# results for all parameters beginning with 'b' or including 'sd'.
jagsresults(x=jagsfit, param='^b|sd', regex=TRUE)

# results for all parameters not beginning with 'beta'.
# note this is equivalent to using param='^beta' with invert=TRUE and regex=TRUE
jagsresults(x=jagsfit, param='^(?!beta)', regex=TRUE, perl=TRUE)

### rstan example
library(rstan)

# The following example is provided at ?rstan
stanmodelcode <- "
data {
    int<lower=0> N;
    real temp[N];
    real<lower=0> rain[N];
    real<lower=0> wind[N];
    real y[N];
} 

parameters {
    real beta_temp;
    real beta_rain;
    real beta_wind;
    real<lower=0> sigma;
    real a;
} 

transformed parameters {
    vector[N] y_hat;
    vector[N] resid;
    for (i in 1:N) {
      y_hat[i] <- a + beta_temp*temp[i] + beta_rain*rain[i] + beta_wind*wind[i];
      resid[i] <- y[i] - y_hat[i];
    }
}

model {
    y ~ normal(y_hat, sigma);
}
"

fit <- stan(model_code=stanmodelcode, data=dat, iter=2000, chains=3)
jagsresults(x=fit, param='a')         
jagsresults(x=fit, param='^beta', regex=TRUE)         
            
            
}