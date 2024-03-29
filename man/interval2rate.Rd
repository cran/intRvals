% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DroppingInterval.R
\name{interval2rate}
\alias{interval2rate}
\title{Conversion of interval estimates to rates}
\usage{
interval2rate(
  data,
  minint = data$mu/100,
  maxint = data$mu + 3 * data$sigma,
  digits = max(3L, getOption("digits") - 3L),
  method = "exact"
)
}
\arguments{
\item{data}{An object of class \code{intRvals}, usually a result of a call to \link[intRvals]{estinterval}}

\item{minint}{the minimum interval value from which numerical integrations converting to rates are started}

\item{maxint}{the maximum interval value up to which numerical integrations converting to rates are continued}

\item{digits}{the number of digits for printing to screen}

\item{method}{A string equal to 'exact' or 'taylor'. When 'exact' exact formula or numeric integration
is used. When 'taylor' a Taylor approximation is used as in standard propagation of uncertainty in the case of division.}
}
\value{
The function \code{interval2rate} computes and returns a named vector with the rate mean and standard deviation
}
\description{
Conversion of interval estimates to rates
}
\details{
\subsection{Gamma-distributed intervals}{
When inter-arrival times (intervals) follow a gamma distribution with mean \eqn{\mu} and
standard deviation \eqn{\sigma}, i.e. follow the probability density function
\code{\link[stats]{GammaDist}(shape=}\eqn{\alpha=\mu^2/\sigma^2}\code{, scale=}\eqn{\beta=\sigma^2/\mu)},
 then the associated distribution of rates is given by an inverse gamma distribution
 with shape parameter \eqn{\alpha} and scale parameter \eqn{1/\beta}.

The mean of this inverse gamma distribution is given by the formula
\deqn{\mu_{rate}=\mu/(\mu^2 - \sigma^2)}
provided that \eqn{\alpha > 1}, i.e. \eqn{\mu > \sigma}.

The variance of this inverse gamma distribution is given by the formula
\deqn{\sigma^2_{rate}=\mu^2\sigma^2/((\mu^2 - \sigma^2)(\mu^2 - 2\sigma^2)^2}
provided that \eqn{\alpha > 2}, i.e. \eqn{\mu > sqrt(2) * \sigma}.

Values \eqn{\mu} and \eqn{\sigma} are estimated on the interval data, and
above formula are used to calculate the estimated mean and variance of the arrival rate.

If these formula cannot be used (because the provisions on the value
of \eqn{\alpha} are not met), numerical integration is used instead,
analagous to the procedure for normal-distributed intervals, see below.
}
\subsection{Normal-distributed intervals}{
When inter-arrival times (intervals) \eqn{x} follow a normal distribution with mean \eqn{\mu} and
standard deviation \eqn{\sigma}, i.e. follow the probability density function
\code{\link[stats]{Normal}(mean=}\eqn{\mu}\code{, sd=}\eqn{\sigma)},
 then the mean rate (\eqn{\mu_{rate}}) can be calculated numerically by:
 \deqn{\mu_{rate}=\int_0^\infty (1/x) * \phi(x | \mu,\sigma)}
 and the variance of the rate (\eqn{\sigma^2_{rate}}) by:
 \deqn{\sigma^2_{rate}=\int_0^\infty (1/x^2) * \phi(x | \mu,\sigma) -\mu_{rate}^2}
This approximation is only valid for distributions that have a negligable
density near \eqn{x=0}, such that the distribution can be effectively
 truncated before \eqn{x} approaches zero, where the integral is not defined.
For interval data with intervals \eqn{x}
near zero, use of a gamma distribution is recommended instead.
}
}
\examples{
data(goosedrop)
dr=estinterval(goosedrop$interval)
interval2rate(dr)
}
