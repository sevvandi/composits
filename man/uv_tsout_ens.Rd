% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate_tsout_ensemble.R
\name{uv_tsout_ens}
\alias{uv_tsout_ens}
\title{Performs univariate time series outlier ensemble.}
\usage{
uv_tsout_ens(x, frequency = 1, dates = NULL)
}
\arguments{
\item{x}{A univariate time series as either a \code{ts} object or a vector.}

\item{frequency}{The frequency associated with the time series}

\item{dates}{The dates associated with the time series. This is needed for the package \code{anomalize}. If not explicitly set \code{dates} are set at a frequency 1 ending on the system date.}
}
\value{
A list with the following components:
\item{\code{outliers}}{The outliers detected, repeated if detected by multiple outlier methods.}
\item{\code{forcastOut}}{The outliers detected R package \code{forecast}.}
\item{\code{tsoutliersOut}}{The outliers detected R package \code{tsoutliers}.}
\item{\code{otsadOut}}{The outliers detected R package \code{otsad}.}
\item{\code{anomalizeOut}}{The outliers detected R package \code{anomalize}.}
\item{\code{outmat}}{A matrix containing zeros and ones, with ones representing time points identified as outliers from different methods.}
}
\description{
Performs univariate time series outlier ensemble.
}
\examples{
\dontrun{
set.seed(100)
n <- 500
x <- sample(1:100, n, replace=TRUE)
x[25] <- 200
x[320] <- 270
df <- data.frame(timestamp=1:n, value=x)
plot(ts(df$value))
out <- uv_tsout_ens(x)
out
}

}
