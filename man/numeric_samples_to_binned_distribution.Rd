% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samples-to-binned-distributions.R
\name{numeric_samples_to_binned_distribution}
\alias{numeric_samples_to_binned_distribution}
\title{From a vector of numeric samples, create data frame of bin probabilities.}
\usage{
numeric_samples_to_binned_distribution(x, bins)
}
\arguments{
\item{x}{numeric vector of samples}

\item{bins:}{numeric vector of bin start and end points.  For example:
c(seq(from = 0.0, to = 25.0, by = 0.1), 100.0)}
}
\value{
Data frame with columns:
   bin: lower endpoints of bins
   value: proportion of x falling in bin
}
\description{
From a vector of numeric samples, create data frame of bin probabilities.
}
