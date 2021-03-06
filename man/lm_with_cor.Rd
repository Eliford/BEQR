% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_model_explr_funtions.R
\name{lm_with_cor}
\alias{lm_with_cor}
\title{Plot continous versus continous variables.}
\usage{
lm_with_cor(data, mapping, ..., method = "pearson")
}
\arguments{
\item{data}{dataset containg continous and continous variables for which to
explore relationships}

\item{mapping}{aes mappings as in ggplot aes}

\item{...}{other arguments passed to geom smooth}
}
\value{
a ggplot2 object to plot
}
\description{
The function was written for exploration of parameter versus continous
covariates relationships. It can be used to customize plots produced by the
GGally package
}
