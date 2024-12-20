% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseline_characteristics.R
\name{get_desc_stat}
\alias{get_desc_stat}
\title{Get Descriptive Stat for one dataset}
\usage{
get_desc_stat(
  dat,
  raw = F,
  raw_name = F,
  raw_with_header = F,
  median_vars = NULL,
  detail = F,
  detail_simple = F,
  extra_col = F,
  sort = F,
  CI = F,
  CI_method = "wilson",
  alpha = 0.05
)
}
\arguments{
\item{dat}{A dataframe}

\item{raw}{if you want to get dataframe raw output, default is False}

\item{raw_name}{if you want to get raw name stat instead of sample size, default is False}

\item{raw_with_header}{if you want to get raw output with headers, default is false}

\item{median_vars}{specify the variables that need median(IQR) instead of mean(SD)}

\item{detail}{if you want to get all the descriptive stat for continuous variables, default is F}

\item{detail_simple}{if you  want to get all the descriptive stat in a condensed version for continuous variables, default is F}

\item{extra_col}{if you want to preserve the original variable column, default is F}

\item{sort}{logical, sort =T sorts the variables based on their sequence in the data, default is F, which puts continuous variables first}

\item{CI}{logical, CI=T outputs confidence intervals, default is F}

\item{CI_method}{string, the method for the CI of proportions, default is 'wilson', see binom::binom.confint}

\item{alpha}{numeric, the alpha level for CI, default is 0.05 for 95% CI}
}
\value{
A html table for descriptive statistics
}
\description{
This function generates descriptive stat for all the variables in a dataset
}
