% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.R
\name{map2_get_regression}
\alias{map2_get_regression}
\title{Get regression results with multiple outcomes and exposures}
\usage{
map2_get_regression(
  df,
  outcomes,
  exposures,
  covariates,
  outcome_type,
  random_slope_var = NULL,
  lme_method = "REML",
  sub_var = NULL,
  sub_group = NULL,
  interaction_var = NULL,
  multiply = T
)
}
\arguments{
\item{df}{A tibble or dataframe of the data}

\item{outcomes}{A character vector of outcomes}

\item{exposures}{A character vector of exposures}

\item{covariates}{A character vector of covariates}

\item{outcome_type}{A string, can be one of these options:linear, binary, poisson, ordinal, tobit, normal_gee,
binary_gee, poisson_gee, ordinal_gee, lme, logistic_glme}

\item{sub_var}{A string of the subgroup analysis variable}

\item{sub_group}{A string of the subgroup}

\item{interaction_var}{a sting of the interaction covarite}

\item{multiply}{logical, multiply = T specifies all possible combinations of outcomes and exposures}
}
\value{
A tibble
}
\description{
This function genartes regression results with multiple outcomes and exposures.
For now, it only supports the interaction between one covariable and the exposure.
}
