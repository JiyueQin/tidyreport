% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{center_standard}
\alias{center_standard}
\title{Center or standardize}
\usage{
center_standard(dat, vars_vec, center = T, standard = F, id = NULL)
}
\arguments{
\item{dat}{A tibble or dataframe of the data}

\item{vars_vec}{A character vector of variables to center or standardize}

\item{center}{logical, T if you want to center the variables, default is T}

\item{standard}{logical, T if you want to standardize the variables, default is F}

\item{id}{A character of the id column name, optional}
}
\value{
A tibble with centered or standardized variables
}
\description{
This function centers or standardizes a vector of variables,
specify id if the data is longitudinal(for example, educyrs in EAS longitudinal data)
}
