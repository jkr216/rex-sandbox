% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portOptimizer.r
\name{calc_objective_terms}
\alias{calc_objective_terms}
\title{Calculate individual terms of the objective function}
\usage{
calc_objective_terms(x, obj_definition, terms = NULL)
}
\arguments{
\item{x}{Vector of portfolio weights}

\item{obj_definition}{objective definition list perhaps created with addTermToObjective function}

\item{terms}{Default NULL in which all terms calculated. Otherwise a numeric vector of terms to be calculated.}
}
\value{
tibble with Name, Value and Type of the term.
}
\description{
Calculate individual terms of the objective function
}
\examples{
calc_objective_terms(x, obj_definition)
calc_objective_terms(x, obj_definition, 1)
calc_objective_terms(x, obj_definition, c(3,5))
}
