% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_partitioning.R
\name{calc_neighborhood_edges_caps}
\alias{calc_neighborhood_edges_caps}
\title{Calculates capacities of edges connecting pixel vertices}
\usage{
calc_neighborhood_edges_caps(vertices_probs, params)
}
\arguments{
\item{vertices_probs}{df with image information and dnorm_object & dnorm_background columns}

\item{params}{named list with parameters}
}
\value{
df with from & to columns storing node ids, and capacities of those edges
}
\description{
Calculates capacities of edges connecting pixel vertices
}
\examples{
NULL
}
