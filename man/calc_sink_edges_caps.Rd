% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_partitioning.R
\name{calc_sink_edges_caps}
\alias{calc_sink_edges_caps}
\title{Calculates capacities of edges connecting sink vertex and pixel vertices}
\usage{
calc_sink_edges_caps(vertices_probs)
}
\arguments{
\item{vertices_probs}{df with image information and dnorm_object & dnorm_background columns}
}
\value{
df with from & to columns storing node ids, and capacities of those edges
}
\description{
Calculates capacities of edges connecting sink vertex and pixel vertices
}
\examples{
NULL
}
