\name{getOptPathEl}
\alias{getOptPathEl}
\title{Get an element from the optimization path.}
\usage{
  getOptPathEl(op, index)
}
\arguments{
  \item{op}{[\code{\link{OptPath}}]\cr Optimization path.}

  \item{index}{[\code{integer(1)}] Index of element.}
}
\value{
  List with elements \code{x} [named \code{list}], \code{y}
  [named \code{numeric}], \code{dob} [\code{integer(1)}]
  and \code{eol} [\code{integer(1)}].
}
\description{
  Get an element from the optimization path.
}

