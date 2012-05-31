\name{Param}
\alias{makeDiscreteParam}
\alias{makeDiscreteVectorParam}
\alias{makeFunctionParam}
\alias{makeIntegerParam}
\alias{makeIntegerVectorParam}
\alias{makeLogicalParam}
\alias{makeNumericParam}
\alias{makeNumericVectorParam}
\alias{makeUntypedParam}
\alias{Param}
\title{Create a description object for a parameter.}
\usage{
  makeNumericParam(id, lower = -Inf, upper = Inf,
    trafo = identity)

  makeNumericVectorParam(id, length, lower = -Inf,
    upper = Inf, trafo = identity)

  makeIntegerParam(id, lower = -Inf, upper = Inf,
    trafo = identity)

  makeIntegerVectorParam(id, length, lower = -Inf,
    upper = Inf, trafo = identity)

  makeLogicalParam(id)

  makeDiscreteParam(id, values)

  makeDiscreteVectorParam(id, length, values)

  makeFunctionParam(id)

  makeUntypedParam(id)
}
\arguments{
  \item{id}{[\code{character(1)}] Name of parameter.}

  \item{length}{[\code{integer(1)}]\cr Length of vector.}

  \item{lower}{[\code{numeric}]\cr Lower bound.  Default is
  \code{-Inf}.}

  \item{upper}{[\code{numeric}] \cr Upper bound.  Default
  is \code{Inf}.}

  \item{values}{[\code{vector} | \code{list}]\cr Possible
  discrete values. You are allowed to pass a list of
  complex R values, which are used as discrete choices. If
  you do the latter, the elements must be uniquely named,
  so that the names can be used as internal represenatation
  for the choice.}

  \item{trafo}{[\code{function(x)}] \cr Function to
  transform parameter. Is should be applied to the
  parameter value before it is e.g. passed to a
  corresponding fitness function.  Function must accept a
  parameter value as the first argument and return a
  transformed one.  Default is \code{\link{identity}}.}
}
\value{
  [\code{\link{Param}}].
}
\description{
  The S3 class is a list which stores these elements:
  \describe{ \item{id [\code{character(1)}]}{See argument
  of same name.} \item{type [\code{character(1)}]}{Data
  type of parameter. Possible types are \dQuote{numeric},
  \dQuote{numericvector}, \dQuote{integer},
  \dQuote{integervector}, \dQuote{discrete},
  \dQuote{function}, \dQuote{untyped}.} \item{length
  [\code{integer(1)}]}{See argument of same name.}
  \item{lower [\code{numeric}]}{See argument of same name.}
  \item{upper [\code{numeric}]}{See argument of same name.}
  \item{values [\code{list}}{Discrete values, always stored
  as a named list.} \item{trafo [\code{function(x)}]}{See
  argument of same name.} }
}
\details{
  Create a description object for a parameter.
}
\examples{
makeNumericParam("x",lower=-1, upper=1)
makeNumericVectorParam("x", length=2)
makeDiscreteParam("y", values=c("a","b"))
}

