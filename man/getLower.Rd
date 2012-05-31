\name{getLower}
\alias{getLower}
\title{Get lower bounds for numerical / integer parameters.}
\usage{
  getLower(par.set)
}
\arguments{
  \item{par.set}{[\code{\link{ParamSet}}]\cr Parameter
  set.}
}
\value{
  Numeric vector of lower bounds. The lower bounds are all
  concatenated and the vector is named by the parameter
  names with \code{getParamIds(..., repeated=TRUE,
  with.nr=FALSE)}.
}
\description{
  Get lower bounds for numerical / integer parameters.
}
\examples{
ps <- makeParamSet(
  makeNumericParam("u"),
  makeIntegerParam("v", lower=1, upper=2),
  makeDiscreteParam("w", values=1:2),
  makeNumericVectorParam("x", length=2, lower=c(0, 10), upper=c(1, 11))
)
getLower(ps)
}

