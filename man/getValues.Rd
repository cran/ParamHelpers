\name{getValues}
\alias{getValues}
\title{Get possible values for discrete parameters.}
\usage{
  getValues(par.set)
}
\arguments{
  \item{par.set}{[\code{\link{ParamSet}}]\cr Parameter
  set.}
}
\value{
  [\code{list}]. A named list of named lists of values.
}
\description{
  Get possible values for discrete parameters.
}
\examples{
ps <- makeParamSet(
  makeNumericParam("u"),
  makeDiscreteParam("v", values=c("a", "b")),
  makeDiscreteParam("w", values=list(a=list(), b=NULL))
)
getValues(ps)
}

