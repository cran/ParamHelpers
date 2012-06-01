\name{paramValueToString}
\alias{paramValueToString}
\title{Convert a value to a string.}
\usage{
  paramValueToString(par, x)
}
\arguments{
  \item{par}{[\code{\link{Param}} |
  \code{\link{ParamSet}}]\cr Parameter or parameter set.}

  \item{x}{[any]\cr Value for parameter or value for
  parameter set. In the latter case it must be list in the
  same order as the parameters in the set.}
}
\value{
  [\code{character(1)}].
}
\description{
  For discrete parameter values always the name of the
  discrete value is used.
}
\examples{
p <- makeNumericParam("x")
paramValueToString(p, 1) # "1.00"
paramValueToString(p, 1.2345) # "1.23"

p <- makeIntegerVectorParam("x", length=2)
paramValueToString(p, c(1L, 2L)) # 1,2

p <- makeLogicalParam("x")
paramValueToString(p, TRUE) # "TRUE"

p <- makeDiscreteParam("x", values=list(a=NULL, b=2))
paramValueToString(p, NULL) # "a"
}

