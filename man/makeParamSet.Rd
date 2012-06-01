\name{makeParamSet}
\alias{makeParamSet}
\alias{ParamSet}
\title{Construct a parameter set from a bunch of parameters.}
\usage{
  makeParamSet(..., params)
}
\arguments{
  \item{...}{[\code{\link{Param}}]\cr Parameters.}

  \item{params}{[list of \code{\link{Param}}]\cr List of
  parameters, alternative ways instead of using
  \code{...}.}
}
\value{
  [\code{\link{ParamSet}}].
}
\description{
  Multiple sets can be concatenated with \code{c}.
}
\details{
  The constructed S3 class is simply a list that contains
  the element \code{pars}. \code{pars} is a list of the
  passed parameters, named by their ids.
}
\examples{
makeParamSet(
  makeNumericParam("u", lower=1),
  makeIntegerParam("v", lower=1, upper=2),
  makeDiscreteParam("w", values=1:2),
  makeLogicalParam("x"),
  makeDiscreteVectorParam("y", length=2, values=c("a", "b"))
)
}

