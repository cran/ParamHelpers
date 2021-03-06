% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParamSet.R
\name{makeParamSet}
\alias{makeParamSet}
\alias{ParamSet}
\alias{makeNumericParamSet}
\title{Construct a parameter set.}
\usage{
makeParamSet(..., params = NULL, forbidden = NULL, keys = NULL)

makeNumericParamSet(id = "x", len, lower = -Inf, upper = Inf, vector = TRUE)
}
\arguments{
\item{...}{(\code{\link[=Param]{Param()}})\cr
Parameters.}

\item{params}{(list of \code{\link[=Param]{Param()}})\cr
List of parameters, alternative way instead of using \code{...}.}

\item{forbidden}{(\code{NULL} | R expression)\cr
States forbidden region of parameter set via an expression. Every setting
which satisfies this expression is considered to be infeasible. This makes
it possible to exclude more complex region of the parameter space than
through simple constraints or \code{requires}-conditions (although these should
be always used when possible). If parameters have associated trafos, the
forbidden region must always be specified on the original scale and not the
transformed one. Default is \code{NULL} which means no forbidden region.}

\item{keys}{\link{character}\cr
Character vector with keys (names) of feasible variable names which will be
provided via a dictionary/hash later. Default is \code{NULL}.}

\item{id}{(\code{character(1)})\cr
Name of parameter.}

\item{len}{(\code{integer(1)})\cr
Length of vector.}

\item{lower}{(\code{numeric})\cr
Lower bound.
Default is \code{-Inf}.}

\item{upper}{\link{numeric} \cr
Upper bound.
Default is \code{Inf}.}

\item{vector}{(\code{logical(1)}) \cr
Should a \code{NumericVectorParam} be used instead of
n \code{NumericParam} objects?
Default is \code{TRUE}.}
}
\value{
\code{\link[=ParamSet]{ParamSet()}} | \code{LearnerParamSet}.
If all parameters of the \code{ParamSet} are learner parameters, the output
will inherit the class \code{LearnerParamSet}.
}
\description{
\code{makeParamSet}: Construct from a bunch of parameters.

Multiple sets can be concatenated with \code{c}.

The constructed S3 class is simply a list that contains the element \code{pars}.
\code{pars} is a list of the passed parameters, named by their ids.

If \code{keys} are provided it will automatically be checked whether all
expressions within the provided parameters only contain arguments that are a
subset of keys.
}
\examples{
makeParamSet(
  makeNumericParam("u", lower = 1),
  makeIntegerParam("v", lower = 1, upper = 2),
  makeDiscreteParam("w", values = 1:2),
  makeLogicalParam("x"),
  makeDiscreteVectorParam("y", len = 2, values = c("a", "b"))
)
makeParamSet(
  makeNumericParam("u", lower = expression(ceiling(n))),
  makeIntegerParam("v", lower = expression(floor(n)), upper = 2),
  keys = c("p", "n")
)
makeParamSet(
  makeNumericParam("min", lower = 0, upper = 0.8),
  makeNumericParam("max", lower = 0.2, upper = 1),
  forbidden = expression(min > max)
)
}
