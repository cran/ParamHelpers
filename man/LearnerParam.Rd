\name{LearnerParam}
\alias{LearnerParam}
\alias{makeDiscreteLearnerParam}
\alias{makeDiscreteVectorLearnerParam}
\alias{makeFunctionLearnerParam}
\alias{makeIntegerLearnerParam}
\alias{makeIntegerVectorLearnerParam}
\alias{makeLogicalLearnerParam}
\alias{makeNumericLearnerParam}
\alias{makeNumericVectorLearnerParam}
\alias{makeUntypedLearnerParam}
\title{Create a description object for a parameter of a machine learning algorithm.}
\usage{
  makeNumericLearnerParam(id, lower = -Inf, upper = Inf,
    default, pass.default = FALSE, when = "train",
    requires = expression())

  makeNumericVectorLearnerParam(id,
    length = as.integer(NA), lower = -Inf, upper = Inf,
    default, pass.default = FALSE, when = "train",
    requires = expression())

  makeIntegerLearnerParam(id, lower = -Inf, upper = Inf,
    default, pass.default = FALSE, when = "train",
    requires = expression())

  makeIntegerVectorLearnerParam(id,
    length = as.integer(NA), lower = -Inf, upper = Inf,
    default, pass.default = FALSE, when = "train",
    requires = expression())

  makeDiscreteLearnerParam(id, values, default,
    pass.default = FALSE, when = "train",
    requires = expression())

  makeDiscreteVectorLearnerParam(id,
    length = as.integer(NA), values, default,
    pass.default = FALSE, when = "train",
    requires = expression())

  makeLogicalLearnerParam(id, default,
    pass.default = FALSE, when = "train",
    requires = expression())

  makeUntypedLearnerParam(id, default,
    pass.default = FALSE, when = "train",
    requires = expression())

  makeFunctionLearnerParam(id, default,
    pass.default = FALSE, when = "train",
    requires = expression())
}
\arguments{
  \item{id}{[\code{character(1)}] Name of parameter.}

  \item{length}{[\code{integer(1)}]\cr Length of vector.}

  \item{lower}{[\code{numeric}]\cr Lower bound.  Default is
  \code{-Inf}.}

  \item{upper}{[\code{numeric}]\cr Upper bound.  Default is
  \code{Inf}.}

  \item{values}{[\code{vector} | \code{list}]\cr Possible
  discrete values. You are allowed to pass a list of
  complex R values, which are used as discrete choices. If
  you do the latter, the elements must be uniquely named,
  so that the names can be used as internal represenatation
  for the choice.}

  \item{default}{[any]\cr Default value used in learner.
  If this argument is missing, it means no default value is
  available.}

  \item{pass.default}{[\code{logical(1)}]\cr Should the
  default value be always passed to the learner?  Default
  is \code{FALSE}.}

  \item{when}{[\code{character(1)}]\cr Specifies when
  parameter is used in the learner: \dQuote{train},
  \dQuote{predict} or \dQuote{both}.  Default is
  \dQuote{train}.}

  \item{requires}{[\code{expression}]\cr R expression over
  the other parameters to define requirements when this
  parameter is effective.}
}
\value{
  [\code{\link{LearnerParam}}].
}
\description{
  This specializes \code{\link{Param}} by adding a few more
  attributes, like a default value, whether it refers to a
  training or a predict function, etc.
}
\details{
  The S3 class is a list which additionally stores these
  elements: \describe{ \item{default [any]}{See argument of
  same name.} \item{pass.default [\code{logical(1)}]}{See
  argument of same name.} \item{has.default
  [\code{logical(1)}]}{Was a default value provided? We
  cannot check \code{default} for \code{NULL} or \code{NA}
  as this could be the default value!} \item{when
  [\code{numeric}]}{See argument of same name.}
  \item{requires [\code{numeric}]}{See argument of same
  name.} }
}

