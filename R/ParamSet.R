#' Construct a parameter set from a bunch of parameters. 
#' 
#' Multiple sets can be concatenated with \code{c}.
#' 
#' The constructed S3 class is simply a list that contains the element \code{pars}.
#' \code{pars} is a list of the passed parameters, named by their ids. 
#'
#' @param ... [\code{\link{Param}}]\cr
#'   Parameters.
#' @param params [list of \code{\link{Param}}]\cr
#'   List of parameters, alternative ways instead of using \code{...}.
#' @return [\code{\link{ParamSet}}].
#' @aliases ParamSet
#' @export 
#' @examples 
#' makeParamSet(
#'   makeNumericParam("u", lower=1),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeLogicalParam("x"),
#'   makeDiscreteVectorParam("y", length=2, values=c("a", "b"))
#' )
makeParamSet = function(..., params) {
  pars = list(...)
  if (length(pars) > 0 && !missing(params))
    stop("You can only use one of ... or params!")
  if (!missing(params)) {
    checkListElementClass(params, "Param")
    pars = params
  } else {
    checkListElementClass(pars, "Param")
  }
  ns = extractSubList(pars, "id")
  if (any(duplicated(ns)))
    stop("All parameters must have unique names!")
  names(pars) = ns
  x = list(pars=pars)
  class(x) = "ParamSet"
  return(x)
}

#' @S3method print ParamSet
print.ParamSet = function(x, ...) {
  if (length(x$pars) == 0)
    print("Empty parameter set.")
  else  
    sapply(x$pars, print)
}

#' @S3method c ParamSet
c.ParamSet = function(..., recursive=FALSE) {
  pss = list(...)
  pars = Reduce(c, lapply(pss, function(ps) ps$pars))
  do.call(makeParamSet, pars)
}
