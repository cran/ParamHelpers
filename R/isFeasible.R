#' Check if parameter value is valid.
#' 
#' Check if a parameter value satisfies the constraints of the
#' parameter description.
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @param x [any] \cr
#'   Single value to check. 
#'   For a parameter set this must be a list in the correct order.
#' @return logical(1)
#' @examples 
#' p <- makeNumericParam("x", lower=-1, upper=1)
#' isFeasible(p,0) # True
#' isFeasible(p,2) # False, out of bounds
#' isFeasible(p,"a") # False, wrong type
#' # now for parameter sets
#' ps <- makeParamSet(
#'   makeNumericParam("x",lower=-1,upper=1),
#'   makeDiscreteParam("y", values=c("a","b"))
#' )
#' isFeasible(ps, list(0,"a")) # True
#' isFeasible(ps, list("a",0)) # False, wrong order 
#' @export
isFeasible = function(par, x) {
  UseMethod("isFeasible")
}

#' @S3method isFeasible Param
isFeasible.Param = function(par, x) {
  if (length(x) == 0)
    return(FALSE)
  type = par$type
  inValues = function(values, v) any(sapply(par$values, function(w) isTRUE(all.equal(w, v))))
  if (type == "numeric")
    is.numeric(x) && length(x) == 1 && is.finite(x) && x >= par$lower && x <= par$upper
  else if (type == "integer")
    is.numeric(x) && length(x) == 1 && is.finite(x) && x >= par$lower && x <= par$upper && x == as.integer(x)
  else if (type == "numericvector")
    is.numeric(x) && length(x) == par$length && all(is.finite(x) & x >= par$lower & x <= par$upper)
  else if (type == "integervector")
    is.numeric(x) && length(x) == par$length && all(is.finite(x) & x >= par$lower & x <= par$upper & x == as.integer(x))
  else if (type == "discrete")
    inValues(par$values, x)
  else if (type == "discretevector") 
    is.list(x) && length(x) == par$length && all(sapply(x, inValues, values=par$values))
  else if (type == "logical") 
    is.logical(x) && length(x) == 1 && !is.na(x)
  else if (type == "function") 
    is.function(x)
  else if (type == "untyped")
    TRUE
}

#' @S3method isFeasible LearnerParam
isFeasible.LearnerParam = function(par, x) {
  type = par$type
  # extra case for unkown dim in vector
  if (type == "numericvector")
    is.numeric(x) && (is.na(par$length) || length(x) == par$length) && all(is.finite(x) & x >= par$lower & x <= par$upper)
  else if (type == "integervector")
    is.numeric(x) && (is.na(par$length) || length(x) == par$length) && all(is.finite(x) & x >= par$lower & x <= par$upper & x == as.integer(x))
  else
    isFeasible.Param(par, x)
}

#' @S3method isFeasible ParamSet
isFeasible.ParamSet = function(par, x) {
  all(mapply(isFeasible, par$pars, x))
}

