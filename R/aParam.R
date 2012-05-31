#' Create a description object for a parameter.
#' 
#' The S3 class is a list which stores these elements:
#' \describe{
#' \item{id [\code{character(1)}]}{See argument of same name.}
#' \item{type [\code{character(1)}]}{Data type of parameter. Possible types are \dQuote{numeric}, \dQuote{numericvector}, \dQuote{integer}, \dQuote{integervector}, \dQuote{discrete}, \dQuote{function}, \dQuote{untyped}.}
#' \item{length [\code{integer(1)}]}{See argument of same name.}
#' \item{lower [\code{numeric}]}{See argument of same name.}
#' \item{upper [\code{numeric}]}{See argument of same name.}
#' \item{values [\code{list}}{Discrete values, always stored as a named list.}
#' \item{trafo [\code{function(x)}]}{See argument of same name.}
#' }
#' 
#' Create a description object for a parameter.
#' 
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param length [\code{integer(1)}]\cr
#'   Length of vector.
#' @param lower [\code{numeric}]\cr
#'   Lower bound. 
#'   Default is \code{-Inf}.
#' @param upper [\code{numeric}] \cr
#'   Upper bound. 
#'   Default is \code{Inf}.
#' @param values [\code{vector} | \code{list}]\cr
#'   Possible discrete values. You are allowed to pass a list of complex R values,
#'   which are used as discrete choices. If you do the latter,
#'   the elements must be uniquely named, so that the names can be used
#'   as internal represenatation for the choice.    
#' @param trafo [\code{function(x)}] \cr
#'   Function to transform parameter. Is should be applied to the parameter value 
#'   before it is e.g. passed to a corresponding fitness function. 
#'   Function must accept a parameter value as the first argument and return a transformed one.
#'   Default is \code{\link{identity}}.   
#' @return [\code{\link{Param}}].
#' @name Param
#' @rdname Param
#' @examples 
#' makeNumericParam("x",lower=-1, upper=1)
#' makeNumericVectorParam("x", length=2)
#' makeDiscreteParam("y", values=c("a","b"))
NULL

makeParam = function(id, type, length, lower, upper, values, trafo=identity) {
  structure(list(
    id = id,
    type = type,
    length = length,
    lower = lower,
    upper = upper,
    values = values,
    trafo = trafo
  ), class="Param")
}

#' @S3method print Param
print.Param = function(x, ...) {
  type = x$type
  ut = !identical(x$trafo, identity)
  if (type == "numeric")
    catf("Num param '%s'. Constr: %s to %s. Trafo: %s.", x$id, x$lower, x$upper, ut) 
  else if (type == "integer")
    catf("Int param '%s'. Constr: %s to %s. Trafo: %s.", x$id, x$lower, x$upper, ut) 
  else if (type == "numericvector")
    catf("Num vec param '%s'. Len: %i. Constr: %s to %s. Trafo: %s.", 
      x$id, x$length, collapse(x$lower), collapse(x$upper), ut) 
  else if (type == "integervector")
    catf("Int vec param '%s'. Len: %i. Constr: %s to %s. Trafo: %s.", 
      x$id, x$length, collapse(x$lower), collapse(x$upper), ut) 
  else if (type == "discrete") 
    catf("Disc param '%s'. Vals: %s. Trafo: %s.", x$id, collapse(names(x$values)), ut) 
  else if (type == "discretevector")
    catf("Disc vec param '%s'. Len: %i. Vals: %s.", x$id, x$length, collapse(names(x$values))) 
  else if (type == "logical") 
    catf("Log param '%s'.", x$id) 
  else if (type == "function")
    catf("Fun param '%s'.", x$id) 
  else if (type == "untyped")
    catf("Untyped param '%s'. Trafo: %s.", x$id, ut) 
}
