#' Convert encoding name(s) to discrete value(s).
#' 
#' @param par [\code{\link{Param}}]\cr 
#'   Discrete parameter or discrete vector.  
#' @param name [\code{character}]\cr 
#'   Name (string) encoding the value for a discrete parameter, 
#'   or a character vector of names for a discrete vector.   
#' @return [any]. Parameter value for a discrete parameter 
#'   or a list of values for a discrete vector.
#' @examples
#' p <- makeDiscreteParam("u", values=c(x1="a", x2="b", x3="b"))
#' discreteNameToValue(p, "x3")
#' @export
discreteNameToValue = function(par, name) {
  checkArg(par, "Param")
  checkArg(par$type, choices=c("discrete", "discretevector"))
  checkArg(name, "character", na.ok=FALSE, len=ifelse(par$type == "discrete", 1, par$length))
  d = setdiff(name, names(par$values))
  if (length(d) > 0)
    stopf("Names not used in values for parameter %s: %s", par$id, collapse(d))
  if (par$type == "discrete")  
    par$values[[name]]
  else if (par$type == "discretevector")  
    par$values[name]
}

#' Convert discrete value(s) to encoding name(s). 
#'
#' @param par [\code{\link{Param}}]\cr 
#'   Discrete parameter or discrete vector.  
#' @param x [any]\cr 
#'   Parameter value or a list of values for a discrete vector.
#' @return [\code{character}]. Single name for a discrete parameter or a character vector of
#'   names for a discrete vector.
#' @examples 
#' p <- makeDiscreteParam("u", values=c(x1="a", x2="b", x3="c"))
#' discreteValueToName(p, "b")
#' @export
discreteValueToName = function(par, x) {
  checkArg(par, "Param")
  checkArg(par$type, choices=c("discrete", "discretevector"))
  if (par$type == "discretevector")
    if (length(x) != par$length)
      stopf("Length of x must be %i!", par$length)
  ns = names(par$values)
  getIndex = function(values, v) {
    j = which(sapply(values, identical, x=v))
    if (length(j) == 0)
      stop("Value not found!")
    return(j)
  }
  if (par$type == "discrete") {
    ns[getIndex(par$values, x)]
  } else if (par$type == "discretevector")  {
    ns[sapply(x, getIndex, values=par$values)]
  }
}