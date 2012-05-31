#' @rdname Param
#' @export 
makeNumericParam = function(id, lower=-Inf, upper=Inf, trafo=identity) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(lower, "numeric", len=1, na.ok=FALSE)
  checkArg(upper, "numeric", len=1, na.ok=FALSE)
  if (upper < lower)
    stop("No possible value!")
  makeParam(id, "numeric", 1L, lower, upper, NULL, trafo)
} 

#' @rdname Param
#' @export 
makeNumericVectorParam = function(id, length, lower=-Inf, upper=Inf, trafo=identity) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  length = convertInteger(length)
  checkArg(length, "integer", len=1, na.ok=FALSE)
  if (is.numeric(lower) && length(lower) == 1)
    lower = rep(lower, length)
  if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, length)
  checkArg(lower, "numeric", min.len=1, na.ok=FALSE)
  checkArg(upper, "numeric", min.len=1, na.ok=FALSE)
  if (any(upper < lower))
    stop("No possible value!")
  makeParam(id, "numericvector", length, lower, upper, NULL, trafo)
} 

#' @rdname Param
#' @export 
makeIntegerParam = function(id, lower=-Inf, upper=Inf, trafo=identity) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(lower, "numeric", len=1, na.ok=FALSE)
  checkArg(upper, "numeric", len=1, na.ok=FALSE)
  if (upper < lower)
    stop("No possible value!")
  makeParam(id, "integer", 1L,  lower, upper, NULL, trafo)
} 

#' @rdname Param
#' @export 
makeIntegerVectorParam = function(id, length, lower=-Inf, upper=Inf, trafo=identity) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  length = convertInteger(length)
  checkArg(length, "integer", len=1, na.ok=FALSE)
  if (is.numeric(lower) && length(lower) == 1)
    lower = rep(lower, length)
  if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, length)
  checkArg(lower, "numeric", min.len=1, na.ok=FALSE)
  checkArg(upper, "numeric", min.len=1, na.ok=FALSE)
  if (any(upper < lower))
    stop("No possible value!")
  makeParam(id, "integervector", length,  lower, upper, NULL, trafo)
} 

#' @rdname Param
#' @export 
makeLogicalParam = function(id) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  makeParam(id, "logical", 1L, NULL, NULL, values)
} 

#' @rdname Param
#' @export 
makeDiscreteParam = function(id, values) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  if (is.vector(values))
    values = as.list(values)
  checkArg(values, "list")
  if (length(values)==0)
    stop("No possible value!")
  n = length(values)
  # if names missing, set all to ""
  if (is.null(names(values)))
    names(values) = rep("", n)
  # guess missing names
  ns = names(values)
  for (i in 1:n) {
    v = values[[i]]
    if(is.na(ns[i]) || ns[i] == "") {
      if (is.character(v) || is.numeric(v))
        names(values)[i] = as.character(v)
    }
  }  
  if(!isProperlyNamed(values)) {
    stop("Not all values for par. ", id,  " were named and names could not be guessed!")
  }
  if(any(duplicated(names(values))))
    stop("Not all names for par. ", id,  " are unique!")
  makeParam(id, "discrete", 1L, NULL, NULL, values)
} 

#' @rdname Param
#' @export 
makeDiscreteVectorParam = function(id, length, values) {
  if (is.vector(values))
    values = as.list(values)
  checkArg(values, "list")
  if (length(values)==0)
    stop("No possible value!")
  n = length(values)
  # if names missing, set all to ""
  if (is.null(names(values)))
    names(values) = rep("", n)
  # guess missing names
  ns = names(values)
  for (i in 1:n) {
    v = values[[i]]
    if(is.na(ns[i]) || ns[i] == "") {
      if (is.character(v) || is.numeric(v))
        names(values)[i] = as.character(v)
    }
  }  
  if(!isProperlyNamed(values)) {
    stop("Not all values for par. ", id,  " were named and names could not be guessed!")
  }
  if(any(duplicated(names(values))))
    stop("Not all names for par. ", id,  " are unique!")
  makeParam(id, "discretevector", length, NULL, NULL, values)
} 



#' @rdname Param
#' @export 
makeFunctionParam = function(id) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  makeParam(id, "function", 1L, NULL, NULL, NULL)
} 

#' @rdname Param
#' @export 
makeUntypedParam = function(id) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  makeParam(id, "untyped", 1L, NULL, NULL, NULL)
} 




