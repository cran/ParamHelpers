#' Return ids of parameters in parameter set.
#' 
#' Useful if vectors are included.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param repeated [\code{logical(1)}]\cr
#'   Should ids be repeated length-times if parameter is a vector?
#'   Default is \code{FALSE}.
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to id if \code{repeated} is \code{TRUE}?
#'   Otherwise ignored.
#'   Default is \code{FALSE}.
#' @return [\code{character}].
#' @export
#' @examples
#' ps <- makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerVectorParam("v", length=2)
#' )
#' getParamIds(ps)
#' getParamIds(ps, repeated=TRUE)
#' getParamIds(ps, repeated=TRUE, with.nr=TRUE)
getParamIds = function(par.set, repeated=FALSE, with.nr=FALSE) {
  ns = lapply(par.set$pars, function(x) {
    if (repeated && x$type %in% c("numericvector", "integervector", "discretevector")) {
      n = x$length
      if (n > 1 && with.nr)
        paste(rep(x$id, n), 1:n, sep="")
      else
        rep(x$id, n)
    } else {
      x$id
    }
  })
  as.character(do.call(c, ns))
}