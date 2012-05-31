#' @rdname LearnerParam
#' @export 
makeNumericLearnerParam = function(id, lower=-Inf, upper=Inf, default, 
  pass.default=FALSE, when="train", requires=expression()) {
  
  p = makeNumericParam(id, lower, upper)
  learnerParamFromParam(p, default, pass.default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeNumericVectorLearnerParam = function(id, length=as.integer(NA), lower=-Inf, 
  upper=Inf, default, pass.default=FALSE, when="train", requires=expression()) {
  
  length = convertInteger(length)
  checkArg(length, "integer", len=1, na.ok=TRUE)
  if (is.na(length))
    p = makeNumericVectorParam(id, length=1, lower=lower, upper=upper)
  else  
    p = makeNumericVectorParam(id, length=length, lower=lower, upper=upper)
  p = learnerParamFromParam(p, default, pass.default, when, requires)
  p$length = length
  return(p)
}


#' @rdname LearnerParam
#' @export 
makeIntegerLearnerParam = function(id, lower=-Inf, upper=Inf,
  default, pass.default=FALSE, when="train", requires=expression()) {
  
  p = makeIntegerParam(id, lower, upper)
  learnerParamFromParam(p, default, pass.default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeIntegerVectorLearnerParam = function(id, length=as.integer(NA), lower=-Inf, 
  upper=Inf, default, pass.default=FALSE, when="train", requires=expression()) {
  
  length = convertInteger(length)
  checkArg(length, "integer", len=1, na.ok=TRUE)
  if (is.na(length))
    p = makeIntegerVectorParam(id, length=1, lower=lower, upper=upper)
  else  
    p = makeIntegerVectorParam(id, length=length, lower=lower, upper=upper)
  p = learnerParamFromParam(p, default, pass.default, when, requires)
  p$length = length
  return(p)
}

#' @rdname LearnerParam
#' @export 
makeDiscreteLearnerParam = function(id, values, default, pass.default=FALSE, 
  when="train", requires=expression()) {
  
  p = makeDiscreteParam(id, values)
  learnerParamFromParam(p, default, pass.default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeDiscreteVectorLearnerParam = function(id, length=as.integer(NA), values, default, pass.default=FALSE, 
  when="train", requires=expression()) {
  
  length = convertInteger(length)
  checkArg(length, "integer", len=1, na.ok=TRUE)
  if (is.na(length))
    p = makeDiscreteVectorParam(id, length=1, values=values)
  else  
    p = makeDiscreteVectorParam(id, length=length, values=values)
  learnerParamFromParam(p, default, pass.default, when, requires)
}


#' @rdname LearnerParam
#' @export 
makeLogicalLearnerParam = function(id, default, pass.default=FALSE, when="train",
  requires=expression()) {
  
  p = makeLogicalParam(id)
  learnerParamFromParam(p, default, pass.default, when, requires)
}


#' @rdname LearnerParam
#' @export 
makeUntypedLearnerParam = function(id, default, pass.default=FALSE, when="train", requires=expression()) {
  p = makeUntypedParam(id)
  learnerParamFromParam(p, default, pass.default, when, requires)
}


#' @rdname LearnerParam
#' @export 
makeFunctionLearnerParam = function(id, default, pass.default=FALSE, when="train", requires=expression()) {
  p = makeFunctionParam(id)
  learnerParamFromParam(p, default, pass.default, when, requires)
}

learnerParamFromParam = function(p, default, pass.default, when, requires) {
  checkArg(pass.default, "logical", len=1, na.ok=FALSE)
  checkArg(when, choices=c("train", "predict", "both"))
  checkArg(requires, "expression")
  if (!missing(default) && !isFeasible(p, default))
    stop(p$id, " : 'default' must be missing or a feasible parameter setting.")  
  has.default = !missing(default)
  if (missing(default))
    default = NULL
  makeLearnerParam(p, has.default, default, pass.default, when, requires)
}
