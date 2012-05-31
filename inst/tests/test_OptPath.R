context("OptPath")

test_that("OptPath", {
  ps = makeParamSet(
    makeNumericParam("x"),
    makeDiscreteParam("y", values=c("a", "b"))
  )
  op = makeOptPathDF(par.set=ps, y.names=c("z1", "z2"), minimize=c(TRUE, FALSE))
  addOptPathEl(op, x=list(x=1, y="a"), y=c(z1=1, z2=4))
  addOptPathEl(op, x=list(x=2, y="a"), y=c(z1=3, z2=2))
  expect_equal(op$env$dob, 1:2)
  setOptPathElEOL(op, 2, 8)
  expect_equal(op$env$eol[2], 8)

  x = as.data.frame(op)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 6)
  
  expect_output(print(op), "Optimization path")
  
  expect_equal(getOptPathEl(op, 1)$x, list(x=1, y="a"))
  
  gbe = function(op, y.name, dob) {
    i = getOptPathBestIndex(op, y.name, dob)
    getOptPathEl(op, i)
  }
  
  expect_equal(gbe(op, y.name="z1", dob=1:2), getOptPathEl(op, 1))
  expect_equal(gbe(op, y.name="z2", dob=1:2), getOptPathEl(op, 1))
  expect_equal(gbe(op, y.name="z1", dob=1), getOptPathEl(op, 1))
  expect_equal(gbe(op, y.name="z2", dob=1), getOptPathEl(op, 1))
  expect_equal(gbe(op, y.name="z1", dob=2), getOptPathEl(op, 2))
  expect_equal(gbe(op, y.name="z2", dob=2), getOptPathEl(op, 2))
  
  setOptPathElDOB(op, 1, 1)
  setOptPathElDOB(op, 2, 3)
  expect_equal(as.data.frame(op)$dob, c(1, 3))
  setOptPathElDOB(op, 1:2, 7)
  expect_equal(as.data.frame(op)$dob, c(7, 7))
  setOptPathElDOB(op, 1:2, 4:5)
  expect_equal(as.data.frame(op)$dob, c(4, 5))
  setOptPathElEOL(op, 1, 1)
  setOptPathElEOL(op, 2, 3)
  expect_equal(as.data.frame(op)$eol, c(1, 3))
  setOptPathElEOL(op, 1:2, 7)
  expect_equal(as.data.frame(op)$eol, c(7, 7))
  setOptPathElEOL(op, 1:2, 4:5)
  expect_equal(as.data.frame(op)$eol, c(4, 5))
  
  ps = makeParamSet(
    makeNumericVectorParam("x", length=2),
    makeIntegerParam("y")
  )
  op = makeOptPathDF(par.set=ps, y.names="z", minimize=TRUE)
  addOptPathEl(op, x=list(c(1,1), 7L), y=1)
  addOptPathEl(op, x=list(c(2,2), 8L), y=3)
  df = as.data.frame(op)
  expect_equal(dim(df), c(2,3+1+2))
  expect_equal(colnames(df), c("x1", "x2", "y", "z", "dob", "eol"))
  e = getOptPathEl(op, 1)
  expect_equal(e$x, list(x=c(1,1), y=7L))
})

test_that("OptPath with discrete params works", {
  ps = makeParamSet(
    makeDiscreteParam("x1", values=c("a", "b")),
    makeDiscreteParam("x2", values=1:2),
    makeDiscreteParam("x3", values=c(1.2, 5)),
    makeDiscreteParam("x4", values=list(foo=identity, bar=list())),
    makeLogicalParam("x5"),
    makeDiscreteVectorParam("x6", length=2, values=list(a=identity, b=1))
  )
  op = makeOptPathDF(par.set=ps, y.names="y", minimize=TRUE)
  x = list(x1="a", x2=2L, x3=5, x4=identity, x5=FALSE, x6=list(b=1, a=identity))
  addOptPathEl(op, x=x, y=0)
  d = as.data.frame(op)
  expect_true(nrow(d) == 1 && ncol(d) == 7+1+2)
  expect_true(is.character(d$x1))
  expect_true(is.character(d$x2))
  expect_true(is.character(d$x3))
  expect_true(is.character(d$x4))
  expect_true(is.logical(d$x5))
  expect_true(is.character(d$x61))
  expect_true(is.character(d$x62))
  expect_true(d[1,1] == "a" && d[1,2] == "2" && d[1,3] == "5" && d[1,4] == "foo" 
    && d[1,5] == FALSE && d[1,6] == "b" && d[1,7] == "a")
  e = getOptPathEl(op, 1)
  expect_equal(e$x, x)
})
  
test_that("getOptPathBestIndex tie-handling", {
  ps = makeParamSet(
    makeNumericParam("x")
  )
  op = makeOptPathDF(par.set=ps, y.names="y", minimize=TRUE)
  addOptPathEl(op, x=list(x=0), y=0)
  addOptPathEl(op, x=list(x=0), y=0)
  addOptPathEl(op, x=list(x=0), y=1)
  addOptPathEl(op, x=list(x=0), y=0)
  addOptPathEl(op, x=list(x=0), y=1)
  expect_equal(getOptPathBestIndex(op), 4L)
  expect_equal(getOptPathBestIndex(op, ties="first"), 1L)
  expect_equal(getOptPathBestIndex(op, ties="last"), 4L)
  expect_true(getOptPathBestIndex(op, ties="random") %in% c(1L, 2L, 4L))
  expect_equal(getOptPathBestIndex(op, ties="all"), c(1L, 2L, 4L))

  op = makeOptPathDF(par.set=ps, y.names="y", minimize=FALSE)
  addOptPathEl(op, x=list(x=0), y=0)
  addOptPathEl(op, x=list(x=0), y=0)
  addOptPathEl(op, x=list(x=0), y=1)
  addOptPathEl(op, x=list(x=0), y=0)
  addOptPathEl(op, x=list(x=0), y=1)
  expect_equal(getOptPathBestIndex(op), 5L)
  expect_equal(getOptPathBestIndex(op, ties="first"), 3L)
  expect_equal(getOptPathBestIndex(op, ties="last"), 5L)
  expect_true(getOptPathBestIndex(op, ties="random") %in% c(3L, 5L))
  expect_equal(getOptPathBestIndex(op, ties="all"), c(3L, 5L))
})
    

  


  
  