context("sample")

test_that("numeric sampling", {
  p = makeNumericParam(id="x", lower=10, upper=20)
  r = sampleValues(p, 13)
  expect_true(is.list(r))
  r = unlist(r)
  expect_true(is.numeric(r))
  expect_equal(length(r), 13)
  expect_true(all(!is.na(r)))
  expect_true(all(r >= p$lower & r <= p$upper))
})

test_that("integer sampling", {
  p = makeIntegerParam(id="x", lower=10, upper=20)
  r = sampleValues(p, 13)
  expect_true(is.list(r))
  r = unlist(r)
  expect_true(is.integer(r))
  expect_equal(length(r), 13)
  expect_true(all(!is.na(r)))
  expect_true(all(r >= p$lower & r <= p$upper))
})

test_that("integer vector sampling", {
  p = makeIntegerVectorParam(id="x", length=2, lower=1, upper=3)
  r = sampleValues(p, 1000)
  r = do.call(rbind, r)
  expect_true(all(r >= p$lower & r <= p$upper))
  # this is stochastic, we dont want that on CRAN as it can fail
  if (interactive()) {
    r = as.numeric(table(r))
    expect_true(all(r > 600 & r < 730))
  }
})


test_that("integer vector sampling", {
  p = makeLogicalParam(id="x")
  r = sampleValues(p, 13)
  expect_true(is.list(r))
  r = unlist(r)
  expect_true(is.logical(r))
  expect_equal(length(r), 13)
  expect_true(all(!is.na(r)))
})


test_that("discrete sampling", {
  p = makeDiscreteParam(id="x", values=c("a", "b", "c"))
  r = sampleValues(p, 13)
  expect_true(is.list(r))
  r = unlist(r)
  expect_true(is.character(r))
  expect_equal(length(r), 13)
  expect_true(all(!is.na(r)))
  expect_true(all(r %in% p$values))
  
  p = makeDiscreteParam(id="x", values=list(a=list(), b=1:3))
  r = sampleValues(p, 10)
  expect_true(is.list(r))
  expect_equal(length(r), 10)
  expect_true(all(r %in% p$values))

  p = makeDiscreteVectorParam(id="x", length=2, values=list(a=list(), b=1:3))
  r = sampleValues(p, 10)
  expect_true(is.list(r))
  expect_equal(length(r), 10)
  ok = function(x) is.list(x) && length(x) == 2 && 
    (length(x[[1]])==0 || x[[1]] %in% 1:3) &&  (length(x[[2]])==0 || x[[2]] %in% 1:3)
  expect_true(all(sapply(r, ok)))
})

test_that("bounds checked", {
  expect_error(sampleValue(makeNumericParam("u", lower=2)), "Cannot sample")
  expect_error(sampleValue(makeNumericParam("u", upper=2)), "Cannot sample")
  expect_error(sampleValue(makeIntegerVectorParam("u", length=2, lower=2)), "Cannot sample")  
  expect_error(sampleValue(makeIntegerVectorParam("u", length=2, upper=2)), "Cannot sample")
})

  
