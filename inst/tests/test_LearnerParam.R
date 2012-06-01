context("LearnerParam")

test_that("num vec", {
  p = makeNumericVectorLearnerParam("x", length=2, lower=0, upper=2)
  expect_equal(p$id, "x")
  expect_equal(p$lower, c(0,0))
  expect_equal(p$upper, c(2,2))
  expect_equal(p$when, "train")
  expect_true(!isFeasible(p, 1))
  expect_true(isFeasible(p, c(1,1)))
  p = makeNumericVectorLearnerParam("x", lower=0, upper=2)
  expect_equal(p$lower, 0)
  expect_equal(p$upper, 2)
  expect_true(isFeasible(p, 1))
  expect_true(isFeasible(p, c(1,1)))
})

test_that("int vec", {
  p = makeIntegerVectorLearnerParam("x", length=2, lower=0L, upper=2L)
  expect_equal(p$id, "x")
  expect_equal(p$lower, c(0,0))
  expect_equal(p$upper, c(2,2))
  expect_equal(p$when, "train")
  expect_true(!isFeasible(p, 1))
  expect_true(isFeasible(p, c(1,1)))
  p = makeIntegerVectorLearnerParam("x", lower=0L, upper=2L)
  expect_equal(p$lower, 0)
  expect_equal(p$upper, 2)
  expect_true(isFeasible(p, 1))
  expect_true(isFeasible(p, c(1,1)))
})

if (interactive()) {
test_that("s3 objs works for values", {
  library(mboost)
  vals = list(a=AdaExp(), b=Binomial())
  p = makeDiscreteLearnerParam("x", values=vals)
  expect_true(isFeasible(p, AdaExp()))
  p = makeDiscreteLearnerParam("x", values=vals, default=AdaExp())
})
}

test_that("unknown length works", {
  p = makeNumericVectorLearnerParam("x", length=NA, lower=1)
  expect_true(isFeasible(p, c(2)))
  expect_true(isFeasible(p, c(2, 3)))
  expect_false(isFeasible(p, c(0)))
  expect_false(isFeasible(p, c(0, 0)))
  expect_error(sampleValue(p), "Cannot sample")
})
