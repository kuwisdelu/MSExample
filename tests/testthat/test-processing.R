require(testthat)
require(MSExample)

context("processing")

test_that("spectrum proceessing", {

	set.seed(2019)

	s1 <- simSpectra(baseline=4)

	expect_true(validObject(s1))
	
	s2 <- smoothNoise(s1)

	expect_true(validObject(s2))

	s3 <- removeBaseline(s2)

	expect_true(validObject(s3))

	s4 <- findPeaks(s3)

	expect_true(validObject(s4))

	s5 <- binPeaks(s4)

	expect_true(validObject(s5))

})

