test_that("predicate takes questionmarks", {
  x1 <- predicate(a:b~c:d, what = "", prefix = "")
  x2 <- predicate( ?b~c:d, what = "", prefix = "")
  x3 <- predicate(a:b~ ?d, what = "", prefix = "")
  x4 <- predicate( ?b~ ?d, what = "", prefix = "")

  expect_equal(
      x1,
      triple(c("a", "b"), c("", ""), c("c", "d"))
  )

  expect_equal(
      x2,
      triple(c("", "?b"), c("", ""), c("c", "d"))
  )

  expect_equal(
      x3,
      triple(c("a", "b"), c("", ""), c("", "?d"))
  )

  expect_equal(
      x4,
      triple(c("", "?b"), c("", ""), c("", "?d"))
  )

})
