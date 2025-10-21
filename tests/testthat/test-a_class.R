test_that("a_class takes questionmarks", {
    x1 <- a_class(  a,  what = c("p", "c"))
    x2 <- a_class( ?b,  what = "c")
    x3 <- a_class(a:b,  what = "?huh")
    x4 <- a_class("?b", what = c("c"))

    expect_equal(
        x1,
        triple(c("", "a"), c("", "a"), c("p", "c"))
    )

    expect_equal(
        x2,
        triple(c("", "?b"), c("", "a"), c("", "c"))
    )

    expect_equal(
        x3,
        triple(c("a", "b"), c("", "a"), c("?", "huh"))
    )

    expect_equal(
        x4,
        triple(c("", "?b"), c("", "a"), "c")
    )

})
