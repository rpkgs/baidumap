test_that("get_place works", {
    d <- get_place("大学", "沈丘")
    expect_true(nrow(d) > 0)
})
