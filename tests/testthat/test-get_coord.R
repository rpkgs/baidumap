test_that("multiplication works", {
    r = get_coord("北京")
    expect_equal(r$level, "城市")
})
