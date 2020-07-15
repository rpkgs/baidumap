test_that("multiplication works", {
    r = get_coord(c("北京", "广州"), .parallel = FALSE)
    d_loc = get_location(r)
    
    expect_equal(d_loc$cityCode[1], 131)
})
