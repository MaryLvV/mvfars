#test to check that packaged dataset for 2013 loads

test_that("data loads correctly",  {
    expect_equal(nrow(fars_read("inst/extdata/accident_2013.csv.bz2")) == 30202)
    expect_equal(ncol(fars_read("inst/extdata/accident_2013.csv.bz2")) == 50)
})
