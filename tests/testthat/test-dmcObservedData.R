context("dmcObservedData")

test_that("dmcObservedData", {

  # create data frame
  dat <- createDF(nVP = 50, nTrl = 100,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat)
  datOb <- dmcObservedData(dat)

  testthat::expect_type(datOb, "list")
  testthat::expect_s3_class(datOb, "dmcob")

  dat <- createDF(nVP = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(500, 150, 100),
                             "Comp_incomp" = c(530, 150, 150)),
                   Error = list("Comp_comp"   = c(5, 2, 2, 1, 1),
                                "Comp_incomp" = c(7, 4, 2, 1, 1)))
  datOb <- dmcObservedData(dat)

  testthat::expect_type(datOb, "list")
  testthat::expect_s3_class(datOb, "dmcob")

})
