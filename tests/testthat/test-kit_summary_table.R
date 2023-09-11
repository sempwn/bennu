test_that("kit_summary_table returns a table", {
  out <- kit_summary_table(fit, regions, data = d,accuracy = 1)
  testthat::expect_type(out,"list")
})


test_that("kit_summary_table can group by regions and times", {
  out <- kit_summary_table(fit, regions, times,
                           data = d,accuracy = 1)
  testthat::expect_type(out,"list")
})

test_that("kit_summary_table prints correct CrI", {
  out <- kit_summary_table(fit, regions,
                           data = d,accuracy = 1,
                           cri_range = 0.8)
  s <- stringr::str_extract(out[1,2],'\\((.*?)CrI')
  testthat::expect_identical(s,"(80% CrI")

  out <- kit_summary_table(fit, regions,
                           data = d,accuracy = 1,
                           cri_range = 0.65)
  s <- stringr::str_extract(out[1,2],'\\((.*?)CrI')
  testthat::expect_identical(s,"(65% CrI")
})
