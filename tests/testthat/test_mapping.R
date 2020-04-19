test_that("correct mapping of results", {

  inchi <- c("WTJKGGKOPKCXLL-VYOBOKEXSA-N",
             "WTJKGGKOPKCXLL-VYOBOKEXSA-N")

  results <- lipidMapsMapping(context = "compound",
                              input_item = "inchi_key",
                              input_value = inchi[1],
                              output_item = "all")

  # test correct length of list
  expect_equal(length(results), 1)

  results <- lipidMapsMapping(context = "compound",
                              input_item = "inchi_key",
                              input_value = inchi,
                              output_item = "all")

  expect_equal(length(results), 2)


})


