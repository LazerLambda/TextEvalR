

test_that("Test `read_cand` function 1.", {
  ref_a_name <- "refA"
  ref_b_name <- "refB"
  cand_name <- "cand"

  write(c("abcd", "cdag"), ref_a_name)
  write(c("abcd", "curry gang all day"), ref_b_name)
  write(c("ad", "haskell curry"), cand_name)

  test_cand <- read_cand(cand_name)
  test_ref <- read_ref(c(ref_a_name, ref_b_name))
  expect_equal(length(test_cand), 2)
  test_df <- as.data.frame(test_ref)
  expect_equal(nrow(test_df), 2)
  expect_equal(ncol(test_df), 2)
  expect_equal(class(test_cand), "character")
  expect_equal(class(test_ref), "list")
  expect_equal(class(test_ref[[1]]), "character")
  on.exit({
    file.remove(c(cand_name, ref_a_name, ref_b_name))
  })
})

test_that("Test `read_cand` function 2.", {
  ref_a_name <- "refA"
  cand_name <- "cand"

  write(c("abcd", "cdag"), ref_a_name)
  write(c("ad", "haskell curry"), cand_name)

  test_cand <- read_cand(cand_name)
  test_ref <- read_ref(c(ref_a_name))
  expect_equal(length(test_cand), 2)
  expect_equal(length(test_ref), 2)
  expect_equal(class(test_cand), "character")
  expect_equal(class(test_ref), "list")
  expect_equal(class(test_ref[[1]]), "character")
  on.exit({
    file.remove(c(cand_name, ref_a_name))
  })
})

