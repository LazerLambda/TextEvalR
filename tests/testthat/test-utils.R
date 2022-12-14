cand1 = c('It is a guide to action which ensures that the military always obeys the commands of the party')
ref1 = c('It is a guide to action that ensures that the military will forever heed Party commands',
         'It is the guiding principle which guarantees the military forces always being under the command of the Party',
         'It is the practical guide for the army always to heed the directions of the party')
cand2 <- c('he read the book because he was interested in world history')
ref2 <- c('he was interested in world history because he read the book')
ref <- list(ref1, ref2)
cand <- c(cand1, cand2)

test_that("Test `construct_df`.", {
  df <- construct_df(ref, cand)
  expect_equal(nrow(df), 2)
})

test_that("Test `process_df`.", {
  df <- construct_df(ref, cand)
  df <- process_df(df)
  col_names_exp <- c('ref_tok_1', 'cand_tok_1')
  col_names <- names(df)
  cond <- col_names_exp %in% col_names
  expect_true(Reduce(
    function(acc, e) {e && acc},
    cond,
    TRUE))
})

test_that("Test `process_df` with different n-grams.", {
  df <- construct_df(ref, cand)
  df <- process_df(df)
  df <- process_df(df, n = 2)
  df <- process_df(df, n = 3)
  col_names_exp <- c(
    'ref_tok_1',
    'cand_tok_1',
    'ref_tok_2',
    'cand_tok_2',
    'ref_tok_3',
    'cand_tok_3')
  col_names <- names(df)
  cond <- col_names_exp %in% col_names
  expect_true(Reduce(
    function(acc, e) {e && acc},
    cond,
    TRUE))
})

test_that("Test `combine_n_grams`.", {
  test_vec <- c("military", "always", "obeys")
  expect_error(combine_n_grams(list("military", "always", "obeys"), 2))
  expect_equal(
    combine_n_grams(test_vec, 3),
    c("military always obeys"))
  expect_equal(
    combine_n_grams(test_vec, 2),
    c("military always", "always obeys"))
  expect_warning(combine_n_grams(test_vec, 4))
})

test_that("Test `combine_n_grams_list`.", {
  test_list <- list(
    c("military", "always", "obeys"),
    c("military", "always", "obeys"))
  expect_equal(
    combine_n_grams_list(test_list, 3),
    list(
       "military always obeys",
       "military always obeys"))
  expect_equal(
    combine_n_grams_list(test_list, 2),
    list(
      c("military always", "always obeys"),
      c("military always", "always obeys")))
})
