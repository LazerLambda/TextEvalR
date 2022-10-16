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

test_that("Test `tokenize_df`.", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df)
  expect_subset(c('ref_tok_1', 'cand_tok_1'), names(df))
})

test_that("Test `tokenize_df` with different n-grams.", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df)
  df <- tokenize_df(df, n = 2)
  df <- tokenize_df(df, n = 3)
  expect_subset(c(
    'ref_tok_1',
    'cand_tok_1',
    'ref_tok_2',
    'cand_tok_2',
    'ref_tok_3',
    'cand_tok_3'), names(df))
})

test_that("Test `eff_ref_len_atomic`.", {
  ref <- list(
    c("it", "is", "a", "guide", "to", "action", "that", "ensures", "that", "the", "military", "will", "forever", "heed", "party", "commands"),
    c("it", "is", "the", "guiding", "principle", "which", "guarantees", "the", "military", "forces", "always", "being", "under", "the", "command", "of", "the", "party", "!"),
    c("it", "is", "the", "practical", "guide", "for", "the", "army", "always", "to", "heed", "the", "directions", "of", "the", "party")
  )
  expect_equal(eff_ref_len_atomic(18, ref), 19)
})

test_that("Test `add_cand_length`.", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df)
  df <- add_cand_length(df)
  expect_equal(sum(df$cand_len), 29)
})

test_that("Test `eff_ref_len`.", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df)
  df <- add_cand_length(df)
  df <- add_eff_ref_len(df)
  expect_equal(sum(df$eff_ref_len), 29)
})

test_that("Test `combine_n_grams`.", {
  test_vec <- c("military", "always", "obeys")
  expect_error(combine_n_grams(list("military", "always", "obeys"), 2))
  expect_equal(
    combine_n_grams(test_vec, 3),
    list(c("military", "always", "obeys")))
  expect_equal(
    combine_n_grams(test_vec, 2),
    list(c("military", "always"), c("always", "obeys")))
  expect_warning(combine_n_grams(test_vec, 4))
})

test_that("Test `brevity_penalty`.", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df)
  df <- add_cand_length(df)
  df <- add_eff_ref_len(df)
  expect_equal(brevity_penalty(df), 1)
})

test_that("Test `ref_n_gram_count` for correct clipping of max count", {
  n_grams_list <- tokenizers::tokenize_ngrams(ref[[1]], n = 1)
  counts <- ref_n_gram_count(n_grams_list)
  count_the <- unname(subset(counts, Var1 == "the")[ ,1])
  count_that <- unname(subset(counts, Var1 == "that")[ ,1])
  expect_equal(as.character(count_the), "the")
  expect_equal(as.character(count_that), "that")
})

test_that("Test `ref_n_gram_count` for lists of lists", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df, n = 1)
  test_list <- lapply(df$ref_tok_1, function(e) {ref_n_gram_count(e)})
  expect_equal(
    Reduce(
      function(acc, e) {(class(e) == "data.frame") && acc},
      test_list,
      TRUE),
    TRUE)
})

test_that("Test `mod_prec_atomic` gte 0", {
  cand_tmp <- tokenizers::tokenize_ngrams(cand, n= 1)[[1]]
  ref_tmp <- tokenizers::tokenize_ngrams(ref[[1]], n= 1)
  mpa <- mod_prec_atomic(ref_tmp, cand_tmp)
  expect_gte(mpa$nominator, 0)
  expect_gte(mpa$denominator, 0)
})

test_that("Test `mod_prec`.", {
  df <- construct_df(ref, cand)
  df <- tokenize_df(df)
  mp <- mod_prec(df[tail(names(df), 2)])
  expect_gte(mp, 0)
})

test_that("Test `bleu_c`.", {
  bleu_test <- bleu_c(ref, cand)
  expect_gte(bleu_test, 0)
})

test_that("Test BLEU function based on NLTK example.", {
  cand = c('It is a guide to action which ensures that the military always obeys the commands of the party')
  ref = list(c('It is a guide to action that ensures that the military will forever heed Party commands',
           'It is the guiding principle which guarantees the military forces always being under the command of the Party',
           'It is the practical guide for the army always to heed the directions of the party'))
  c <- bleu_c(ref, cand)

  expect_equal(c, 0.5045666840058485, tolerance = 0.01)
})

# TODO: Test eff ref len atomic
