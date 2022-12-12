cand1 <- c('It is a guide to action which ensures that the military always obeys the commands of the party')
ref1 <- c('It is a guide to action that ensures that the military will forever heed Party commands',
         'It is the guiding principle which guarantees the military forces always being under the command of the Party',
         'It is the practical guide for the army always to heed the directions of the party')
cand2 <- c('he read the book because he was interested in world history')
ref2 <- c('he was interested in world history because he read the book')
ref <- list(ref1, ref2)
cand <- c(cand1, cand2)

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
  df <- process_df(df)
  df <- add_cand_length(df)
  expect_equal(sum(df$cand_len), 29)
})

test_that("Test `eff_ref_len`.", {
  df <- construct_df(ref, cand)
  df <- process_df(df)
  df <- add_cand_length(df)
  df <- add_eff_ref_len(df)
  expect_equal(sum(df$eff_ref_len), 29)
})

test_that("Test `brevity_penalty`.", {
  df <- construct_df(ref, cand)
  df <- process_df(df)
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
  df <- process_df(df, n = 1)
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
  df <- process_df(df)
  mp <- mod_prec(df[tail(names(df), 2)])
  expect_gte(mp, 0)
})

test_that("Test `bleu`.", {
  bleu_test <- bleu(ref, cand)
  expect_gte(bleu_test, 0)
})

test_that("Test BLEU function based on NLTK example.", {
  cand = c("It is a guide to action which ensures that the military always obeys the commands of the party")
  ref = list(c("It is a guide to action that ensures that the military will forever heed Party commands",
           "It is the guiding principle which guarantees the military forces always being under the command of the Party",
           "It is the practical guide for the army always to heed the directions of the party"))
  c <- bleu(ref, cand)

  expect_equal(c, 0.5045666840058485, tolerance = 0.01)
})

test_that("Test BLEU function based on WMT 22 samples computed by NLTK.", {
  ref <- list(c("The goods cost less than 20 euros.",
                "The merchandise was less than 20 EURO."),
              c("The fee would equal 40% of the value of the goods...",
                "The fee corresponds with 40 % of the goods’ value..."),
              c("I am #PRS_ORG# a serious customer and that is why it is not a problem for me.",
                "I am a major client of #PRS_ORG# and thus it is no problem for me."),
              c("I just need a number or a instructions what I should attach to the package so that it can be traced by you all as a return.",
                "I need only a number or instructions for what to attach to the package so you could track the return with that."),
              c("I ordered the #PRS_ORG# a few days ago...for €249.",
                "A few days ago I ordered the #PRS_ORG# for € 249."),
              c("Now it costs €179.",
                "Today the item costs € 179."),
              c("That really bothers me, I must say.",
                "To be honest, I find this very unfair."))
  cand <- c("The goods cost less than 20 euros.",
            "The fee corresponds to 40% of the value of the goods....",
            "I'm a #PRS_ORG# major customer so it's not a problem for me.",
            "All I need is a number or instructions on what to include in the package so that it can be tracked by you as a return.",
            "I ordered the #PRS_ORG# a few days ago... for 249€.",
            "Today it costs 179€.",
            "To be honest, I find this very annoying.")
  c <- bleu(ref, cand)
  expect_equal(c, 0.5958573576913339, tolerance = 0.01)
})

test_that("Test BLEU function based on WMT 22 samples computed by NLTK II.", {
  ref <- list(c("The goods cost less than 20 euros.",
                "The merchandise was less than 20 EURO."),
              c("The fee would equal 40% of the value of the goods...",
                "The fee corresponds with 40 % of the goods’ value..."),
              c("I am #PRS_ORG# a serious customer and that is why it is not a problem for me.",
                "I am a major client of #PRS_ORG# and thus it is no problem for me."))
  cand <- c("The goods cost less than 20 euros.",
            "The fee corresponds to 40% of the value of the goods....",
            "I'm a #PRS_ORG# major customer so it's not a problem for me.")
  c <- bleu(ref, cand)
  expect_equal(c, 0.5727104863931309, tolerance = 0.01)
})

test_that("Test BLEU function based on WMT 22 samples computed by NLTK II with weights.", {
  ref <- list(c("The goods cost less than 20 euros.",
                "The merchandise was less than 20 EURO."),
              c("The fee would equal 40% of the value of the goods...",
                "The fee corresponds with 40 % of the goods’ value..."),
              c("I am #PRS_ORG# a serious customer and that is why it is not a problem for me.",
                "I am a major client of #PRS_ORG# and thus it is no problem for me."))
  cand <- c("The goods cost less than 20 euros.",
            "The fee corresponds to 40% of the value of the goods....",
            "I'm a #PRS_ORG# major customer so it's not a problem for me.")
  c <- bleu(ref, cand, weights = c(0.25, 0.25, 0.25, 0.25))
  expect_equal(c, 0.5727104863931309, tolerance = 0.01)
})

test_that("Test BLEU function on negative example.", {
  ref <- list(c("That's good.", "Incredible result."))
  cand <- c("It is great.")
  c <- bleu(ref, cand, n = 2)
  expect_equal(c, 0, tolerance = 0.01)
})
