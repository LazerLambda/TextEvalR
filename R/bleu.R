library(tokenizers)
library(checkmate)

#' Add length of Candidates to Dataframe.
#'
#' New column is added and filled with length of unigrams of candidate.
#' The length of the sentence in token count is used here.
#'
#' @param df data.frame including cand_tok_1 column with tokenized unigrams.
#' @returns Modified data.frame.
add_cand_length <- function(df) {
  df$cand_len <- unlist(
    lapply(df$cand_tok_1, function(e) length(e[[1]])))
  return(df)
}

#' Compute Effective Reference Length for Each Pair.
#'
#' Get all length of references and choose the length which is closest to
#' the length of the candidate sentence (effective refence length).
#'
#' @param cand_len length of the candidate sentence.
#' @param reference List of reference sentences.
#' @returns Effective reference length.
eff_ref_len_atomic <- function(cand_len, reference) {
  # TODO Test
  ref_lengths <- vapply(reference, length, numeric(1))
  ref_length_ind <- which.min(abs(ref_lengths - cand_len))
  return(ref_lengths[[ref_length_ind]])
}

#' Add Effective Reference Length to data.frame.
#'
#' Add effective reference length to data.frame using the eff_ref_len_atomic
#' function. Only chose unigrams as described in the Papineni et al., 2002.
#'
#' @param df data.frame including already added cand_len and tokenized ref token
#' in column ref_tok_1.
#' @returns Modified data.frame.
add_eff_ref_len <- function(df) {
  i_cand_len <- which(names(df) == "cand_len")
  i_ref_tok <- which(names(df) == "ref_tok_1")
  df$eff_ref_len <- unlist(
    apply(df, 1, function(e) {
      eff_ref_len_atomic(
        e[[i_cand_len]],
        e[[i_ref_tok]])
    })
  )
  return(df)
}

#' Compute Brevity Penalty
#'
#' Compute brevity penalty according to Papineni et al., 2002. Returns 0 iff
#' candidate length is 0. Provided data.frame must include previously computed
#' effective reference length column (eff_ref_len) and candidate length
#' (cand_len).
#' @param df data.frame including already added cand_len and eff_ref_len.
#' @returns Modified data.frame.
brevity_penalty <- function(df) {
  r <- sum(df$eff_ref_len)
  c <- sum(df$cand_len)
  if (c == 0) {
    return(0)
  }
  if (c >= r) {
    return(1)
  } else {
    return(exp(1 - r / c))
  }
}

#' Get n-gram Frequency Table
#'
#' Computes frequency table for n-grams.
#'
#' @param ref List of reference sentences.
#' @return Frequency table.s
ref_n_gram_count <- function(ref) {
  n_gram_tables <- lapply(ref, table)
  n_gram_df <- lapply(n_gram_tables, as.data.frame)
  res <- Reduce(function(...) merge(..., all = TRUE, by = "Var1"), n_gram_df)
  res$max_ref <- apply(res[-1], 1, max, na.rm = TRUE)
  return(res[c("Var1", "max_ref")])
}

#' Count and Clip n-grams for a Row.
#'
#' Occurences are counted and clipped. Clipping is necessary to avoid a higher
#' candidate token count. Due to the precision-inspired nature, this procedure
#' consideres the "true positive" occurences.
#'
#' @param ref List of reference sentences.
#' @param cand A candidate sentence.
#' @return List including nominator and denominator to be divided after
#' summation.
mod_prec_atomic <- function(ref, cand) {

  cand_occ <- as.data.frame(table(cand))
  ref_occ <- ref_n_gram_count(ref)
  combined <- merge(
    ref_occ,
    cand_occ,
    by.x = "Var1",
    by.y = "cand",
    all = TRUE)
  combined[is.na(combined)] <- 0

  # Get clipped values
  combined$clipped <- apply(
    combined,
    1,
    function(e) {
      ref_count <- as.numeric(unname(e[2]))
      cand_count <- as.numeric(unname(e[3]))
      if (ref_count > cand_count) return(cand_count) else return(ref_count)
    }
  )

  return(list(
    nominator = sum(combined$clipped),
    denominator = sum(combined$Freq)))
}

#' Compute Modified Precision for N-Gram.
#'
#' Compute modified precision based on clipped sums from mod_prec_atomic.
#' Sum both nominators and denominators and divide fraction.
#'
#' @param df_loc Slice of data depending on n-gram size. Must include candidate
#' and references in same n-gram size.
#' @return Modified precision for provided corpus.
mod_prec <- function(df_loc) {
  checkmate::expect_data_frame(df_loc)

  sums <- apply(df_loc, 1, function(e) mod_prec_atomic(e[[1]], e[[2]]))

  # Get only n-grams that occur in the candidate
  fraction <- Reduce(
    function(acc, e) {
      return(list(
        nominator = (e$nominator + acc$nominator),
        denominator = (e$denominator + acc$denominator)))},
    sums,
    list(nominator = 0, denominator = 0))
  if (fraction$denominator == 0) return(0)
  return(fraction$nominator / fraction$denominator)
}


#' Compute BLEU (Corpus).
#'
#' Compute BLEU for a corpus-based on according to Papineni et al., 2002.
#'
#' @param ref List of vectors of references.
#' @param cand Vector of candidate sentences.
#' @param n Max n-gram size to be considered (default set to 4).
#' @param weights Custom weight vector for modified precision for each n-gram
#' size.
#' @param tokenize Boolean indicating if tokenization is necessary or has already
#' been applied.
#' @return BLEU-Score for provided corpus.
bleu_c <- function(ref, cand, n = 4, weights = NA, tokenize = TRUE) {
  checkmate::expect_character(cand)
  checkmate::expect_list(ref, types = c("character"))
  checkmate::expect_numeric(n)

  df <- construct_df(ref, cand)
  if (is.na(weights)) {
    weights <- rep(1, n) / n
  } else {
    checkmate::expect_numeric(weights)
  }
  mod_prec_n <- c()
  for (i in seq_len(n)) {
    df <- tokenize_df(df, n = i, tokenize = tokenize)
    # TODO: NOT TOKENIZE FOR EVERY N
    indices <- utils::tail(names(df), 2)
    if (i == 1) {
      df <- add_cand_length(df)
      df <- add_eff_ref_len(df)
    }
    mod_prec_i <- mod_prec(df[indices])
    mod_prec_n <- c(mod_prec_n, mod_prec_i)
  }
  bp <- brevity_penalty(df)
  result <- bp * exp(weights %*% log(mod_prec_n))
  return(result[[1]])
}

#' Compute BLEU (Sentence).
#'
#' Compute BLEU for a sentence-based on according to Papineni et al., 2002.
#'
#' @param ref List of vectors of references.
#' @param cand Vector of candidate sentences.
#' @param n Max n-gram size to be considered (default set to 4).
#' @param weights Custom weight vector for modified precision for each n-gram
#' size.
#' @return BLEU-Score for provided corpus.
bleu_s <- function(ref, cand, n = 4, weights = NA) {
  return(bleu_c(list(ref), cand, n = n, weights = weights))
}
