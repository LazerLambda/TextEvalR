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
    lapply(
      df$cand_tok_1,
      function(e) {
        ifelse(
          test = anyNA(e[[1]]),
          yes = 0,
          no = length(e[[1]]))
      })
    )
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
  ref_lengths <- vapply(
    reference,
    length,
    numeric(1))
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
  # TODO: Not covered by tests
  # BEGIN
  if (fraction$denominator == 0) {
    stop(
      paste("\n\t'-> This error should not have happened.",
            "Please report to the maintainers."), sep = " ")
  }
  # END
  return(fraction$nominator / fraction$denominator)
}


#' Compute BLEU.
#'
#' Compute BLEU according to Papineni et al., 2002. This function can be
#' run by providing a list of sets of references for a vector of candidate
#' sentences.
#'
#' @param ref List of vectors of references.
#' @param cand Vector of candidate sentences.
#' @param n Max n-gram size to be considered (default set to 4).
#' @param weights Custom weight vector for modified precision for each n-gram
#' size.
#' @return BLEU-Score for provided corpus.
#' @export
#' @examples
#' # Corpus
#' ref <- list(
#'   c("The goods cost less than 20 euros.",
#'     "The merchandise was less than 20 EURO."),
#'   c("The fee would equal 40% of the value of the goods...",
#'     "The fee corresponds with 40 % of the goods??? value..."),
#'   c("I am #PRS_ORG# a serious customer and that is why it is not a problem for me.",
#'     "I am a major client of #PRS_ORG# and thus it is no problem for me."))
#' cand <- c("The goods cost less than 20 euros.",
#'     "The fee corresponds to 40% of the value of the goods....",
#'     "I'm a #PRS_ORG# major customer so it's not a problem for me.")
#' bleu_score <- bleu(ref, cand)
#'
#' # Sentence
#' ref <- list(
#'   c("The goods cost less than 20 euros.",
#'     "The merchandise was less than 20 EURO."))
#' cand <- c("The goods cost less than 20 euros.")
#' bleu_score <- bleu(ref, cand)
bleu <- function(ref, cand, n = 4, weights = NA) {
  checkmate::expect_character(cand)
  checkmate::expect_list(ref, types = c("character"))
  checkmate::expect_numeric(n)

  zero_in <- Reduce(
    function(acc, e) return((e == 0) || acc),
    unlist(lapply(ref, nchar)),
    FALSE)
  if (zero_in) {
    stop("\n\t'-> It appears there is an empty string in the reference set.")
  }

  df <- construct_df(ref, cand)
  if ((length(weights) == 1) && is.na(weights)) {
    weights <- rep(1, n) / n
  } else {
    checkmate::expect_numeric(weights)
  }

  mod_prec_n <- c()
  for (i in seq_len(n)) {
    df <- process_df(df, n = i)
    # Get recent n-grams for (i)
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
