library(tokenizers)
library(checkmate)

construct_df <- function(reference, candidate) {
  checkmate::check_character(candidate)
  checkmate::expect_list(reference, types = "character")
  df <- data.frame(cand = candidate)
  df$ref <- reference
  return(df)
}

tokenize_df <- function(df, n = 1) {
  checkmate::check_data_frame(df)
  col_name <- paste("ref_tok", as.character(n), sep = '_')
  df[col_name] <- list(apply(
    df,
    1,
    function(e) {
      return(tokenizers::tokenize_ngrams(e$ref, n = n))
    }
  ))
  col_name <- paste("cand_tok", as.character(n), sep = '_')
  df[col_name] <- list(apply(
    df,
    1,
    function(e) {
      return(tokenizers::tokenize_ngrams(e$cand, n = n))
    }
  ))
  return(df)
}

add_cand_length <- function(df) {
  df$cand_len <- unlist(
    lapply(df$cand_tok_1, function(e) {length(e[[1]])}))
  return(df)
}

eff_ref_len_atomic <- function(cand_len, reference) {
  # TODO Test
  ref_lengths <- vapply(reference, length, numeric(1))
  ref_length_ind <- which.min(abs(ref_lengths - cand_len))
  return(ref_lengths[[ref_length_ind]])
}

add_eff_ref_len <- function(df) {
  i_cand_len <- which(names(df) == "cand_len")
  i_ref_tok <- which(names(df) == "ref_tok_1")
  df$eff_ref_len <- unlist(
    apply(df, 1, function(e){
      eff_ref_len_atomic(
        e[[i_cand_len]],
        e[[i_ref_tok]])
    })
  )
  return(df)
}

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

combine_n_grams <- function(ngrams, n) {
  checkmate::expect_atomic_vector(ngrams)
  if (n > length(ngrams)) {
    n <- length(ngrams)
    warning("`n` longer than length of tokens. `n` set to length of tokens.")
  }
  indices <- lapply(1:n, function(e) {e:length(ngrams)})
  tokens <- lapply(
    indices,
    function(e) {
      append(
        ngrams[e],
      rep(NA, length(ngrams) - length(e)))
    })
  tmp_df <- as.data.frame(tokens)
  tmp_df <- na.omit(tmp_df)
  ngrams_raw <- apply(tmp_df, 1, function(e) {list(unname(e))})
  new_ngrams <- lapply(ngrams_raw, function(e) {do.call(paste, e)})
  return(unname(new_ngrams))
}

#' Get n-gram Frequency Table
#'
#' Computes frequency table for n-grams.
#'
#' @param reference List of reference sentences.
#' @param n Determining n-length of n-gram.
#' @return Frequency table.s
ref_n_gram_count <- function(ref) {
  # TODO: Checkkmate for list input in form of list(vectors)
  n_gram_tables <- lapply(ref, table)
  n_gram_df <- lapply(n_gram_tables, as.data.frame)
  res <- Reduce(function(...) merge(..., all = TRUE, by = "Var1"), n_gram_df)
  res$max_ref <- apply(res[-1], 1, max, na.rm = TRUE)
  return(res[c("Var1", "max_ref")])
}

mod_prec_atomic <- function(ref, cand) {
  # TODO Typecheck

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

  # Compute modified precision
  # Freq is frequency of n-grams in candidate
  denominator <- sum(combined$Freq)
  nominator <- sum(combined$clipped)
  # if (denominator == 0) return(0) else
  # return(nominator / denominator)
  return(list(
    nominator = sum(combined$clipped),
    denominator = sum(combined$Freq)))
}

mod_prec <- function(df_loc) {
  checkmate::expect_data_frame(df_loc)

  # cand_occurences <- lapply(cand, table)
  # ref_occurences <-  lapply(ref, ref_n_gram_count)

  sums <- apply(df_loc, 1, function(e){mod_prec_atomic(e[[1]], e[[2]])})

  # Get only n-grams that occur in the candidate
  fraction <- Reduce(
    function(acc, e) {
      return(list(
        nominator=(e$nominator + acc$nominator),
        denominator=(e$denominator + acc$denominator)))},
    sums,
    list(nominator=0, denominator=0))
  return(fraction$nominator / fraction$denominator)
}

bleu_c <- function(ref, cand, N = 4, weights = NA) {
  df <- construct_df(ref, cand)
  if (is.na(weights)) {
    weights <- rep(1, N) / N
  }
  mod_prec_n <- c()
  for (i in seq_len(N)) {
    df <- tokenize_df(df, n = i)
    indices <- tail(names(df), 2)
    if (i == 1){
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
