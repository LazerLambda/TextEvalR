

construct_df <- function(reference, candidate) {
  checkmate::check_character(candidate)
  checkmate::expect_list(reference, types = "character")
  df <- data.frame(cand = candidate)
  df$ref <- reference
  return(df)
}

tokenize_df <- function(df, n = 1, tokenize = TRUE) {
  checkmate::check_data_frame(df)
  if (!tokenize) {

  }
  col_name <- paste("ref_tok", as.character(n), sep = "_")
  df[col_name] <- list(apply(
    df,
    1,
    function(e) {
      return(tokenizers::tokenize_ngrams(e$ref, n = n))
    }
  ))
  col_name <- paste("cand_tok", as.character(n), sep = "_")
  df[col_name] <- list(apply(
    df,
    1,
    function(e) {
      return(tokenizers::tokenize_ngrams(e$cand, n = n))
    }
  ))
  return(df)
}

combine_n_grams <- function(ngrams, n) {
  checkmate::expect_atomic_vector(ngrams)
  if (n > length(ngrams)) {
    n <- length(ngrams)
    warning("`n` longer than length of tokens. `n` set to length of tokens.")
  }
  indices <- lapply(1:n, function(e) e:length(ngrams))
  tokens <- lapply(
    indices,
    function(e) {
      append(
        ngrams[e],
        rep(NA, length(ngrams) - length(e)))
    })
  tmp_df <- as.data.frame(tokens)
  tmp_df <- stats::na.omit(tmp_df)
  ngrams_raw <- apply(tmp_df, 1, function(e) list(unname(e)))
  new_ngrams <- lapply(ngrams_raw, function(e) do.call(paste, e))
  return(unname(new_ngrams))
}

construct_df <- function(reference, candidate) {
  checkmate::check_character(candidate)
  checkmate::expect_list(reference, types = "character")
  df <- data.frame(cand = candidate)
  df$ref <- reference
  return(df)
}

tokenize_df <- function(df, n = 1, tokenize = TRUE) {
  checkmate::check_data_frame(df)
  if (!tokenize) {
    df$ref_tok_1 <- df$ref
    df$ref_cand_1 <- df$cand
    return(df)
  }
  col_name <- paste("ref_tok", as.character(n), sep = "_")
  df[col_name] <- list(apply(
    df,
    1,
    function(e) {
      return(tokenizers::tokenize_ngrams(e$ref, n = n))
    }
  ))
  col_name <- paste("cand_tok", as.character(n), sep = "_")
  df[col_name] <- list(apply(
    df,
    1,
    function(e) {
      return(tokenizers::tokenize_ngrams(e$cand, n = n))
    }
  ))
  return(df)
}

combine_n_grams <- function(ngrams, n) {
  checkmate::expect_atomic_vector(ngrams)
  if (n > length(ngrams)) {
    n <- length(ngrams)
    warning("`n` longer than length of tokens. `n` set to length of tokens.")
  }
  indices <- lapply(1:n, function(e) e:length(ngrams))
  tokens <- lapply(
    indices,
    function(e) {
      append(
        ngrams[e],
        rep(NA, length(ngrams) - length(e)))
    })
  tmp_df <- as.data.frame(tokens)
  tmp_df <- stats::na.omit(tmp_df)
  ngrams_raw <- apply(tmp_df, 1, function(e) list(unname(e)))
  new_ngrams <- lapply(ngrams_raw, function(e) do.call(paste, e))
  return(unname(new_ngrams))
}
