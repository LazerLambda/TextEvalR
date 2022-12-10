
#' Construct Dataframe.
#'
#' Construct Dataframe from references and candidates. References are to be
#' presented as a list of character vectors while candidates are required to be
#' character vectors. The reference list and the candidate vector must have the
#' same length. A Dataframe is constructed from this input.
#' @param reference List of references.
#' @param candidate Vector of candidate strings.
#' @return Dataframe, containing two columns for ref and candidate
#' in length of input.
construct_df <- function(reference, candidate) {
  checkmate::check_character(candidate)
  checkmate::expect_list(reference, types = "character")
  df <- data.frame(cand = candidate)
  df$ref <- reference
  return(df)
}

#' Process Dataframe.
#'
#' Tokenize input and append as column to df, furthermore add n-grams for each
#' n > 1 as a separate column. Only tokenizing for unigrams, for all other n > 1
#' tokens are only expanded to n-grams. Special case for BLEU on sentence level
#' to avoid treatment of vectors as strings.
#' @param df Dataframe, from `construct_df`.
#' @param n n-gram level.
#' @return Dataframe with new columns for requested n-gram.
process_df <- function(df, n = 1) {
  checkmate::check_data_frame(df)
  if (n == 1) {
    df["ref_tok_1"] <- list(apply(
      df,
      1,
      function(e) {
        return(tokenizers::tokenize_ngrams(e$ref, n = 1))
      }))
    df["cand_tok_1"] <- list(apply(
      df,
      1,
      function(e) {
        return(tokenizers::tokenize_ngrams(e$cand, n = 1))
      }
    ))
  } else {
    # Reference
    i_ref <- which(names(df) == "ref_tok_1")
    col_name <- paste("ref_tok", as.character(n), sep = "_")
    df[col_name] <- list(apply(
      df,
      1,
      function(e) {
        return(combine_n_grams_list(e[[i_ref]], n = n))
      }
    ))
    # Candidate
    i_cand <- which(names(df) == "cand_tok_1")
    col_name <- paste("cand_tok", as.character(n), sep = "_")
    if (nrow(df) == 1) {
      # Avoid error regarding list truncation
      df[col_name] <- list(apply(
        df,
        1,
        function(e) {
          return(combine_n_grams_list((e[[i_cand]]), n = n))
        }
      ))
    } else {
      df[col_name] <- list(apply(
        df,
        1,
        function(e) {
          return(combine_n_grams(unlist(e[[i_cand]]), n = n))
        }
      ))
    }
  }
  return(df)
}

#' N-Gram Wrapper.
#'
#' Call function `combine_n_grams` for each entry in list.
#' @param ngrams_l List of tokens to expand into n-grams.
#' @param n Parameter to determine range of n-grams.
#' @return List of vectors containing requested n-grams.
combine_n_grams_list <- function(ngrams_l, n) {
  checkmate::expect_list(ngrams_l, types = c("character"))
  return_val <- lapply(ngrams_l, function(e) combine_n_grams(e, n))
  return(return_val)
}

#' Get n-grams from Tokens.
#'
#' Collapse tokens to n-grams for list of atomic Strings. Create three lists of
#' tokens in which sequentially the first token is the subsequently following
#' token of the first token of the previous list. A data.frame is constructed
#' based on this list and combined to the requested n-grams.
#'
#' @param ngrams Vector of tokens.
#' @param n Parameter to determine range of n-grams.
#' @return Vector of strings entailing requested n-grams.
combine_n_grams <- function(ngrams, n) {
  checkmate::expect_atomic_vector(ngrams)
  if (n > length(ngrams)) {
    n <- length(ngrams)
    warning("`n` longer than length of tokens. `n` set to length of tokens.")
  }

  # Create list of sequences to index token
  indices <- lapply(1:n, function(e) e:length(ngrams))
  # Create n lists with shifted token list to combine to n-gram
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
  # Transform list of words to an n-gram string
  new_ngrams <- lapply(
    ngrams_raw,
    function(e) do.call(paste, list(e[[1]], collapse = " ")))
  # Transform into a vector of strings
  return(unlist(unname(new_ngrams)))
}
