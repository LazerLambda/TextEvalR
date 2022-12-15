library(checkmate)

#' Read Candidate Sentences.
#'
#' Helper function to conveniently read the candidate sentences from a file.
#' The file must be of the format in which each line refers to a
#' system's output. The function is a wrapper for the `readLines` function.
#'
#' @param path Absolute path to the candidates text file.
#' @returns Vector of candidate sentences.
#' @export
read_cand <- function(path) {
  checkmate::expect_character(path, max.len = 1)
  readLines(path)
}

#' Read Reference Sentences.
#'
#' Helper function to conveniently read the reference sentences from different
#' files. The files must be in the format so that each line refers to a
#' system's output.
#'
#' @param files List of filenames of reference files.
#' @param path Parent folder for files.
#' @returns List including vectors of reference sentences.
#' @export
read_ref <- function(files, path = ".") {
  checkmate::expect_character(path, max.len = 1)
  checkmate::expect_character(files)

  readFiles <- lapply(
    files,
    function(file) {
      path_tmp <- paste(path, file, sep = "/")
      readLines(path_tmp)
    }
  )
  matrix_tmp <- apply(
    as.data.frame(readFiles),
    1,
    function(row) c(unname(unlist(row)))
    )
  unname(as.list(as.data.frame(t(matrix_tmp))))
}
