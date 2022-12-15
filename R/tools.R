library(checkmate)

read_cand <- function(path) {
  checkmate::expect_character(path, max.len = 1)
  readLines(path)
}

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
