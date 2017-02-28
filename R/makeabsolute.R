#' Ensure absolute path is given
#'
#' @param path
#'
makeabsolute <- function(path) {

  # 1. Check to vector length, does not work > 1
  if (length(path) > 1) stop("makeabsolute only works on one entry.")

  # 2. Check that it exists as specified.
  path <- gsub("(/|\\\\)$", "", path)
  if (!file.exists(path)) stop("The directory specified in 'path', ",
    path, ", does not exit.")

  # 3. Ensure that an absolute path was given.
  if (regexpr("^.:(/|\\\\)", path) == -1L) {
    # Make the path absolute rather than relative
    if (identical(normalizePath(path, mustWork = FALSE), normalizePath(file.path(getwd(), path)))) {
      path <- normalizePath(path)
    } else {
      stop("The path specified in 'path', ", path, ",\nwas relative",
        " rather than absolute, \nand cannot be made absolute given your",
        " current working directory.")
    }
  }

  return(path)
}
