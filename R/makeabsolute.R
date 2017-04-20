#' Ensure absolute path is given
#'
#' @param path A path that works from your current directory,
#' i.e., either an absolute path that exists or a relative path
#' that exists given your current directory.
#' @param trailer A character value, \code{NULL}, or \code{TRUE},
#' where the first will end the path with the folder name and the
#' latter two will end the path with slashes appropriate for your
#' operating system, if \code{TRUE}, or however you wish, if
#' \code{"/"}, or some other entry.
#'
makeabsolute <- function(path, trailer = NULL) {

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

  # 4. If you need to use paste on a directory later with a filename
  # then you must add a trailer.
  if (!is.null(trailer)) {
    if (trailer == TRUE) {
      path <- normalizePath(paste0(path, .Platform$file.sep))
    } else {
      path <- normalizePath(paste0(path, trailer))
    }
  }


  return(path)
}
