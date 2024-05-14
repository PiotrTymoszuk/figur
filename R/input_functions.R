# Functions used for import of bibliography and other objects

# Bibliography ------

#' Read bibliography from a text file.
#'
#' @description Reads a BibTex file from the disc and converts it to an
#' \code{\link{mdbib}} class object.
#'
#' @details Technically, the function employs the \code{\link[bib2df]{bib2df}}
#' function.
#'
#' @param file character, path or URL to a .bib file.
#' @param ... extra arguments passed to \code{\link[bib2df]{bib2df}}.
#'
#' @return an \code{\link{mdbib}} class object.
#'
#' @export

  read_bib <- function(file, ...) {

    df <- bib2df(file, ...)

    mdbib(df)

  }

# END ------
