# Non-exported multi-purpose utilities

# Regex search --------

#' Search a vector with a regular expression.
#'
#' @description Searches a vector (character or list) with a regular expression.
#' @param x a vector.
#' @param regex a regular expression.
#' @param ... additional arguments passed to \code{\link[stringi]{stri_detect}}.
#' @return a logical vector.

  search_regex <- function(x, regex, ...) {

    if(!is.list(x)) {

      return(stringi::stri_detect(str = x, regex = regex, ...))

    }

    search_res <- purrr::map(x,
                             stringi::stri_detect,
                             regex = regex, ...)


    return(purrr::map_lgl(search_res, any))

  }

# END ------
