# Multi-purpose tools

# Re-naming of a figure list ---------

#' Set consecutive figure numbers in a list of `figure` objects.
#'
#' @description
#' Appends labels of `figure` objects in a list with consecutive numbers (e.g.
#' 'figure_1_...', 'figure_2_...').
#'
#' @param figure_lst a list of \code{\link{figure}} objects.
#' @param prefix a character string that precedes the figure number.
#'
#' @return a list of \code{\link{figure}} objects.
#'
#' @export

  number_figures <- function(figure_lst,
                             prefix = 'figure_') {

    ## entry control -------

    stopifnot(is.character(prefix))

    error_txt <- "'figure_lst' has to be a list of 'figure' objects."

    if(!is.list(figure_lst)) stop(error_txt, call. = FALSE)

    classes <- map_lgl(figure_lst, is_figure)

    if(any(!classes)) stop(error_txt, call. = FALSE)

    ## sets consecutive numbers in labels of figure objects in a list ------

    regex_txt <- paste0('^', prefix, '_\\d+_')

    for(i in seq_along(figure_lst)) {

      if(stri_detect(figure_lst[[i]]$label, regex = regex_txt)) {

        figure_lst[[i]]$label <-
          stri_replace(figure_lst[[i]]$label,
                       regex = regex_txt,
                       replacement = '')

      }

      figure_lst[[i]]$label <-
        paste0(prefix, i, '_', figure_lst[[i]]$label)

    }

    figure_lst

  }

# HTML text -------

#' Ready-to-use HTML expressions.
#'
#' @description
#' This function set generates HTML expressions which may be used in e.g. in
#' titles, labels and text of `ggplot` objects in combination with `ggtext`
#' package's `element_markdown`.
#'
#' @param x a character string or an object which can be converted to a
#' character string.
#'
#' @return a string
#'
#' @export

  html_italic <- function(x) paste0('<em>', x, '</em>')

#' @rdname html_italic
#' @export

  html_bold <- function(x) paste0('<b>', x, '</b>')

#' @rdname html_italic
#' @export

  html_sup <- function(x) paste0('<sup>', x, '</sup>')

#' @rdname html_italic
#' @export

  html_sub <- function(x) paste0('<sub>', x, '</sub>')

# Text formatting helpers -------

#' Collapse elements of a text vector with comma/and.
#'
#' @description
#' Collapses elements of a text vector with commas and inserts 'and' before
#' the last element.
#'
#' @return a character string.
#'
#' @param x a text vector or a vector coercible to a character.
#' @param oxford logical, should the 'and' be placed according to the Oxford
#' comma rule?
#'
#' @export

  collapse_and <- function(x, oxford = TRUE) {

    ## entry control

    x <- try(as.character(x), silent = TRUE)

    if(inherits(x, 'try-error')) {

      stop("'x' must be coercible to a character vector.", call. = FALSE)

    }

    stopifnot(is.logical(oxford))

    ## collapsing

    n <- length(x)

    if(n == 1) return(x)

    if(n == 2) return(paste(x[1], x[2], sep = ' and '))

    if(oxford) {

      separator <- ", and "

    } else {

      separator <- " and "

    }

    result <- paste(x[-n], collapse = ', ')

    result <- paste(result, x[n], sep = separator)

    result

  }

# END ------
