# Tools for flextable objects.

# Dimensiona of the columns ------

#' Set column widths in a flextable.
#'
#' @description
#' Sets widths of columns in a `flextable` object.
#'
#' @return a `flextable` class object.
#'
#' @param x a `flextable` object.
#' @param widths a numeric vector with widths of the columns, begining from
#' the first one.
#' @param unit unit of width, one of 'cm' (default), 'mm' or 'in'.
#'
#' @export

  set_widths <- function(x, widths, unit = c('cm', 'mm', 'in')) {

    ## entry control -------

    if(!inherits(x, 'flextable')) {

      stop("'x' has to be a flextable class object.", call. = FALSE)

    }

    unit <- match.arg(unit[1], c('cm', 'mm', 'in'))

    if(!is.numeric(widths)) {

      stop("'widths' has to be a numeric vector.", call. = FALSE)

    }

    n_cols <- flextable::ncol_keys(x)

    if(length(widths) > n_cols) {

      stop(paste("The maximal 'widths' vector length is", n_cols),
           call. = FALSE)

    }

    ## modification --------

    for(i in seq_along(widths)) {

      x <- flextable::width(x, i, width = widths[i], unit = unit)

    }

    return(x)

  }

# END -------
