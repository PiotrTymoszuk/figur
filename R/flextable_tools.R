# Tools for flextable objects.

# Dimensions of the columns ------

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

    n_cols <- ncol_keys(x)

    if(length(widths) > n_cols) {

      stop(paste("The maximal 'widths' vector length is", n_cols),
           call. = FALSE)

    }

    ## modification --------

    for(i in seq_along(widths)) {

      x <- width(x, i, width = widths[i], unit = unit)

    }

    return(x)

  }

# Table save ---------

#' Save a list of `mdtables` as an Excel file.
#'
#' @description
#' Saves a list of \code{\link{mdtable}} objects as an Excel file. Each list
#' element is saved as a separate sheet and named with the
#' 'Table 1', 'Table 2', ... scheme. The first sheet named 'Cover' provides a
#' handy legend with the table name and its caption.
#'
#' @param tab_lst a list of `mdtable` data frames.
#' @param path path of to the Excel file.
#' @param prefix prefix used to name sheets of the Excel file.
#' @param ... extra arguments passed to \code{\link[writexl]{write_xlsx}}.
#'
#' @return none, called for the side effects.
#'
#' @export

  save_excel <- function(tab_lst,
                         path,
                         prefix = 'Table ', ...) {

    ## entry control ------

    error_txt <- "'tab_lst' has to be a named list of 'mdtable' data frames."

    if(!is.list(tab_lst)) stop(error_txt, call. = FALSE)

    if(is.null(names(tab_lst))) stop(error_txt, call. = FALSE)

    classes <- map_lgl(tab_lst, is_mdtable)

    if(any(!classes)) stop(error_txt, call. = FALSE)

    stopifnot(is.character(prefix))
    stopifnot(is.character(path))

    ## saves a list of mdtables on the disc -------

    cover <- map_chr(tab_lst, attr, 'caption')

    Table <- NULL
    Caption <- NULL

    cover <-
      tibble(Table = paste0(prefix, 1:length(tab_lst)),
             Caption = cover)

    tab_lst <- set_names(tab_lst, paste0(prefix, 1:length(tab_lst)))

    tab_lst <- c(list(Cover = cover), tab_lst)

    write_xlsx(tab_lst, path = path, ...)

  }

# END -------
