# Coercion methods

# collate ------

#' @include imports.R
#' @include generics.R
#' @include classes.R

  NULL

# Figure object --------

#' Convert an ggplot into a figure object.
#'
#' @description
#' Bundles a ggplot/cowplot graphic, file reference, the final
#' rendered graphic dimensions and markdown references in one figure object.
#'
#' @inheritParams figure
#' @param ... extra arguments, currently none.
#' @details ref_name: needs to be a valid markdown/bookdown reference name. By
#' default, spaces, slashes and underscores are silently turned into '-'.
#'
#' @return An object of class \code{\link{figure}}.
#'
#' @export

  as_figure <- function(x, ...) UseMethod('as_figure')

#' @rdname as_figure
#' @export

  as_figure.ggplot <- function(x,
                               label,
                               w,
                               h,
                               unit = c('mm', 'cm', 'in'),
                               ref_name = NULL,
                               caption = NULL,
                               legend_text = NULL, ...) {

    stopifnot(is.ggplot(x))

    figure(x = x,
           label = label,
           w = w,
           h = h,
           unit = unit,
           ref_name = ref_name,
           caption = caption,
           legend_text = legend_text)

  }

# mdtable ---------

#' Convert a data frame into an mdtable object.
#'
#' @description
#' Bundles a data frame with its R markdown reference and caption
#' and file name for saving on the disc.
#'
#' @details
#' Technically, a data frame with label, ref_name
#' and caption attributes.
#' The \code{\link{mdtable}} class inherits from data frame most of its methods.
#' ref_name: needs to be a valid markdown/bookdown reference name. By
#' default, spaces, slashes and underscores are silently turned into '-'.
#'
#' @inheritParams mdtable
#' @param ... extra arguments. In case of matrices, extra arguments passed to
#' \code{\link[base]{as.data.frame}}
#'
#' @return An object of class \code{\link{mdtable}}.
#'
#' @export

  as_mdtable <- function(x, ...) {

    UseMethod('as_mdtable')

  }

#' @rdname as_mdtable
#' @export

  as_mdtable.data.frame <- function(x,
                                    label,
                                    ref_name,
                                    caption, ...) {

    stopifnot(is.data.frame(x))

    mdtable(x = x,
            label = label,
            ref_name = ref_name,
            caption = caption)

  }

#' @rdname as_mdtable
#' @export

  as_mdtable.matrix <- function(x,
                                label,
                                ref_name,
                                caption, ...) {

    stopifnot(is.matrix(x))

    mdtable(x = as.data.frame(x, ...),
            label = label,
            ref_name = ref_name,
            caption = caption)

  }

# mdbib ------

#' Convert an object to an mdbib class instance.
#'
#' @description Converts an object (usually an expression)
#' to an mdbib instance.
#'
#' @param x an object.
#' @param ... extra arguments passed to methods.
#'
#' @return an object of \code{\link{mdbib}} class.
#' @export

  as_mdbib <- function(x, ...) {

    UseMethod('as_mdbib')

  }

#' @rdname as_mdbib
#' @export

  as_mdbib.data.frame <- function(x, ...) {

    mdbib(x, ...)

  }

# END ------
