# Provides functions for resizing and saving/rendering

# resizing ------

#' Resize a figure object.
#'
#' @param figure_object A figure object
#' @param value The new dimension value
#' @param what The requested dimension to be changed or the scaling factor
#' @param unit The dimension unit of the final plot object
#' @param lock If TRUE, the aspect ratio is kept constant (scaling), ignored if 'what' is set to 'scale'
#' @return A figure object with the modified dimensions and (optionally) unit
#' @export

  resize <- function(figure_object,
                     value,
                     what = c('w', 'h', 'scale'),
                     unit = extract(figure_object, 'unit'),
                     lock = TRUE) {

    if(!is_figure(figure_object)) stop("An object of class 'figure' is required", call. = FALSE)
    if(!is.numeric(value)) stop('The value argument needs to be a number', call. = FALSE)

    what <- match.arg(what[1], c('w', 'h', 'scale'))
    unit <- match.arg(unit[1], c('mm', 'cm', 'in'))

    if(unit != figure_object$unit) {

      figure_object <- convert(figure_object, to = unit)

    }

    if(what == 'scale') {

      new_h <- height(figure_object) * value

      height(figure_object, lock = TRUE) <- new_h

    } else if(what == 'w') {

      height(figure_object, lock = lock) <- value

    } else {

      width(figure_object, lock = lock) <- value

    }

    figure_object

  }

# saving -------

#' Save a figure object on the disc
#'
#' @param figure_object A figure object
#' @param path The target path. The destination folder will not be created
#' @param format the desired file format.
#' @param ... extra arguments passed to \code{\link[ggplot2]{ggsave}}.
#' @return None, called for side effects
#' @export

  save_figure <- function(figure_object,
                          path = '.',
                          format = 'pdf', ...) {

    if(!is_figure(figure_object)) {

      stop("An object of class 'figure' is required", call. = FALSE)

    }

    if(!dir.exists(path)) stop('The target_path does not exist', call. = FALSE)

    filename <- paste(extract(figure_object, 'label'), format, sep = '.')

    message(paste('Saving:',
                  filename,
                  'to',
                  path))

    ggplot2::ggsave(filename = filename,
                    plot = plot(figure_object),
                    path = path,
                    width = width(figure_object),
                    height = height(figure_object),
                    units = extract(figure_object, 'unit'), ...)

  }

#' Save an md table object on the disc
#'
#' @param mdtable_object A mdtable object.
#' @param folder The target folder. The destination folder will not be created.
#' @param format the desired file format.
#' @param delim delimiter for clumns separation, tabulation by default.
#' @param ... extra arguments passed to \code{\link[readr]{write_delim}}.
#' @return None, called for side effects
#' @export

  save_mdtable <- function(mdtable_object,
                           folder = '.',
                           format = 'tsv',
                           delim = '\t', ...) {

    if(!is_mdtable(mdtable_object)) {

      stop("An object of class 'mdtable' is required", call. = FALSE)

    }

    if(!dir.exists(folder)) stop('The target_path does not exist', call. = FALSE)

    filename <- paste(attr(mdtable_object, 'label'), format, sep = '.')

    message(paste('Saving:',
                  filename,
                  'to',
                  folder))

    path <- paste(folder, filename, sep = '/')

    readr::write_delim(x = mdtable_object, file = path, delim = delim, ...)

  }

# END -----
