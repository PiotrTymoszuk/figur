# Provides functions for resizing and saving/rendering

# saving -------

#' Save a figure object on the disc
#'
#' @param figure_object A figure object
#' @param path The target path. The destination folder will not be created
#' @param format the desired file format.
#' @param ... extra arguments passed to \code{\link[ggplot2]{ggsave}}.
#'
#' @return None, called for side effects

  save_figure <- function(figure_object,
                          path = '.',
                          format = 'pdf', ...) {

    if(!is_figure(figure_object)) {

      stop("An object of class 'figure' is required", call. = FALSE)

    }

    if(!dir.exists(path)) stop('The target_path does not exist', call. = FALSE)

    filename <- paste(components(figure_object, 'label'), format, sep = '.')

    message(paste('Saving:',
                  filename,
                  'to',
                  path))

    ggsave(filename = filename,
           plot = plot(figure_object),
           path = path,
           width = width(figure_object),
           height = height(figure_object),
           units = components(figure_object, 'unit'), ...)

  }

#' Save an md table object on the disc
#'
#' @param mdtable_object A mdtable object.
#' @param folder The target folder. The destination folder will not be created.
#' @param format the desired file format.
#' @param delim delimiter for column separation, tabulation by default.
#' @param ... extra arguments passed to \code{\link[readr]{write_delim}}.
#'
#' @return None, called for side effects

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

    write_delim(x = mdtable_object, file = path, delim = delim, ...)

  }

# END -----
