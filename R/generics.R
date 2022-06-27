# S3 Generics

# type conversion ------

#' Convert and object to a figure class instance.
#'
#' @description Converts an object (in most cases a graphic object)
#' to a figure object.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return an object of figure class (\code{\link{figure}})
#' @export

  as_figure <- function(x, ...) {

    UseMethod('as_figure', x)

  }

#' Convert and object to an mdtable class instance.
#'
#' @description Converts an object (table or matrix in most cases) to an
#' mdtable instance.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return an object of mdtable class (\code{\link{mdtable}})
#' @export

  as_mdtable <- function(x, ...) {

    UseMethod('as_mdtable', x)

  }

#' Convert an object to an mdexpr class instance.
#'
#' @description Converts an object (usually an expression)
#' to an mdexpr instance.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return an object of mdtable class (\code{\link{mdtable}})
#' @export

  as_mdexpr <- function(x, ...) {

    UseMethod('as_mdexpr', x)

  }

# modification of object features -----

#' Extract object features.
#'
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The requested feature
#' @export

  extract <- function(x, ...) {

    UseMethod('extract', x)

  }

#' Convert plot units.
#'
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object with the value converted to the requested unit
#' @export

  convert <- function(x, ...) {

    UseMethod('convert', x)

  }

#' Get object height.
#'
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object's height
#' @export

  height <- function(x, ...) {

    UseMethod('height', x)

  }

#' Get object width.
#'
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object's width
#' @export

  width <- function(x, ...) {

    UseMethod('width', x)

  }

#' Set object height.
#'
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object with the modified h property
#' @export

  `height<-` <- function(x, ...) {

    UseMethod('height<-', x)

  }

#' Set object width.
#'
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object with the modified w property
#' @export

  `width<-` <- function(x, ...) {

    UseMethod('width<-', x)

  }

# object reference in markdown --------

#' Insert an object chunk into an R markdown file
#'
#' @description Inserts a reference to the object as an R code chunk into
#' a R markdown file or prints it into the standard output.
#' @param object an object to be referenced.
#' @param ... extra arguments passed to methods.
#' @export

  insert <- function(object, ...) {

    UseMethod('insert', object)

  }

#' Insert an object citation/reference into an R markdown file
#'
#' @description Inserts a reference to the object as an inline markdown
#' reference or prints it into the standard output.
#' @param object am object to be referenced.
#' @param .... extra arguments passed to methods.
#' @export

  refer <- function(object, ...) {

    UseMethod('refer', object)

  }

# saving on the disc ------

#' Save and object on the disc.
#'
#' @description Saves an object on the disc.
#' @param object an object.
#' @param ... extra arguments passed to methods.
#' @export

  pickle <- function(object, ...) {

    UseMethod('pickle', object)

  }

# END -----
