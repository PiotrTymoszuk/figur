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

    UseMethod('as_figure')

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

    UseMethod('as_mdtable')

  }

#' Convert an object to an mdexpr class instance.
#'
#' @description Converts an object (usually an expression)
#' to an mdexpr instance.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return an object of mdexpr class (\code{\link{mdexpr}})
#' @export

  as_mdexpr <- function(x, ...) {

    UseMethod('as_mdexpr')

  }

#' Convert an object to an mdbib class instance.
#'
#' @description Converts an object (usually an expression)
#' to an mdbib instance.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return an object of mdbib class (\code{\link{mdbib}})
#' @export

  as_mdbib <- function(x, ...) {

    UseMethod('as_mdbib')

  }

# modification of object features -----

#' @rdname convert.numeric
#' @export

  convert <- function(x, ...) {

    UseMethod('convert')

  }

# Change of the object size -------

#' @rdname height.figure
#' @export

  `height<-` <- function(x, value) {

    UseMethod('height<-')

  }

  #' @rdname height.figure
  #' @export

  `width<-` <- function(x, value) {

    UseMethod('width<-')

  }

#' @rdname height.figure
#' @export

  height <- function(x, ...) {

    UseMethod('height')

  }

#' @rdname height.figure
#' @export

  width <- function(x, ...) {

    UseMethod('width', x)

  }

#' @rdname height.figure
#' @export

  resize <- function(x, ...) {

    UseMethod('resize')

  }


# object reference in markdown --------

#' @rdname insert.figure
#' @export

  insert <- function(object, ...) {

    UseMethod('insert', object)

  }

#' @rdname refer.figure
#' @export

  refer <- function(object, ...) {

    UseMethod('refer', object)

  }

# saving on the disc ------

#' @rdname pickle.figure
#' @export

  pickle <- function(object, ...) {

    UseMethod('pickle', object)

  }

# searching ------

#' @rdname reglook.default
#' @export

  reglook <- function(object, ...) {

    UseMethod('reglook')

  }

# END -----
