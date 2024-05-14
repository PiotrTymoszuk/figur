# S3 Generics


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
