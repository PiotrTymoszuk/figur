# OP for general data structures

# Regex search -------

#' Search an object by regular expression.
#'
#' @description Select elements of a vector or data frame records which match
#' the given regular expression.
#' @details `reglook()` is a S3 generic function with methods defined
#' for the `data_frame` and `default` class.
#' @param object an object to be searched.
#' @param regex a regular expression.
#' @param keys a vector of data frame variable names to be included
#' in the search. Defaults to NULL, which means that all variables will be
#' looked up.
#' @param multiple ignored when a single search key is provided.
#' It describes how searching results from multiple data frame columns will
#' be handled. 'OR' (default) specifies that the multiply column search results
#' will be merged by logical sum, for 'AND' they will be merged by logical
#' intersection.
#' @param ... extra arguments, currently none.
#' @return an object of the same class as the input one.
#' @export reglook.default
#' @export

  reglook.default <- function(object, regex, ...) {

    search_res <- search_regex(x = object, regex = regex)

    object[search_res]

  }

#' @rdname reglook.default
#' @export reglook.data.frame
#' @export

  reglook.data.frame <- function(object,
                                 regex,
                                 keys = NULL,
                                 multiple = c('OR', 'AND'), ...) {

    ## entry control ---------

    stopifnot(is.data.frame(object))
    stopifnot(is.character(regex))

    if(is.null(keys)) keys <- names(object)

    if(any(!keys %in% names(object))) {

      stop("some 'keys' are absent from the object's variables.",
           call. = FALSE)

    }

    multiple <- match.arg(multiple[1], c('OR', 'AND'))

    ## searching and merging ---------

    search_res <- purrr::map(object[keys],
                             search_regex,
                             regex = regex)

    if(length(search_res) == 1) {

      return(object[search_res[[1]], ])

    }

    search_res <- purrr::map(search_res,
                             ~ifelse(is.na(.x), FALSE, .x))

    search_res <-
      switch(multiple,
             OR = purrr::reduce(search_res, `|`),
             AND = purrr::reduce(search_res, `&`))

    return(object[search_res, ])

  }

# END -------
