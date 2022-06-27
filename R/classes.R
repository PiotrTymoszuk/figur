# class definitions

# figure, mdtable and mdexpr S3 object constructors -----

#' Generate a figure object.
#'
#' @description Bundles a ggplot/cowplot object, file reference, the final
#' rendered graphic dimensions and markdown references in one figure object.
#' @param x a ggplot/cowplot object
#' @param label the final file name
#' @param w the final width after saving/rendering
#' @param h the final height after saving/rendering
#' @param unit the unit of h and w, mm be default
#' @param ref_name optional, name of the reference to the figure,
#' used mainly in markdown documents.
#' @param caption optional, text to be presented in the figure caption.
#' @param legend optional, text to be presented in the extended figure legend.
#' @details ref_name: needs to be a valid markdown/bookdown reference name. By
#' default, spaces, slashes and underscores are silently turned into '-'.
#' @return An object of class 'figure'
#' @export

  figure <- function(x,
                     label,
                     w,
                     h,
                     unit = c('mm', 'cm', 'in'),
                     ref_name = NULL,
                     caption = NULL,
                     legend_text = NULL) {

    if(!ggplot2::is.ggplot(x)) stop("An object of class 'ggplot' is required", call. = FALSE)
    if(!is.numeric(w)) stop('The w argument needs to be a number', call. = FALSE)
    if(!is.numeric(h)) stop('The h argument needs to be a number', call. = FALSE)

    unit  <- match.arg(unit[1], c('mm', 'cm', 'in'))

    if(!is.null(ref_name)) {

      ref_name <- stringi::stri_replace_all(ref_name,
                                            regex = '\\s|\\|/|_',
                                            replacement = '-')

    }

    figure_obj <- list(plot = x,
                       label = as.character(label),
                       h = h,
                       w = w,
                       unit = unit,
                       ref_name = ref_name,
                       caption = caption,
                       legend_text = legend_text)

    structure(figure_obj,
              class = 'figure')

  }

#' Generate an mdtable object.
#'
#' @description Bundles a data frame with its R markdown reference and caption
#' and file name for saving on the disc.
#' @details Technically, a data frame with label, ref_name
#' and caption attributes.
#' The mdtable inherits from data frame most of its methods.
#' ref_name: needs to be a valid markdown/bookdown reference name. By
#' default, spaces, slashes and underscores are silently turned into '-'.
#' @param x a data frame or tibble.
#' @param label the final file name.
#' @param ref_name name of the reference to the table,
#' used mainly in markdown documents.
#' @param caption text to be presented in the table caption.
#' @return An object of class 'mdtable'
#' @export

  mdtable <- function(x,
                      label,
                      ref_name,
                      caption) {

    if(!is.data.frame(x)) {

      stop('A valid data frame is required.', call. = FALSE)

    }

    ref_name <- stringi::stri_replace_all(ref_name,
                                          regex = '\\s|\\|/|_',
                                          replacement = '-')

    attr(x, 'label') <- label
    attr(x, 'ref_name') <- ref_name
    attr(x, 'caption') <- caption

    structure(x, class = c('mdtable', class(x)))


  }

#' Generate an mdexpr object.
#'
#' @description Generates an mdexpression object which bundles an R expression
#' with its evaluation result and enables seamless insertion
#' into an R markdown document.
#' @details Throws any errors and warnings encountered during the evaluation.
#' In addition checks if the result is NULL, NA or Inf and raises a warning
#' if this is the case.
#' @param x an expression.
#' @param ref_name name of the reference to the code block,
#' used mainly in markdown documents (waiting for a bookdown implementation).
#' @param caption text to be presented in the table caption, currently
#' no implementation in bookdown
#' (https://github.com/rstudio/bookdown/issues/238)
#' @param ... extra arguments, currently none.
#' @details ref_name: needs to be a valid markdown/bookdown reference name. By
#' default, spaces, slashes and underscores are silently turned into '-'.
#' @return an object of mdexpr class.
#' @export

  mdexpr <- function(x,
                     ref_name = NULL,
                     caption = NULL, ...) {

    quo_call <- rlang::enquo(x)

    quo_res <- rlang::eval_tidy(quo_call)

    if(any(is.null(quo_res))) {

      warning('The object evaluation result is NULL!', call. = FALSE)

    }

    if(any(is.na(quo_res))) {

      warning('The object evaluation result is NA!', call. = FALSE)

    }

    if(any(is.infinite(quo_res))) {

      warning('The object evaluation result is infinite!', call. = FALSE)

    }

    if(!is.null(ref_name)) {

      ref_name <- stringi::stri_replace_all(ref_name,
                                            regex = '\\s|\\|/|_',
                                            replacement = '-')

    }

    structure(list(quosure = quo_call,
                   result = quo_res,
                   ref_name = ref_name,
                   caption = caption),
              class = 'mdexpr')

  }

# S3 class checker -----

#' Check for a figure object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'figure' class object
#' @export

  is_figure <- function(x) {

    inherits(x, 'figure')

  }

#' Check for a mdtable object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'figure' class object
#' @export

  is_mdtable <- function(x) {

    inherits(x, 'mdtable')

  }

#' Check for a mdexpr object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'figure' class object
#' @export

  is_mdexpr <- function(x) {

    inherits(x, 'mdexpr')

  }

# END ------
