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
#' @param legend_text optional, text to be presented in
#' the extended figure legend.
#' @details ref_name: needs to be a valid markdown/bookdown reference name. By
#' default, spaces, slashes and underscores are silently turned into '-'.
#' You may easily insert the read-to-use figure object code chunk into your
#' Rmarkdown document with the `insert()` method or reference it in the text
#' by calling `refer()`.
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
#' You may easily insert the read-to-use table object code chunk into your
#' Rmarkdown document with the `insert()` method or reference it in the text
#' by calling `refer()`.
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
#' You may easily insert the read-to-use R code chunk stored in the mdexpr
#' object into your Rmarkdown document with the `insert()` or `refer()` method.
#' @param x an expression.
#' @param ref_name name of the reference to the code block,
#' used mainly in markdown documents (waiting for a bookdown implementation).
#' @param caption text to be presented in the code chunk caption, currently
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

# link and html tag S3 object constructors -------

#' Generate an mdlink object.
#'
#' @description Generates an mdlink object which holds together
#' the URL and tile of a link.
#' @details Particularly useful for link used multiple times
#' in an Rmarkdown document. You may easily insert them (e.g. via copy and paste)
#' by calling the `insert()` or `refer()` method.
#' @param x a URL
#' @param ref_name a link title
#' @export

  mdlink <- function(x, ref_name) {

    if(!is.character(x)) {

      stop("'x' hast to be a character.", call. = FALSE)

    }

    if(!is.character(ref_name)) {

      stop("'ref_name' hast to be a character.", call. = FALSE)

    }

    structure(list(URL = x,
                   ref_name = ref_name),
              class = 'mdlink')

  }

#' Generate an mdhtml object.
#'
#' @description Generates an mdhtml object which stores a HTML or XTML tag
#' @details Particularly useful for custom HTML element used multiple times
#' in an Rmarkdown document. You may easily insert them (e.g. via copy and paste)
#' by calling the `insert()` or `refer()` method. Technically,
#' it requires a string starting with an `<tag>` or `<tag />` and ending with
#' an `</tag>` or `<tag />`.
#' @param x a string to be stored as a HTML or XTML tag, see the details!
#' @export

  mdhtml <- function(x) {

    ## entry control ---------

    if(!is.character(x)) {

      stop("'x' hast to be a character.", call. = FALSE)

    }

    if(!stringi::stri_detect(x, regex = '^<.*>')) {

      stop("The string provided as 'x' is unlikely a valid HTML tag.",
           call. = FALSE)

    }

    if(!stringi::stri_detect(x, regex = '(</.*>)|(<.*\\s{1}/>)$')) {

      stop("The string provided as 'x' is unlikely a valid HTML tag.",
           call. = FALSE)

    }

    structure(x,
              class = c('character', 'mdhtml'))


  }

# text S3 object constructors --------

#' Create an mdtext container.
#'
#' @description Creates a container for custom text to be (re-) used in the
#' Rmarkdown document.
#' @details Storing some fixed text parts used multiple times in the Rmarkdown
#' document (e.g. parts of figure legends) as mdtext provides a smarter
#' alternative to the tarditional 'copy-paste' approach.
#' You may insert the text chunk in your document by calling the
#' `insert()` or `refer()` method.
#' @param x a string to be stored as mdtext object.
#' @export

  mdtext <- function(x) {

    x <- as.character(x)

    structure(x,
              class = c('character', 'mdtext'))


  }

# mdbib object constructor -------

#' Create an mdbib object storing document's bibliography
#'
#' @description Creates an mdbib object storing bibliography information
#' (preferably derived from a BibTex file). Takes a data frame or a similar
#' structure. The variable named 'BIBTEXKEY' is required.
#' @details The object is built on the top of a data frame and inherits
#' multiple functions/methods from the 'data_frame' class.
#' @param x a data frame. It has to contain the 'BIBTEXKEY' variable.
#' @param ... additional arguments, currently none defined.
#' @return an object of the 'mdbib' class.
#' @export

  mdbib <- function(x, ...) {

    ## entry control --------

    if(!is.data.frame(x)) {

      x <- try(as.data.frame(x), silent = TRUE)

      if(inherits(x, 'try-error')) {

        stop("'x' cannot be converted to a data frame.", call. = FALSE)

      }

    }

    if(!'BIBTEXKEY' %in% names(x)) {

      stop("'x' has to have a column named 'BIBTEXKEY'")

    }

    ## output -------

    structure(x,
              class = c(class(x), 'mdbib'))

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
#' @return Logical, TRUE if the 'mdtable' class object provided.
#' @export

  is_mdtable <- function(x) {

    inherits(x, 'mdtable')

  }

#' Check for a mdexpr object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'mdexpr' class object provided.
#' @export

  is_mdexpr <- function(x) {

    inherits(x, 'mdexpr')

  }

#' Check for a mdlink object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'mdlink' class object provided.
#' @export

  is_mdlink <- function(x) {

    inherits(x, 'mdlink')

  }

#' Check for a mdhtml object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'mdhtml' class object provided.
#' @export

  is_mdhtml <- function(x) {

    inherits(x, 'mdhtml')

  }

#' Check for a mdtext object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'mdtext' class object provided.
#' @export

  is_mdtext <- function(x) {

    inherits(x, 'mdtext')

  }

#' Check for a mdbib object.
#'
#' @param x An object to test
#' @return Logical, TRUE if the 'mdbib' class object provided.
#' @export

  is_mdbib <- function(x) {

    inherits(x, 'mdbib')

  }

# END ------
