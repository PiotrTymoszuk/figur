# S3 methods for the figure, mdtable and mdexpr class

# extracting and printing figure object features -----

#' Extract features of a figure object.
#'
#' @param x A figure object
#' @param what The requested feature.
#' 'plot': the ggplot,
#' 'label': the rendering/saving label,
#' 'h': rendering height,
#' 'w': the rendering width,
#' 'unit': the rendering unit,
#' 'ref_name': reference name used in R markdown,
#' 'caption' object reference caption used in R markdown,
#' 'legend' extended legend text used in R markdown.
#' @param ... extra arguments, currently none.
#' @return The requested feature
#' @export

  extract.figure <- function(x,
                             what = c('plot', 'label',
                                      'w', 'h', 'unit',
                                      'ref_name', 'caption', 'legend'), ...) {

    stopifnot(is_figure(x))

    what <- match.arg(what[1],
                      c('plot', 'label',
                        'w', 'h', 'unit',
                        'ref_name', 'caption', 'legend'))

    x[[what]]

  }

#' Print figure object properties.
#'
#' @param x A figure object
#' @return The figure's label and dimensions
#' @export

  print.figure <- function(x, ...) {

    stopifnot(is_figure(x))

    cat(paste0(extract(x, 'label'),
               ', Width: ', extract(x, 'w'), ' ', extract(x, 'unit'),
               ', Height: ', extract(x, 'h'), ' ', extract(x, 'unit')))

  }

#' Plot a figure object.
#'
#' @param x A figure object
#' @return The figure's ggplot object
#' @export

  plot.figure <- function(x, ...) {

    stopifnot(is_figure(x))

    extract(x, 'plot')

  }

#' Get figure object's height.
#'
#' @param x A figure object
#' @return The figure's height
#' @export

  height.figure <- function(x, ...) {

    stopifnot(is_figure(x))

    extract(x, 'h')

  }

#' Get figure object's width.
#'
#' @param x A figure object
#' @return The figure's width
#' @export

  width.figure <- function(x, ...) {

    stopifnot(is_figure(x), ...)

    extract(x, 'w')

  }

# extracting the mdtable object features ------

#' Extract features of an mdtable object.
#'
#' @param x An mdtable object
#' @param what The requested feature.
#' 'label': the rendering/saving label,
#' 'ref_name': reference name used in R markdown,
#' 'caption' object reference caption used in R markdown,
#' @param ... extra arguments, currently none.
#' @return The requested feature
#' @export

  extract.mdtable <- function(x,
                              what = c('label',
                                       'ref_name',
                                       'caption'), ...) {

    stopifnot(is_mdtable(x))

    what <- match.arg(what[1],
                      c('label', 'ref_name', 'caption'))

    attr(x, what)

  }

# extracting the mdexpr features and printing -----

#' Extract features of an mdexpr object.
#'
#' @param x An mdexpr object
#' @param what The requested feature.
#' 'quosure': the quosure,
#' 'expression': expression,
#' 'result': the evaluation result,
#' 'ref_name': reference name used in R markdown,
#' 'caption' object reference caption used in R markdown,
#' @param ... extra arguments, currently none.
#' @return The requested feature
#' @export

  extract.mdexpr <- function(x,
                             what = c('quosure', 'expression', 'result',
                                      'ref_name', 'caption'), ...) {

    stopifnot(is_mdexpr(x))

    what <- match.arg(what,
                      c('quosure', 'expression', 'result',
                        'ref_name', 'caption'))

    if(what == 'expression') {

      return(as.expression(rlang::quo_get_expr(x$quosure)))

    }

    x[[what]]

  }

#' Print as mdexpr object.
#'
#' @description Prints the expression and evaluation result of an mdexpr
#' object.
#' @param x an mdexpr object.
#' @param ... extra arguments, currently none.
#' @export

  print.mdexpr <- function(x, ...) {

    expr <- as.expression(rlang::quo_get_expr(x$quosure))

    cat(paste0('mdexpr: {', as.character(expr), '} = ', x$result))

  }

# extracting and printing for the mdlink objects -------

#' Extract features of an mdlink object.
#'
#' @param x an mdlink object.
#' @param what feature to be extracted:
#' 'ref_name': reference name used in R markdown or
#' 'URL': the link's URL.
#' @param ... extra arguments, currently none.
#' @export

  extract.mdlink <- function(x,
                            what = c('ref_name', 'URL'), ...) {

    stopifnot(is_mdlink(x))

    x[[what]]

  }

#' Print an mdlink object.
#'
#' @description displays an mdlink object.
#' @param x an mdlink object.
#' @param ... extra arguments, currently none.
#' @export

  print.mdlink <- function(x, ...) {

    stopifnot(is_mdlink(x))

    cat(paste(x[['ref_name']],
              x[['URL']],
              sep = ': '))

  }


# figure S3 methods: setting width and height -----

#' Set figure object height.
#'
#' @param x A figure object
#' @param value New height
#' @param lock If TRUE, the aspect ratio is kept constant (scaling)
#' @return A figure object with the height property modified
#' @export

  `height<-` <- function(x, value, lock = FALSE) {

    if(lock) {

      old_w <- width(x)
      old_h <- height(x)

      width(x) <- old_w * value/old_h

    }

    x$h <- value

    x

  }

#' Set figure object width.
#'
#' @param x A figure object
#' @param value New height
#' @param lock If TRUE, the aspect ratio is kept constant (scaling)
#' @return A figure object with the height property modified
#' @export

  `width<-` <- function(x, value, lock = FALSE) {

    if(lock) {

      old_w <- width(x)
      old_h <- height(x)

      height(x) <- old_h * value/old_w

    }

    x$w <- value

    x

  }

# figure S3 methods: unit conversion -----

#' Convert plot units.
#'
#' @param x A numeric vector
#' @param from The input unit
#' @param to The target unit
#' @return A numeric vector after the unit conversion
#' @examples
#' convert(10, 'mm', 'cm')
#' convert(180, 'mm', 'in')
#' convert(7.086614, 'in', 'mm')
#' @export

  convert.numeric <- function(x,
                              from = c('mm', 'cm', 'in'),
                              to = c('mm', 'cm', 'in')) {

    if(!is.numeric(x)) stop('A numeric vector is required')

    from <- match.arg(from[1], c('mm', 'cm', 'in'))
    to <- match.arg(to[1], c('mm', 'cm', 'in'))

    if(from == 'mm') {

      switch(to,
             mm = x,
             cm = 0.1 * x,
             `in` = 0.0393700787 * x)

    } else if(from == 'cm') {

      switch(to,
             mm = 10 * x,
             cm = x,
             `in` = 0.393700787 * x)

    } else {

      switch(to,
             mm = x/0.0393700787,
             cm = x/0.393700787,
             `in` = x)

    }

  }

#' Convert plot units.
#'
#' @param x A figure object
#' @param to the requested unit
#' @return A figure object with the dimensions converted to the requested unit
#' @export

  convert.figure <- function(x, to = c('mm', 'cm', 'in')) {

    stopifnot(is_figure(x))

    old_unit <- extract(x, 'unit')
    old_h <- height(x)
    old_w <- width(x)

    height(x, lock = FALSE) <- convert(old_h,
                                       from = old_unit,
                                       to = to)

    width(x, lock = FALSE) <- convert(old_w,
                                      from = old_unit,
                                      to = to)

    x$unit <- to

    x

  }

# Markdown references: chunks --------

#' Insert a figure object chunk into an R markdown file
#'
#' @description Inserts a reference to the figure object as an R code chunk into
#' a R markdown file or prints it into the standard output and, concomitantly,
#' copies into the clipboard.
#' @param object a figure object to be referenced (\code{\link{figure}})
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param html logical: should the legend be HTML styled?
#' @param style_ref name of the CSS style of the legend text, valid only for
#' the HTML output.
#' @param append logical, should the output file be appended?
#' @param relative_dim logical, should the figure dimensions be inserted
#' as a call to an unit-converting function? If FALSE, fixed numeric dimensions
#' are inserted into the code chunk.
#' @param ... extra arguments, currently none.
#' @details Only for the figure class instances with
#' defined 'ref_name' and 'caption' parameters.
#' To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @return returns invisibly the requested R code chunk.
#' @export insert.figure
#' @export

  insert.figure <- function(object,
                            file = NULL,
                            html = FALSE,
                            style_ref = 'legend',
                            append = TRUE,
                            relative_dim = FALSE, ...) {

    ## entry control -----

    stopifnot(figur::is_figure(object))
    stopifnot(is.logical(html))
    stopifnot(is.logical(append))

    if(is.null(object$ref_name)) {

      warning('Valid only for figure object with a defined ref_name and caption parameters.',
              call. = FALSE)

      return(invisible(NULL))

    }

    if(is.null(object$caption)) {

      warning('Valid only for figure object with a defined ref_name and caption parameters.',
              call. = FALSE)

      return(invisible(NULL))

    }

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    object_ref <- substitute(object)

    object <- convert(object, to  = 'in')

    if(is.null(object$legend_text)) {

      legend_text <- '<<legend>>'

    } else {

      legend_text <- object$legend_text

    }

    ## handling dimensions -------

    if(!relative_dim) {

      object_w <- object$w
      object_h <- object$h

    } else {

      dim_call_str <-
        paste0("figur::convert(",
               rlang::as_label(object_ref),
               ", to = 'in')")

      object_w <- paste0(dim_call_str, '$w')
      object_h <- paste0(dim_call_str, '$h')

    }

    ## figure chunk construction --------

    if(!html) {

      chunk <- figur:::build_fig_back(figure_call = object_ref,
                                      ref_name = object$ref_name,
                                      caption = object$caption,
                                      figure_w = object_w,
                                      figure_h = object_h,
                                      legend = TRUE,
                                      legend_text = legend_text)

    } else {

      chunk <- figur:::build_fig_html(figure_call = object_ref,
                                      ref_name = object$ref_name,
                                      caption = object$caption,
                                      figure_w = object_w,
                                      figure_h = object_h,
                                      legend = TRUE,
                                      legend_text = legend_text,
                                      style_ref = style_ref)

    }

    ## ouput

    if(is.null(file)) {

      cat(chunk)

      try(clipr::write_clip(content = chunk,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

    } else {

      cat(chunk,
          file = file,
          append = append,
          fill = FALSE)

    }

    return(invisible(chunk))

  }

#' Insert a data frame chunk into an R markdown file
#'
#'@description Inserts a reference to a data frame as an R code chunk into
#' as R markdown file or prints it into the standard output.
#' The input data frame needs to be a mdtable object (\code{\link{mdtable}}).
#' @param object an mdtable object (\code{\link{mdtable}}).
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param format which format the table should be inserted in?
#' 'flextable' (default) or 'kable' available currently.
#' @param rownames logical, should the table rownames be included in the
#' markdown output? If TRUE, they are displayed in the first, unnamed column.
#' Relevant only for format = 'flextable'.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export insert.mdtable
#' @export

  insert.mdtable <- function(object,
                             file = NULL,
                             format = c('flextable', 'kable'),
                             rownames = FALSE,
                             append = TRUE, ...) {

    ## entry control

    stopifnot(figur::is_mdtable(object))
    stopifnot(is.logical(append))

    format <- match.arg(format[1], c('flextable', 'kable'))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    object_call <- rlang::as_label(substitute(object))

    object_ref <- attr(object, 'ref_name')

    object_cap <- attr(object, 'caption')

    ins_fun <- switch(format,
                      flextable = 'flextable::flextable',
                      kable = 'knitr::kable')

    ## table chunk construction:

    ### heading

    heading <- paste0("r tab-",
                      object_ref,
                      ", tab.cap = '",
                      object_cap, "'")

    heading <- paste0('{', heading, '}')

    ### body

    if(format == 'flextable' & rownames) {

      md_call <- paste0(ins_fun,
                        '(tibble::rownames_to_column(',
                        object_call, ', " "))')

    } else {

      md_call <- paste0(ins_fun, '(', object_call, ')')

    }

    chunk <- paste0('```', heading, '\n\n', md_call, '\n\n', '```')

    ## output

    if(is.null(file)) {

      cat(chunk)

      try(clipr::write_clip(content = chunk,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

      return(invisible(chunk))

    } else {

      cat(chunk,
          file = file,
          append = append,
          fill = FALSE)

      return(invisible(chunk))

    }

  }

#' Insert a code chunk into an R markdown file
#'
#' @description Inserts a code chunk based on an mdexpr object
#' (\code{\link{mdexpr}}) into an R markdown file or prints it into the
#' standard output.
#' @param object an mdexpr object (\code{\link{mdexpr}}).
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param format which format the table should be inserted in?
#' 'chunk' (default): a multiline code chunk or 'inline' code piece.
#' @param include the include option of the chunk, skipped if NULL.
#' @param echo echo chunk option, skipped if NULL.
#' @param warning warning chunk option, skipped if NULL.
#' @param message message chunk option, skipped if NULL.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}. If the ref_name parameter is set, the code
#' chunk will be named after it, otherwise the name is absent.
#' For include, echo, message and warning chunk options, see:
#' https://rmarkdown.rstudio.com/lesson-3.html
#' @export insert.mdexpr
#' @export

  insert.mdexpr <- function(object,
                            file = NULL,
                            format = c('chunk', 'inline'),
                            include = NULL,
                            echo = NULL,
                            warning = NULL,
                            message = NULL,
                            append = TRUE, ...) {

    ## entry control

    stopifnot(is_mdexpr(object))
    stopifnot(is.logical(append))

    format <- match.arg(format[1], c('chunk', 'inline'))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    ## chunk construction

    if(format == 'chunk') {

      ref_txt <- figur:::build_chunk(quosure = object$quosure,
                                     ref_name = object$ref_name,
                                     include = include,
                                     echo = echo,
                                     warning = warning,
                                     message = message)

    } else {

     ref_txt <- figur:::build_inline(quosure = object$quosure)

    }

    ## output

    if(is.null(file)) {

      cat(ref_txt)

      try(clipr::write_clip(content = ref_txt,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

      return(invisible(ref_txt))

    } else {

      cat(ref_txt,
          file = file,
          append = append,
          fill = FALSE)

      return(invisible(ref_txt))

    }

  }

# Inserting and referencing links and HTML elements --------

#' Insert/refer a link.
#'
#' @description Inserts a link based on an mdlink object
#' (\code{\link{mdlink}}) into an R markdown file or prints it into the
#' standard output.
#' @param object an mdlink object (\code{\link{mdlink}}).
#' @param title a string to be used as a link title, defaults to the `ref_name`
#' of the mdlink object.
#' @param html logical, should the link be in a HTML format? Defaults to FALSE.
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export insert.mdlink
#' @export

  insert.mdlink <- function(object,
                            title = object$ref_name,
                            html = FALSE,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdlink(object))
    stopifnot(is.logical(append))
    stopifnot(is.character(title))
    stopifnot(is.logical(html))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.',
                                     call. = FALSE)

    }

    ## link text -------

    if(!html) {

      lk_string <-
        paste0('[', title, '](', object$URL, ')')

    } else {

      lk_string <-
        paste0('<a href = "', object$URL, '">',
               title, '</a>')

    }

    ## output -------

    if(is.null(file)) {

      cat(lk_string)

      try(clipr::write_clip(content = lk_string,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

      return(invisible(lk_string))

    } else {

      cat(lk_string,
          file = file,
          append = append,
          fill = FALSE)

      return(invisible(lk_string))

    }

  }

#' @rdname insert.mdlink
#' @export refer.mdlink
#' @export

  refer.mdlink <- function(object,
                           title = object$ref_name,
                           html = FALSE,
                           file = NULL,
                           append = TRUE, ...) {

    insert.mdlink(object = object,
                  title = title,
                  html = html,
                  file = file,
                  append = append, ...)

  }

#' Insert/refer an HTML element.
#'
#' @description Inserts a HTML element based on an mdhtml object
#' (\code{\link{mdhtml}}) into an R markdown file or prints it into the
#' standard output.
#' @param object an mdhtml object (\code{\link{mdhtml}}).
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export insert.mdhtml
#' @export

  insert.mdhtml <- function(object,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdhtml(object))
    stopifnot(is.logical(append))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.',
                                     call. = FALSE)

    }

    ## output -------

    if(is.null(file)) {

      cat(object)

      try(clipr::write_clip(content = object,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

      return(invisible(object))

    } else {

      cat(object,
          file = file,
          append = append,
          fill = FALSE)

      return(invisible(object))

    }

  }

#' @rdname insert.mdhtml
#' @export refer.mdhtml
#' @export

  refer.mdhtml <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    insert.mdhtml(object = object,
                  file = file,
                  append = append, ...)

  }

# Inserting and referencing text -------

#' Insert/refer a text element.
#'
#' @description Inserts a text element based on an mdtext object
#' (\code{\link{mdtext}}) into an R markdown file or prints it into the
#' standard output.
#' @param object an mdtext object (\code{\link{mdtext}}).
#' @param file a file to which the chunk will be written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export insert.mdtext
#' @export

  insert.mdtext <- function(object,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdtext(object))
    stopifnot(is.logical(append))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.',
                                     call. = FALSE)

    }

    ## output -------

    if(is.null(file)) {

      cat(object)

      try(clipr::write_clip(content = object,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

      return(invisible(object))

    } else {

      cat(object,
          file = file,
          append = append,
          fill = FALSE)

      return(invisible(object))

    }

  }

#' @rdname insert.mdtext
#' @export refer.mdtext
#' @export

  refer.mdtext <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    insert.mdtext(object = object,
                  file = file,
                  append = append, ...)

  }

# Searching and referencing bibliography -------

#' Search bibliography by a regular expression.
#'
#' @description Searches the content of a bibliography object of the 'mdbib'
#' class. In combination with the \code{\link{refer.mdbib}} method,
#' it can be used to search a bibliography and paste the selected citations
#' into an Rmarkdown document.
#' @details Technically, the search is accomplished by the
#' \code{\link[stringi]{stri_detect}} function.
#' @param object an mdbib object (\code{\link{mdbib}}).
#' @param regex a regular expression.
#' @param keys columns of the mdbib object to be searched for the regex.
#' Defaults to NULL, which means that all columns will be looked up.
#' @param multiple ignored when a single search key is provided.
#' It describes how searching results from multiple mdbib columns will
#' be handled. 'OR' (default) specifies that the multiply column search results
#' will be merged by logical sum, for 'AND' they will be merged by logical
#' intersection.
#' @param ... extra arguments, currently none.
#' @return a mdbib object.
#' @export reglook.mdbib
#' @export

  reglook.mdbib <- function(object,
                            regex,
                            keys = NULL,
                            multiple = c('OR', 'AND')) {

    stopifnot(is_mdbib(object))

    NextMethod()

  }

#' Reference to bibliography.
#'
#' @description Inserts a citation to a all entries of a bibliography object
#' of the 'mdbib' class. In combination with the \code{\link{reglook.mdbib}}
#' or the tidyverse's `filter()` method, it can be used to search
#' a bibliography and paste the selected citations into an Rmarkdown document.
#' @details Creates Rmarkdown style citations: `[@citation1; @citation2; ...]`.
#' @param object an mdbib object (\code{\link{mdbib}}).
#' @param file a file to which the chunk will be written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export refer.mdbib
#' @export

  refer.mdbib <- function(object,
                          file = NULL,
                          append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdbib(object))
    stopifnot(is.logical(append))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.',
                                     call. = FALSE)

    }

    if(nrow(object) == 0) {

      warning("The bibliography is empty!", call. = FALSE)

      return(NULL)

    }

    ## citation text -------

    if(nrow(object) == 1) {

      cit_txt <- paste0('[@', object$BIBTEXKEY[1], ']')

    } else {

      cit_txt <- paste0('@', object$BIBTEXKEY)

      cit_txt <- paste(cit_txt, collapse = '; ')

      cit_txt <- paste0('[', cit_txt, ']')

    }

    ## output -------

    if(is.null(file)) {

      cat(cit_txt)

      try(clipr::write_clip(content = cit_txt,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

      return(invisible(cit_txt))

    } else {

      cat(cit_txt,
          file = file,
          append = append,
          fill = FALSE)

      return(invisible(cit_txt))

    }

  }

# Markdown inline citations -------

#' Cite a figure object.
#'
#' @description Generates an inline citation to a figure object, currently
#' only in the bookdown style.
#' @param object a figure object.
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console.
#' @param append logical, should the output file be appended?
#' @param ... extra arguments, currently none.
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export refer.figure
#' @export

  refer.figure <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    ## entry control

    stopifnot(figur::is_figure(object))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    if(is.null(object$ref_name)) {

      warning('Valid only for figure object with a defined ref_name and caption parameters.',
              call. = FALSE)

      return(invisible(NULL))

    }

    ## citation

    ref <- figur:::build_bookdown_ref(ref_name = object$ref_name,
                                      ref_type = 'figure')


    if(is.null(file)) {

      cat(ref)

      try(clipr::write_clip(content = ref,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

    } else {

      cat(ref,
          file = file,
          append = append,
          fill = FALSE)

    }

    return(invisible(ref))

  }

#' Cite an mdtable object.
#'
#' @description Generates an inline citation to an mdtable object, currently
#' only in the bookdown style.
#' @param object an mdtable object.
#' @inheritParams refer.figure
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export refer.mdtable
#' @export

  refer.mdtable <- function(object,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control

    stopifnot(figur::is_mdtable(object))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    ## citation

    ref <- figur:::build_bookdown_ref(ref_name = attr(object, 'ref_name'),
                                      ref_type = 'table')


    if(is.null(file)) {

      cat(ref)

      try(clipr::write_clip(content = ref,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

    } else {

      cat(ref,
          file = file,
          append = append,
          fill = FALSE)

    }

    return(invisible(ref))

  }

#' Insert inline code.
#'
#' @description Generates an inline code text for the provided mdexpr object.
#' @param object an mdexpr object.
#' @inheritParams refer.figure
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' @export refer.mdexpr
#' @export

  refer.mdexpr <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    ## entry control

    stopifnot(figur::is_mdexpr(object))

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    ## citation

    ref <- figur:::build_inline(quosure = object$quosure)


    if(is.null(file)) {

      cat(ref)

      try(clipr::write_clip(content = ref,
                            object_type = 'character',
                            breaks = '\n'),
          silent = TRUE)

    } else {

      cat(ref,
          file = file,
          append = append,
          fill = FALSE)

    }

    return(invisible(ref))


  }

# Saving on the disc -------

#' Save a figure object on the disc.
#'
#' @description Saves a figure object on the disc.
#' @param object a figure object.
#' @param path The target path. The destination folder will not be created
#' @param format the desired file format.
#' @param ... extra arguments passed to \code{\link[ggplot2]{ggsave}}.
#' @return None, called for side effects
#' @export pickle.figure
#' @export

  pickle.figure <- function(object,
                            path = '.',
                            format = 'pdf', ...) {

    stopifnot(figur::is_figure(object))

    figur::save_figure(figure_object = object,
                       path = path,
                       format = format, ...)

  }

#' Save an mdtable object on the disc.
#'
#' @description Saves an mdtable object on as a text file on the disc.
#' @param object an mdtable object.
#' @param folder The target folder. The destination folder will not be created.
#' @param format the desired file format.
#' @param delim delimiter for clumns separation, tabulation by default.
#' @param ... extra arguments passed to \code{\link[readr]{write_delim}}.
#' @return None, called for side effects
#' @export

  pickle.mdtable <- function(object,
                             folder = '.',
                             format = 'tsv',
                             delim = '\t', ...) {

    stopifnot(figur::is_mdtable(object))

    figur::save_mdtable(mdtable_object = object,
                        folder = folder,
                        format = format,
                        delim = delim, ...)

  }

# END -----
