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
                            append = TRUE, ...) {

    ## entry control

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

    ## figure chunk construction

    if(!html) {

      chunk <- figur:::build_fig_back(figure_call = object_ref,
                                      ref_name = object$ref_name,
                                      caption = object$caption,
                                      figure_w = object$w,
                                      figure_h = object$h,
                                      legend = TRUE,
                                      legend_text = legend_text)

    } else {

      chunk <- figur:::build_fig_html(figure_call = object_ref,
                                      ref_name = object$ref_name,
                                      caption = object$caption,
                                      figure_w = object$w,
                                      figure_h = object$h,
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
