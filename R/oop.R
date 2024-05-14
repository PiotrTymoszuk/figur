# S3 methods for the figure, mdtable and mdexpr class

# collate ------

#' @include imports.R
#' @include generics.R
#' @include classes.R

  NULL

# Object appearance --------

#' Print a figure, mdexpr or mdlink object
#'
#' @param x an object
#' @param ... extra arguments, currently none.
#' @return The figure's label and dimensions
#' @export

  print.figure <- function(x, ...) {

    stopifnot(is_figure(x))

    cat(paste0(components(x, 'label'),
               ', Width: ', components(x, 'w'), ' ', components(x, 'unit'),
               ', Height: ', components(x, 'h'), ' ', components(x, 'unit')))

  }

#' @rdname print.figure
#' @export

  print.mdexpr <- function(x, ...) {

    expr <- as.expression(quo_get_expr(x$quosure))

    cat(paste0('mdexpr: {', as.character(expr), '} = ', x$result))

  }

#' @rdname print.figure
#' @export

  print.mdlink <- function(x, ...) {

    stopifnot(is_mdlink(x))

    cat(paste(x[['ref_name']],
              x[['URL']],
              sep = ': '))

  }

# Accessing object components ----------

#' Extract components of a figure, mdtable, mdexpr or mdlink object.
#'
#' @description Accesses specific components of an object specified by the
#' `what` argument or plots it.
#' @param object an object.
#' @param x an object.
#' @param what The requested feature.
#' 'plot': the ggplot,
#' 'label': the rendering/saving label,
#' 'h': rendering height,
#' 'w': the rendering width,
#' 'unit': the rendering unit,
#' 'ref_name': reference name used in R markdown,
#' 'caption' object reference caption used in R markdown,
#' 'legend_text' extended legend text used in R markdown.
#' 'quosure': the quosure,
#' 'expression': R expression,
#' 'result': the evaluation result,
#' 'URL': the link's URL.
#' @param ... extra arguments, currently none.
#' @return The requested feature
#' @importFrom generics components
#' @export

  components.figure <- function(object,
                                what = c('plot', 'label',
                                         'w', 'h', 'unit',
                                         'ref_name', 'caption',
                                         'legend_text'), ...) {

    stopifnot(is_figure(object))

    what <- match.arg(what[1],
                      c('plot', 'label',
                        'w', 'h', 'unit',
                        'ref_name', 'caption', 'legend_text'))

    object[[what]]

  }

#' @rdname components.figure
#' @export

  plot.figure <- function(x, ...) {

    stopifnot(is_figure(x))

    x$plot

  }

#' @rdname components.figure
#' @export

  components.mdtable <- function(object,
                                 what = c('label',
                                          'ref_name',
                                          'caption'), ...) {

    stopifnot(is_mdtable(object))

    what <- match.arg(what[1],
                      c('label', 'ref_name', 'caption'))

    attr(object, what)

  }

#' @rdname components.figure
#' @export

  components.mdexpr <- function(object,
                                what = c('quosure',
                                         'expression',
                                         'result',
                                         'ref_name',
                                         'caption'), ...) {

    stopifnot(is_mdexpr(object))

    what <- match.arg(what,
                      c('quosure', 'expression', 'result',
                        'ref_name', 'caption'))

    if(what == 'expression') {

      return(as.expression(quo_get_expr(object$quosure)))

    }

    object[[what]]

  }

#' @rdname components.figure
#' @export

  components.mdlink <- function(object,
                                what = c('ref_name', 'URL'), ...) {

    stopifnot(is_mdlink(object))

    object[[what]]

  }

# figure S3 methods: width and height -----

#' Get or set set figure object dimensions.
#'
#' @description
#' Get or set dimensions of a `figure` class object.
#'
#' @details
#' `height()`, `width()` and `resize()` are S3 generic functions.
#'
#' @param x a figure class object.
#' @param value new height or width or a scaling factor.
#' @param what The requested dimension to be changed or the scaling factor.
#' @param lock logical, if TRUE, the aspect ratio is kept constant (scaling).
#' @param unit The dimension unit of the final plot object.
#' @param ... extra arguments passed to methods.
#'
#' @return A `figure` object with the height or width property modified
#' or the requested dimensions.
#'
#' @export height.figure
#' @export

  height.figure <- function(x, ...) {

    stopifnot(is_figure(x))

    components(x, 'h')

  }

#' @rdname height.figure
#' @export width.figure
#' @export

  width.figure <- function(x, ...) {

    stopifnot(is_figure(x), ...)

    components(x, 'w')

  }

#' @rdname height.figure
#' @export

  `height<-` <- function(x, value) {

    x$h <- value

    x

  }

#' @rdname height.figure
#' @export

  `width<-` <- function(x, value) {

    x$w <- value

    x

  }

#' @rdname height.figure
#' @export resize.figure
#' @export

  resize.figure <- function(x,
                            value,
                            what = c('w', 'h', 'scale'),
                            unit = x$unit,
                            lock = TRUE, ...) {

    if(!is_figure(x)) {

      stop("An object of class 'figure' is required", call. = FALSE)

    }

    if(!is.numeric(value)) {

      stop('The value argument needs to be numeric', call. = FALSE)

    }

    what <- match.arg(what[1], c('w', 'h', 'scale'))
    unit <- match.arg(unit[1], c('mm', 'cm', 'in'))

    if(unit != x$unit) {

      x <- convert(x, to = unit)

    }

    if(what == 'scale') {

      height(x) <- height(x) * value
      width(x) <- width(x) * value

    } else if(what == 'h') {

      if(lock) {

        old_w <- width(x)
        old_h <- height(x)

        width(x) <- old_w * value/old_h

      }

      height(x) <- value

    } else {


      if(lock) {

        old_w <- width(x)
        old_h <- height(x)

        height(x) <- old_h * value/old_w

      }

      width(x) <- value

    }

    x

  }

# figure S3 methods: unit conversion -----

#' Convert units.
#'
#' @param x a numeric vector or a figure object.
#' @param from the input unit.
#' @param to the target unit.
#' @param ... extra arguments passed to methods.
#' @return a numeric vector after the unit conversion or
#' a `figure` class object with the unit information altered.
#' @examples
#' convert(10, 'mm', 'cm')
#' convert(180, 'mm', 'in')
#' convert(7.086614, 'in', 'mm')
#' @details `convert()` is a S3 generic function with methods defined
#' for numeric vectors and the `figure` class.
#' @export

  convert.numeric <- function(x,
                              from = c('mm', 'cm', 'in'),
                              to = c('mm', 'cm', 'in'), ...) {

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

#' @rdname convert.numeric
#' @export

  convert.figure <- function(x, to = c('mm', 'cm', 'in'), ...) {

    stopifnot(is_figure(x))

    old_unit <- components(x, 'unit')
    old_h <- height(x)
    old_w <- width(x)

    height(x) <- convert(old_h,
                         from = old_unit,
                         to = to)

    width(x) <- convert(old_w,
                        from = old_unit,
                        to = to)

    x$unit <- to

    x

  }

# Markdown references: chunks --------

#' Insert a figure, data frame, R code chunk, URL link
#' or text into an R markdown file
#'
#' @description
#' Inserts a reference to the figure, mdtable, mdexpr, mdhtml,
#' mdtext or mdlink object as an R code chunk into a R markdown file or prints
#' it into the standard output and, concomitantly, copies into the clipboard.
#'
#' @param object an object to be referenced.
#' @param file a file to which the chunks are written. If the file exists
#' already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param html logical: should the figure legend or link be HTML styled?
#' @param title a string to be used as a link title, defaults to the `ref_name`
#' of the mdlink object.
#' @param style_ref name of the CSS style of the legend text, valid only for
#' the HTML output.
#' @param format which format the table should be inserted in?
#' 'flextable' (default) or 'kable' available currently.
#' @param rownames logical, should the table rownames be included in the
#' markdown output? If TRUE, they are displayed in the first, unnamed column.
#' Relevant only for format = 'flextable'.
#' @param append logical, should the output file be appended?
#' @param relative_dim logical, should the figure dimensions be inserted
#' as a call to an unit-converting function? If FALSE, fixed numeric dimensions
#' are inserted into the code chunk.
#' @param include the include option of the chunk, skipped if NULL.
#' @param echo echo chunk option, skipped if NULL.
#' @param warning warning chunk option, skipped if NULL.
#' @param message message chunk option, skipped if NULL.
#' @param ... extra arguments, currently none.
#'
#' @details
#' Only for the \code{\link{figure}}, \code{\link{mdtable}} class instances with
#' defined 'ref_name' and 'caption' parameters.
#' To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' For include, echo, message and warning chunk options, see:
#' https://rmarkdown.rstudio.com/lesson-3.html
#'
#' @return returns invisibly the requested R code chunk.
#'
#' @export insert.figure
#' @export

  insert.figure <- function(object,
                            file = NULL,
                            html = FALSE,
                            style_ref = 'legend',
                            append = TRUE,
                            relative_dim = FALSE, ...) {

    ## entry control -----

    stopifnot(is_figure(object))
    stopifnot(is.logical(html))
    stopifnot(is.logical(append))

    if(is.null(object$ref_name)) {

      warning(paste('Valid only for figure object with a defined',
                    'ref_name and caption parameters.'),
              call. = FALSE)

      return(invisible(NULL))

    }

    if(is.null(object$caption)) {

      warning(paste('Valid only for figure object with a defined',
                    'ref_name and caption parameters.'),
              call. = FALSE)

      return(invisible(NULL))

    }

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

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
               as_label(object_ref),
               ", to = 'in')")

      object_w <- paste0(dim_call_str, '$w')
      object_h <- paste0(dim_call_str, '$h')

    }

    ## figure chunk construction --------

    if(!html) {

      chunk <- build_fig_back(figure_call = object_ref,
                              ref_name = object$ref_name,
                              caption = object$caption,
                              figure_w = object_w,
                              figure_h = object_h,
                              legend = TRUE,
                              legend_text = legend_text)

    } else {

      chunk <- build_fig_html(figure_call = object_ref,
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

      try(write_clip(content = chunk,
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

#' @rdname insert.figure
#' @export insert.mdtable
#' @export

  insert.mdtable <- function(object,
                             file = NULL,
                             format = c('flextable', 'kable'),
                             rownames = FALSE,
                             append = TRUE, ...) {

    ## entry control

    stopifnot(is_mdtable(object))
    stopifnot(is.logical(append))

    format <- match.arg(format[1], c('flextable', 'kable'))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

    }

    object_call <- as_label(substitute(object))

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

      try(write_clip(content = chunk,
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

#' @rdname insert.figure
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

      ref_txt <- build_chunk(quosure = object$quosure,
                             ref_name = object$ref_name,
                             include = include,
                             echo = echo,
                             warning = warning,
                             message = message)

    } else {

     ref_txt <- build_inline(quosure = object$quosure)

    }

    ## output

    if(is.null(file)) {

      cat(ref_txt)

      try(write_clip(content = ref_txt,
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

#' @rdname insert.figure
#' @export insert.mdtext
#' @export

  insert.mdtext <- function(object,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdtext(object))
    stopifnot(is.logical(append))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

    }

    ## output -------

    if(is.null(file)) {

      cat(object)

      try(write_clip(content = object,
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


#' @rdname insert.figure
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

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

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

      try(write_clip(content = lk_string,
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

#' @rdname insert.figure
#' @export insert.mdhtml
#' @export

  insert.mdhtml <- function(object,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdhtml(object))
    stopifnot(is.logical(append))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

    }

    ## output -------

    if(is.null(file)) {

      cat(object)

      try(write_clip(content = object,
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

# Markdown inline citations -------

#' Cite a figure, table, literature, text, URL link
#' or generate inline R/HTML code
#'
#' @description
#' Generates an inline citation to a figure, mdtable object or
#' literature references, currently only in the bookdown style.
#' For mdbib objects, it inserts a citation to a all entries of
#' a bibliography entries (e.g. `[@citation1; @citation2; ...]`).
#' In combination with the \code{\link{reglook.mdbib}}
#' or the tidyverse's `filter()` method, it can be used to search
#' a bibliography object and paste the selected citations
#' into an Rmarkdown document.
#' For mdexpr, mdhtml and mdtext objects storing R, HTML code
#' and ordinary text, respectively, an inline text is generated.
#' For mdlinks a link in the markdown or html format is generated.
#'
#' @param object an object to be referenced.
#' @param file a file to which the references or code are written.
#' If the file exists already, it will be appended or overwritten.
#' If NULL, the text is printed in the console and copied into the clipboard.
#' @param append logical, should the output file be appended?
#' @param title a string to be used as a link title, defaults to the `ref_name`
#' of the mdlink object.
#' @param html logical, should the link be in a HTML format? Defaults to FALSE.
#' @param ... extra arguments, currently none.
#'
#' @details To enable the clipboard access, you may need to set the CLIPR_ALLOW
#' environment variable to TRUE, as described for
#' \code{\link[clipr]{write_clip}}.
#' `refer()` is a S3 generic function.
#'
#' @export refer.figure
#' @export

  refer.figure <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    ## entry control

    stopifnot(is_figure(object))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

    }

    if(is.null(object$ref_name)) {

      warning(paste('Valid only for figure object with a defined ref_name',
                    'and caption parameters.'),
              call. = FALSE)

      return(invisible(NULL))

    }

    ## citation

    ref <- build_bookdown_ref(ref_name = object$ref_name,
                              ref_type = 'figure')


    if(is.null(file)) {

      cat(ref)

      try(write_clip(content = ref,
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

#' @rdname refer.figure
#' @export refer.mdtable
#' @export

  refer.mdtable <- function(object,
                            file = NULL,
                            append = TRUE, ...) {

    ## entry control

    stopifnot(is_mdtable(object))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

    }

    ## citation

    ref <- build_bookdown_ref(ref_name = attr(object, 'ref_name'),
                              ref_type = 'table')


    if(is.null(file)) {

      cat(ref)

      try(write_clip(content = ref,
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

#' @rdname refer.figure
#' @export refer.mdexpr
#' @export

  refer.mdexpr <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    ## entry control

    stopifnot(is_mdexpr(object))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

    }

    ## citation

    ref <- build_inline(quosure = object$quosure)


    if(is.null(file)) {

      cat(ref)

      try(write_clip(content = ref,
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

#' @rdname refer.figure
#' @export refer.mdbib
#' @export

  refer.mdbib <- function(object,
                          file = NULL,
                          append = TRUE, ...) {

    ## entry control -----

    stopifnot(is_mdbib(object))
    stopifnot(is.logical(append))

    if(!is.null(file)) {

      if(!file.exists(file)) {

        warning('The target_path does not exist, a new will be created.',
                call. = FALSE)

      }

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

      try(write_clip(content = cit_txt,
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

#' @rdname refer.figure
#' @export refer.mdtext
#' @export

  refer.mdtext <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    insert.mdtext(object = object,
                  file = file,
                  append = append, ...)

  }

#' @rdname refer.figure
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

#' @rdname refer.figure
#' @export refer.mdhtml
#' @export

  refer.mdhtml <- function(object,
                           file = NULL,
                           append = TRUE, ...) {

    insert.mdhtml(object = object,
                  file = file,
                  append = append, ...)

  }

# Searching bibliography -------

#' @rdname reglook.default
#' @return a mdbib object.
#' @export

  reglook.mdbib <- function(object,
                            regex,
                            keys = NULL,
                            multiple = c('OR', 'AND'), ...) {

    stopifnot(is_mdbib(object))

    NextMethod()

  }

# Saving on the disc -------

#' Save a figure object or mdtable object on the disc.
#'
#' @description Saves a figure object on the disc.
#'
#' @details S3 generic function.
#'
#' @param object a figure object.
#' @param path The target path. The destination folder will not be created.
#' @param folder The target folder. The destination folder will not be created.
#' @param format the desired file format.
#' @param delim delimiter for column separation, tabulation by default.
#' @param ... extra arguments passed to \code{\link[ggplot2]{ggsave}} (figure)
#' or \code{\link[readr]{write_delim}} (table).
#'
#' @return None, called for side effects
#'
#' @export pickle.figure
#' @export

  pickle.figure <- function(object,
                            path = '.',
                            format = 'pdf', ...) {

    stopifnot(is_figure(object))

    save_figure(figure_object = object,
                path = path,
                format = format, ...)

  }

#' @rdname pickle.figure
#' @export pickle.mdtable
#' @export

  pickle.mdtable <- function(object,
                             folder = '.',
                             format = 'tsv',
                             delim = '\t', ...) {

    stopifnot(is_mdtable(object))

    save_mdtable(mdtable_object = object,
                 folder = folder,
                 format = format,
                 delim = delim, ...)

  }

# END -----
