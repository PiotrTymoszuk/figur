# Tools for inserting figure objects into markdown files (not exported)

# markdown helpers ----

#' Create a R markdown backbone for a figure call.
#'
#' @description Builds a character with the R code chunk for the given figure.
#' @param figure_call call to a figure object.
#' @param ref_name name of the figure chunk in the R markdown output.
#' @param caption figure caption.
#' @param figure_w figure width in inches or a call to a function
#' returning a numeric provided as a character.
#' @param figure_h figure height in inches or a call to a function
#' returning a numeric provided as a character.
#' @param legend logical, should a text with the figure reference in bold be included below
#' the figure chunk?
#' @param legend_text detailed legend text.
#' @return a string with the figure object R markdown chunk.

  build_fig_back <- function(figure_call,
                             ref_name = 'figure',
                             caption = 'figure',
                             figure_w = 5,
                             figure_h = 7,
                             legend = TRUE,
                             legend_text = '<<legend>>') {

    stopifnot(is.logical(legend))

    fig_label <- rlang::as_label(figure_call)

    ## heading

    heading <- paste0("r fig-",
                      ref_name,
                      ", fig.width = ",
                      figure_w,
                      ", fig.height = ",
                      figure_h,
                      ", fig.cap = '",
                      caption, "'")

    heading <- paste0('{', heading, '}')

    ## body

    body <- paste0('```', heading, '\n\n', fig_label, '$plot\n\n', '```')

    if(!legend) return(body)

    ## legend

    fig_leg <- paste0("__Figure \\@ref(fig:fig-",
                      ref_name, "). ",
                      caption,
                      "__ \n_",
                      legend_text,
                      "_")

    paste0(body, '\n\n', fig_leg, '\n\n')

  }

# markdown HTML helpers -------

#' Create an R markdown/HTML backbone for a figure call.
#'
#' @description Builds a character with the R code chunk for the given figure
#' and HTML/CSS styling of the legend.
#' @inheritParams build_fig_back
#' @param style_ref name of the CSS style of the legend text.

  build_fig_html <- function(figure_call,
                             ref_name = 'figure',
                             caption = 'figure',
                             figure_w = 5,
                             figure_h = 7,
                             legend = TRUE,
                             legend_text = '<<legend>>',
                             style_ref = 'legend') {

    stopifnot(is.logical(legend))

    ## heading

    body <- build_fig_back(figure_call = figure_call,
                           ref_name = ref_name,
                           caption = caption,
                           figure_w = figure_w,
                           figure_h = figure_h,
                           legend = FALSE)

    if(!legend) return(body)

    ## legend

    style_tag <- paste0('<p class = "',
                        style_ref,
                        '">')

    fig_leg <- paste0(style_tag,
                      '\n<b>',
                      'Figure \\@ref(fig:fig-',
                      ref_name,
                      '). ',
                      caption,
                      '</b>',
                      '\n<br>\n',
                      legend_text,
                      '\n</p>')

    paste0(body, '\n\n', fig_leg, '\n\n')

  }

# markdown citation helpers -------

#' Create an R markdown figure/table citation text.
#'
#' @description Generates a customized inline reference to a figure or table
#' with the bookdown standard.
#' @param ref_name reference name.
#' @param ref_type reference type: table or figure.
#' @return a citation text.

  build_bookdown_ref <- function(ref_name,
                                 ref_type = c('table', 'figure')) {

    ref_name <- as.character(ref_name)

    ref_type <- match.arg(ref_type[1], c('table', 'figure'))

    type_txt <- switch(ref_type,
                       table = 'tab:tab-',
                       figure = 'fig:fig-')

    type_prefix <- switch(ref_type,
                          table = 'Table ',
                          figure = 'Figure ')

    paste0(type_prefix, '\\@ref(', type_txt, ref_name, ')')

  }

# markdown chunk and inline code construction helpers ------

#' Create an R markdown code chunk.
#'
#' @description Builds a standard code chunk with the user-specified options
#' based on the quosure object provided.
#' @param quosure a quosure object.
#' @param ref_name reference name, skipped if NULL.
#' @param include include chunk option, skipped if NULL.
#' @param echo echo chunk option, skipped if NULL.
#' @param warning warning chunk option, skipped if NULL.
#' @param message message chunk option, skipped if NULL.
#' @return a chunk text.

  build_chunk <- function(quosure,
                          ref_name = NULL,
                          include = NULL,
                          echo = NULL,
                          warning = NULL,
                          message = NULL) {

    ## entry control

    stopifnot(rlang::is_quosure(quosure))

    quo_text <- as.expression(rlang::quo_get_expr(quosure))

    quo_text <- as.character(quo_text)

    if(is.null(include)) {

      include_txt <- NULL

    } else {

      include_txt <- if(include) 'include = TRUE' else 'include = FALSE'

    }

    if(is.null(echo)) {

      echo_txt <- NULL

    } else {

      echo_txt <- if(echo) 'echo = TRUE' else 'echo = FALSE'

    }

    if(is.null(warning)) {

      warning_txt <- NULL

    } else {

      warning_txt <- if(warning) 'warning = TRUE' else 'warning = FALSE'

    }

    if(is.null(message)) {

      message_txt <- NULL

    } else {

      message_txt <- if(message) 'message = TRUE' else 'message = FALSE'

    }

    ## stitching the chunk together

    options <- paste(c(include_txt,
                       echo_txt,
                       warning_txt,
                       message_txt),
                     collapse = ', ')

    if(options == '' | is.null(options)) {

      head <- ref_name

    } else {

      head <- paste(ref_name, options, sep = ', ')

    }

    if(is.null(ref_name)) {

      if(options == '' | is.null(options)) {

        head <- NULL

      } else {

        head <- options

      }

    }

    paste0('```',
           '{r ', head, '}',
           '\n\n', quo_text, '\n\n',
           '```')

  }

#' Create an inline R code text.
#'
#' @description Creates a standard inline R code based on a quosure.
#' @param quosure a quosure object.
#' @return inline code text.

  build_inline <- function(quosure) {

    stopifnot(rlang::is_quosure(quosure))

    quo_text <- as.expression(rlang::quo_get_expr(quosure))

    quo_text <- as.character(quo_text)

    paste0('`r ', quo_text, '`')

  }

# figure inserting function -----

#' Insert a reference to figure objects into a Rmarkdown file.
#'
#' @description Builds a character with the R code chunks for the given figure objects.
#' @param ... figure objects.
#' @param file a file to which the chunks are written. If the file exists already, it will
#' be appended or overwritten. If NULL, the text is printed in the console.
#' @param ref_names names of the figure chunk in the R markdown output.
#' If NULL, default names ('figure1', 'figure2' and so on) are used.
#' @param captions figure captions. If NULL, default captions ('caption for figure 1' and so on) are used.
#' @param legend logical, should a text with the figure reference in bold be included below
#' the figure chunk?
#' @param legend_text a character vector or list with detailed legend texts.
#' @param html logical: should the legend be HTML styled?
#' @param style_ref name of the CSS style of the legend text, valid only for
#' the HTML output.
#' @param append logical, should the output file be appended?

  insert_figure <- function(...,
                            file = NULL,
                            ref_names = NULL,
                            captions = NULL,
                            legend = TRUE,
                            legend_text = '<<legend>>',
                            html = FALSE,
                            style_ref = 'legend',
                            append = FALSE) {

    ## entry control

    stopifnot(is.logical(legend))
    stopifnot(is.logical(append))
    stopifnot(is.logical(html))

    inp_obj <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_obj, is_figure)

    if(!all(classes)) stop("An object of class 'figure' is required.", call. = FALSE)

    if(!is.null(captions)) {

      if(length(captions) != length(inp_obj)) {

        stop('The ref_name and captions argument lengths must be equal to the number of figure objects', call. = FALSE)

      }

    }

    if(!is.null(ref_names)) {

      if(length(ref_names) != length(inp_obj)) {

        stop('The ref_name and captions argument lengths must be equal to the number of figure objects', call. = FALSE)

      }

    }

    if(length(legend_text) > 1) {

      if(length(legend_text) != length(inp_obj)) {

        stop('The legned_text argument lengths must be 1 or equal to the number of figure objects', call. = FALSE)

      }

    }

    inp_obj <- purrr::map(inp_obj,
                          function(x) if(x$unit != 'in') convert(x, to = 'in') else x)

    if(is.null(ref_names)) {

      ref_names <- paste0('figure', 1:length(inp_obj))

    }

    if(is.null(captions)) {

      captions <- paste0('caption for figure ', 1:length(inp_obj))

    }

    if(!is.null(file)) {

      if(!file.exists(file)) warning('The target_path does not exist, a new will be created.', call. = FALSE)

    }

    ## building the backbone

    fig_calls <- rlang::enexprs(...)

    fig_w <- purrr::map(inp_obj, width)
    fig_h <- purrr::map(inp_obj, height)

    if(!html) {

      backbones <- purrr::pmap(list(figure_call = fig_calls,
                                    ref_name = ref_names,
                                    caption = captions,
                                    figure_w = fig_w,
                                    figure_h = fig_h,
                                    legend_text = legend_text),
                               build_fig_back,
                               legend = legend)

    } else {

      backbones <- purrr::pmap(list(figure_call = fig_calls,
                                    ref_name = ref_names,
                                    caption = captions,
                                    figure_w = fig_w,
                                    figure_h = fig_h,
                                    legend_text = legend_text),
                               build_fig_html,
                               legend = legend,
                               style_ref = style_ref)

    }

    if(is.null(file)) {

      purrr::walk(backbones, cat)

      invisible(backbones)

    } else {

      backbones <- purrr::reduce(backbones, c)

      backbones <- paste(backbones, collapse = '\n\n')

      cat(backbones,
          file = file,
          append = append,
          fill = FALSE)

      invisible(backbones)

    }

  }

# END ------
