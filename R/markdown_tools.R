# Tools for inserting figure objects into markdown files.

# helpers ----

#' Create a R markdown backbone for a figure call.
#'
#' @description Builds a character with the R code chunk for the given figure.
#' @param figure_call call to a figure object.
#' @param ref_name name of the figure chunk in the R markdown output.
#' @param caption figure caption.
#' @param figure_w figure width in inches.
#' @param figure_h figure height in inches.
#' @param legend logical, should a text with the figure reference in bold be included below
#' the figure chunk?
#' @return a string with the figure object R markdown chunk.

  build_fig_back <- function(figure_call,
                             ref_name = 'figure',
                             caption = 'figure',
                             figure_w = 5,
                             figure_h = 7,
                             legend = TRUE) {

    #stopifnot(rlang::is_call(figure_call))
    stopifnot(is.logical(legend))
    stopifnot(is.numeric(figure_w))
    stopifnot(is.numeric(figure_h))

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
                      "__")

    paste0(body, '\n\n', fig_leg, '\n\n')

  }

# inserting function -----

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
#' @param append logical, should the output file be appended?
#' @export

  insert_figure <- function(...,
                            file = NULL,
                            ref_names = NULL,
                            captions = NULL,
                            add_extern_legend = TRUE,
                            append = FALSE) {

    ## entry control

    stopifnot(is.logical(add_extern_legend))
    stopifnot(is.logical(append))

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


    inp_obj <- purrr::map(inp_obj,
                          function(x) if(x$unit != 'in') figur::convert(x, to = 'in') else x)

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

    fig_w <- purrr::map(inp_obj, figur::width)
    fig_h <- purrr::map(inp_obj, figur::height)

    backbones <- purrr::pmap(list(figure_call = fig_calls,
                                  ref_name = ref_names,
                                  caption = captions,
                                  figure_w = fig_w,
                                  figure_h = fig_h),
                             figur:::build_fig_back,
                             legend = add_extern_legend)

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


