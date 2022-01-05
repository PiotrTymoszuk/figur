# Provides functions for generation of the figure object and methods for printing,
# dimensions and saving/rendering

# S3 object constructor -----

#' Generate a figure object.
#' 
#' @param ggplot_object A ggplot/cowplot object
#' @param label The final file name
#' @param w The final width after saving/rendering
#' @param h The final height after saving/rendering
#' @param unit The unit of h and w
#' @return An object of class 'figure'
#' @export

  as_figure <- function(ggplot_object, label, w, h, unit = c('mm', 'cm', 'in')) {

    if(!ggplot2::is.ggplot(ggplot_object)) stop("An object of class 'ggplot' is required", call. = FALSE)
    if(!is.numeric(w)) stop('The w argument needs to be a number', call. = FALSE)
    if(!is.numeric(h)) stop('The h argument needs to be a number', call. = FALSE)
    
    unit  <- match.arg(unit[1], c('mm', 'cm', 'in'))

    figure_obj <- list(plot = ggplot_object, 
                       label = as.character(label), 
                       h = h, 
                       w = w, 
                       unit = unit)
    
    structure(figure_obj, 
              class = 'figure')
    
    
  }
  
# S3 class checker -----
  
#' Check for a figure object.
#' 
#' @param x An object to test
#' @return Logical, TRUE if the 'figure' class object
#' @export

  is_figure <- function(x) {
    
    any(class(x) == 'figure')
    
  }
  
# generics -----
  
#' Extract object features.
#' 
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The requested feature
#' @export
  
  extract <- function(x, ...) {
    
    UseMethod('extract', x)
    
  }
  
#' Convert plot units.
#' 
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object with the value converted to the requested unit  
#' @export

  convert <- function(x, ...) {
    
    UseMethod('convert', x)
    
  }
  
#' Get object height.
#' 
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object's height
#' @export

  height <- function(x, ...) {
    
    UseMethod('height', x)
    
  }
  
#' Get object width.
#' 
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object's width
#' @export
  
  width <- function(x, ...) {
    
    UseMethod('width', x)
    
  }
  
#' Set object height.
#' 
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object with the modified h property
#' @export
  
  `height<-` <- function(x, ...) {
    
    UseMethod('height<-', x)
    
  }
  
#' Set object width.
#' 
#' @param x An object of interest
#' @param ... Arguments passed to specific methods
#' @return The object with the modified w property
#' @export
  
  `width<-` <- function(x, ...) {
    
    UseMethod('width<-', x)
    
  }

# S3 methods for extracting and printing object features ------
  
#' Extract features of a figure object.
#' 
#' @param figure_object A figure object
#' @param what The requested feature. 
#'    'plot': the ggplot, 'label': the rendering/saving label, 'h': rendering height, 
#'    'w': the rendering width, 'unit': the rendering unit.
#' @return The requested feature
#' @export
  
  extract.figure <- function(figure_object, what = c('plot', 'label', 'w', 'h', 'unit')) {
    
    stopifnot(is_figure(figure_object))
    
    what <- match.arg(what[1], c('plot', 'label', 'w', 'h', 'unit'))
    
    figure_object[[what]]
    
  }
  
#' Print figure object properties.
#' 
#' @param figure_object A figure object
#' @return The figure's label and dimensions
#' @export  
  
  print.figure <- function(figure_object) {
    
    stopifnot(is_figure(figure_object))
    
    cat(paste0(extract(figure_object, 'label'), 
               ', Width: ', extract(figure_object, 'w'), ' ', extract(figure_object, 'unit'), 
               ', Height: ', extract(figure_object, 'h'), ' ', extract(figure_object, 'unit')))
    
  }
  
#' Plot a figure object.
#' 
#' @param figure_object A figure object
#' @return The figure's ggplot object
#' @export
  
  plot.figure <- function(figure_object) {
    
    stopifnot(is_figure(figure_object))
    
    extract(figure_object, 'plot')
    
  }  
  
#' Get figure object's height.
#' 
#' @param figure_object A figure object
#' @return The figure's height
#' @export

  height.figure <- function(figure_object) {
    
    stopifnot(is_figure(figure_object))
    
    extract(figure_object, 'h')
    
  }
  
#' Get figure object's width.
#' 
#' @param figure_object A figure object
#' @return The figure's width
#' @export
  
  width.figure <- function(figure_object) {
    
    stopifnot(is_figure(figure_object))
    
    extract(figure_object, 'w')
    
  }
  
# S3 methods: setting width and height -----
  
#' Set figure object height.
#' 
#' @param figure_object A figure object
#' @param value New height
#' @param lock If TRUE, the aspect ratio is kept constant (scaling)
#' @return A figure object with the height property modified
#' @export  
  
  `height<-` <- function(figure_object, value, lock = FALSE) {
    
    if(lock) {
      
      old_w <- width(figure_object)
      old_h <- height(figure_object)
      
      width(figure_object) <- old_w * value/old_h
      
    }
    
    figure_object$h <- value
    
    figure_object
    
  }
  
#' Set figure object width.
#' 
#' @param figure_object A figure object
#' @param value New height
#' @param lock If TRUE, the aspect ratio is kept constant (scaling)
#' @return A figure object with the height property modified
#' @export  
  
  `width<-` <- function(figure_object, value, lock = FALSE) {
    
    if(lock) {
      
      old_w <- width(figure_object)
      old_h <- height(figure_object)
      
      height(figure_object) <- old_h * value/old_w
      
    }
    
    figure_object$w <- value
    
    figure_object
    
  }
  
# S3 methods: unit conversion -----
  
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
  
  convert.numeric <- function(x, from = c('mm', 'cm', 'in'), to = c('mm', 'cm', 'in')) {
    
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
#' @param figure_object A figure object
#' @param to the requested unit
#' @return A figure object with the dimensions converted to the requested unit
#' @export

  convert.figure <- function(figure_object, to = c('mm', 'cm', 'in')) {
    
    stopifnot(is_figure(figure_object))
    
    old_unit <- extract(figure_object, 'unit')
    old_h <- height(figure_object)
    old_w <- width(figure_object)
    
    height(figure_object, lock = FALSE) <- convert(old_h, 
                                                   from = old_unit, 
                                                   to = to)
    
    width(figure_object, lock = FALSE) <- convert(old_w, 
                                                  from = old_unit, 
                                                  to = to)
    
    figure_object$unit <- to
    
    figure_object
    
  }
  
# functions ------
  
#' Resize a figure object.
#' 
#' @param figure_object A figure object
#' @param value The new dimension value
#' @param what The requested dimension to be changed or the scaling factor
#' @param unit The dimension unit of the final plot object
#' @param lock If TRUE, the aspect ratio is kept constant (scaling), ignored if 'what' is set to 'scale'
#' @return A figure object with the modified dimensions and (optionally) unit
#' @export

  resize <- function(figure_object, 
                     value, 
                     what = c('w', 'h', 'scale'), 
                     unit = extract(figure_object, 'unit'), 
                     lock = TRUE) {
    
    if(!is_figure(figure_object)) stop("An object of class 'figure' is required", call. = FALSE)
    if(!is.numeric(value)) stop('The value argument needs to be a number', call. = FALSE)
    
    what <- match.arg(what[1], c('w', 'h', 'scale'))
    unit <- match.arg(unit[1], c('mm', 'cm', 'in'))
    
    if(unit != figure_object$unit) {
      
      figure_object <- convert(figure_object, to = unit)
      
    }
    
    if(what == 'scale') {
      
      new_h <- height(figure_object) * value
      
      height(figure_object, lock = TRUE) <- new_h
      
    } else if(what == 'w') {
      
      height(figure_object, lock = lock) <- value
      
    } else {
      
      width(figure_object, lock = lock) <- value
      
    }

    figure_object
    
  }
  
#' Save a plot object on the disc
#' 
#' @param figure_object A figure object
#' @param path The target path. The destination folder will not be created
#' @return None, called for side effects
#' @export
  
  save_figure <- function(figure_object, 
                          path = '.', 
                          format = 'pdf', ...) {
    
    if(!is_figure(figure_object)) stop("An object of class 'figure' is required", call. = FALSE)
    if(!dir.exists(path)) stop('The target_path does not exist', call. = FALSE)
    
    filename <- paste(extract(figure_object, 'label'), format, sep = '.')

    message(paste('Saving:', 
                  filename, 
                  'to', 
                  path))

    ggplot2::ggsave(filename = filename, 
                    plot = plot(figure_object), 
                    path = path, 
                    width = width(figure_object), 
                    height = height(figure_object), 
                    units = extract(figure_object, 'unit'))
    
  }
  
# END -----