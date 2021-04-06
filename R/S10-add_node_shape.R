#' Add Node Shape
#'
#' Given a list with the x and y-axis coordinates for the
#' bottom, left, top, and right of the internal text box,
#' draws a specified shape on an existing plot.
#'
#' @param nd A named list of vectors with the Cartesian
#'   coordinates for the bottom, left, top, and right of
#'   the internal text box for the node.
#' @param shape The default shape for nodes, either 'box',
#'   or 'circle'.
#' @param shape.col The fill color for the node.
#' @param shape.lwd The line wide for the node border.
#' @param shape.border The mode border color
#'   (\code{NA} will suppress the border).
#' @param shape.lty The line type for the node.
#' @param shape.x The fixed width for the x-axis.
#' @param shape.y The fixed height for the y-axis.
#' @param xpd A logical value or NA. If \code{FALSE}, all
#'   plotting is clipped to the plot region, if
#'   \code{TRUE}, all plotting is clipped to the figure
#'   region, and if NA, all plotting is clipped to the
#'   device region.
#'
#' @examples
#' # Empty plot
#' create_base_figure()
#'
#' # Named list of coordinates
#' nd = list(
#'   top = c( .5, .6 ),
#'   bottom = c( .5, .4 ),
#'   left = c( .4, .5 ),
#'   right = c( .6, .5 )
#' )
#'
#' # Rectangle
#' add_node_shape( nd )
#' # Ellipse
#' add_node_shape(
#'   nd, shape = 'circle',
#'   shape.border = 'blue', shape.lty = 2
#' )
#'
#' @export

add_node_shape = function( nd,
                           shape = 'box',
                           shape.col = 'white',
                           shape.lwd = 2,
                           shape.border = 'black',
                           shape.lty = 1,
                           shape.x = NA,
                           shape.y = NA,
                           xpd = NA ) {

  #< Draw rectangle
  if ( shape %in% c( 'box', 'rectangle', 'rect', 'square' ) ) {

    #<< No fixed dimensions for the x-axis
    if ( is.na( shape.x ) ) {

      x_coord = c(
        rep( nd$left[1], 2 ),
        rep( nd$right[1], 2 )
      )

      #>> Close conditional for no fixed dimensions
    } else {

      x_coord = c(
        rep( nd$bottom[1] - shape.x/2, 2 ),
        rep( nd$bottom[1] + shape.x/2, 2 )
      )

      #>> Close conditional for fixed dimensions
    }


    #<< No fixed dimensions for the y-axis
    if ( is.na( shape.y ) ) {

      y_coord = c(
        nd$top[2],
        rep( nd$bottom[2], 2 ),
        nd$top[2]
      )

      #> Close conditional for no fixed dimensions
    } else {

      y_coord = c(
        nd$left[2] - shape.y/2,
        rep( nd$left[2] + shape.y/2, 2 ),
        nd$left[2] - shape.y/2
      )

      #>> Close conditional for fixed dimensions
    }

    polygon( x_coord, y_coord,
             col = shape.col,
             lwd = shape.lwd,
             border = shape.border,
             lty = shape.lty,
             xpd = xpd )

    #>> Close conditional for rectangle
  }

  #< Draw ellipse
  if ( shape %in% c( 'circle', 'ellipse', 'circ', 'ell' ) ) {

    #<< No fixed dimensions for the x-axis
    if ( is.na( shape.x ) ) {

      x_coord = c( nd$left[1], nd$right[1] )

      #>> Close conditional for no fixed dimensions
    } else {

      x_coord = nd$center[1] + c( -shape.x/2, shape.x/2 )

      #>> Close conditional for fixed dimensions
    }


    #<< No fixed dimensions for the y-axis
    if ( is.na( shape.y ) ) {

      y_coord = c( nd$bottom[2], nd$top[2] )

      #>> Close conditional for no fixed dimensions
    } else {

      y_coord = nd$center[2] + c( -shape.y/2, shape.y/2 )

      #>> Close conditional for fixed dimensions
    }

    xc = x_coord[1] + diff( x_coord )/2
    yc = y_coord[1] + diff( y_coord )/2

    # Distance of center to foci
    ctf = diff( x_coord )/2

    # Semi-latus rectum
    slr = diff(y_coord)/2

    # Semi-major axis
    smja = sqrt( ( ctf )^2 + ( slr )^2 )

    # Semi-minor axis
    smna = sqrt( smja^2 - ctf^2 )

    # x and y coordinates for ellipse
    pts = seq( 0, 2 * pi, length.out = 100 )
    xv = smja * cos( pts ) + xc
    yv = smna * sin( pts ) + yc

    # Draw shape
    polygon( xv,
             yv,
             col = shape.col,
             border = shape.border,
             lwd = shape.lwd,
             lty = shape.lty,
             xpd = xpd )

    #> Close conditional for ellipse
  }

}

