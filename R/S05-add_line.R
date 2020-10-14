#' Add Line to Diagram
#'
#' Adds a line (e.g., to connect nodes)
#' to an existing diagram (see
#' \code{\link{create_base_figure}}.
#'
#' @param x A vector with 1) the x and y-axis
#'   coordinates, respectively, for the starting point
#'   of the line, and 2) the x and y-axis
#'   coordinates, respectively, for the ending point of
#'   the line.
#' @param start The x and y-axis coordinates for the
#'   starting position of the line.
#' @param end The x and y-axis coordinates for the
#'   ending position of the line.
#' @param col The color of the line.
#' @param lwd The thickness of the line.
#' @param check_for_style_guide Logical; if \code{TRUE},
#'   checks if a list called \code{style_guide}
#'   exists in the global environment; if so,
#'   default options for the line
#'   are based on the contents of this list.
#' @param ... Additional arguments for the
#'   \code{\link[graphics]{segments}} function.
#'
#' @examples
#' # Create diagram
#' create_base_figure( guidelines = F )
#' # Add line
#' add_line( c( 0, .5, 1, .5 ), lwd = 3, lty = 2 )
#'
#' # Add nodes
#' ndA = add_node( node_content( x = .25, y = .75, text = 'A' ) )
#' ndB = add_node( node_content( x = .75, y = .25, text = 'B' ) )
#'
#' # Link nodes
#' add_line( start = ndA$bottom, end = ndB$top )
#'
#' @export

add_line = function( x = NULL,
                     start = NULL,
                     end = NULL,
                     col = NULL,
                     lwd = NULL,
                     check_for_style_guide = T,
                     ... ) {

  if ( is.null( x ) ) {

    if ( !is.null( start ) & !is.null( end ) ) {

      x = c( start[1:2], end[1:2] )

    } else {
      stop( '' )
    }

  }

  if ( check_for_style_guide ) {

    if ( exists( 'style_guide', envir = .GlobalEnv ) ) {
      # Set options for line width and color based on
      # style guide
      if ( is.null( col ) ) {
        col = style_guide$line$col
      }
      if ( is.null( lwd ) ) {
        lwd = style_guide$line$lwd
      }
    }

  }

  if ( is.null( col ) ) col = par( 'fg')
  if ( is.null( lwd ) ) lwd = par( 'lwd')

  segments(
    # x and y for starting point
    x[1], x[2],
    # x and y for ending point
    x[3], x[4],
    # Line color
    col = col,
    # Line width
    lwd = lwd,
    # Additional plotting terms
    ...
  )

}
