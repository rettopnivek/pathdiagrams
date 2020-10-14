#' Add Arrow to Diagram
#'
#' Adds an arrow to an existing diagram (see
#' \code{\link{create_base_figure}}.
#'
#' @param x A vector with 1) the x and y-axis
#'   coordinates, respectively, for the starting point
#'   of the arrow, and 2) the x and y-axis
#'   coordinates, respectively, for the ending point of
#'   the arrow.
#' @param start The x and y-axis coordinates for the
#'   starting position of the arrow
#' @param end The x and y-axis coordinates for the
#'   ending position of the arrow.
#' @param col The color of the arrow
#' @param lwd The thickness of the arrow.
#' @param draw_arrowhead_at The location at which to draw the
#'   arrow head, either...
#'   \itemize{
#'     \item 'end';
#'     \item 'start';
#'     \item 'both'.
#'   }
#' @param check_for_style_guide Logical; if \code{TRUE},
#'   checks if a list called \code{style_guide}
#'   exists in the global environment; if so,
#'   default options for the arrow
#'   are based on the contents of this list.
#' @param ... Additional arguments for the
#'   \code{\link[graphics]{arrows}} function.
#'
#' @examples
#' # Create diagram
#' create_base_figure( guidelines = F )
#' # Add line
#' add_arrow( c( .25, .25, .75, .75 ), lwd = 3, lty = 2 )
#'
#' @export

add_arrow = function( x = NULL,
                      start = NULL,
                      end = NULL,
                      col = NULL,
                      lwd = NULL,
                      length = NULL,
                      draw_arrowhead_at = 'end',
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
      if ( is.null( length ) ) {
        length = style_guide$line$length
      }
    }

  }

  if ( is.null( col ) ) col = par( 'fg')
  if ( is.null( lwd ) ) lwd = par( 'lwd')
  if ( is.null( length ) ) length = .25

  code_for_arrows = 2
  if ( draw_arrowhead_at == 'start' ) code_for_arrows = 1
  if ( draw_arrowhead_at == 'both' ) code_for_arrows = 3

  arrows(
    # x and y-axis coordinates for start point
    x[1], x[2],
    # x and y-axis coordinates for end point
    x[3], x[4],
    code = code,
    lwd = lwd,
    col = col,
    length = length,
    ...
  )

}

