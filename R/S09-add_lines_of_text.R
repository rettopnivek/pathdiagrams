#' Add Lines of Text to a Path Diagram
#'
#' A function to add multiple lines of
#' text simultaneously to an existing path diagram
#'
#' @param string ...
#' @param x The x-axis coordinate to place all
#'   lines of text.
#' @param y The y-axis coordinate for the top
#'   line of the text.
#' @param pad The spacing between lines of text, either
#'   as a percentage of the text height or as a fixed
#'   amount.
#' @param pad.fixed Logical; if \code{TRUE} spacing
#'   between lines will be by a fixed amount rather
#'   than a percentage.
#' @param align A vector giving the alignment of the text,
#'   either \code{left}, \code{right}, or \code{center}.
#'   Values are recycled to match the number of lines.
#' @param cex A vector giving the size of the text.
#'   Values are recycled to match the number of lines.
#' @param col A vector giving the color of the text.
#'   Values are recycled to match the number of lines.
#' @param ... Additional arguments to the
#'   \code{\link[graphics]{text}} function.
#'
#' @examples
#' # Empty figure
#' create_base_figure()
#'
#' # Several lines of text
#' string = c(
#'   'Header',
#'   'Some values: 1, 2, 3',
#'   expression( y[i] == beta[0] + beta[1]*x[i] + epsilon[i] )
#' )
#'
#' # Add lines to figure
#' add_lines_of_text( string )
#'
#' # Additional lines of text
#' string = c(
#'   'Here',
#'   '...here',
#'   'or here!'
#' )
#'
#' # Vectorized options for size, color,
#' # and alignment
#' add_lines_of_text(
#'   string, x = .2, y = .7,
#'   cex = c( 3, 2, 1 ),
#'   col = c( 'black', 'red', 'blue' ),
#'   align = c( 'center', 'left', 'right' ),
#'   xpd = NA
#' )
#'
#' @export

add_lines_of_text = function( string,
                              x = .5,
                              y = .5,
                              pad = .65,
                              pad.fixed = F,
                              align = 'left',
                              cex = 1.1,
                              col = 'black',
                              ... ) {

  # Number of lines
  n = length( string )

  # Vectorized inputs
  align = rep_len( align, n )
  cex = rep_len( cex, n )
  col = rep_len( col, n )

  # Starting position
  cur_y = y

  # Loop through lines of text
  for ( i in 1:n ) {

    # Determine alignment
    pos = NULL
    if ( align[i] == 'left' ) pos = 4
    if ( align[i] == 'right' ) pos = 2

    # Determine height of current string
    sh = strheight( string[i], cex = cex[i] )

    text( x, cur_y, string[i], cex = cex[i], pos = pos,
          col = col[i], ... )

    # Determine spacing for next line
    if ( !pad.fixed ) {
      cur_y = cur_y - sh/2 - sh*pad
    } else {
      cur_y = cur_y - sh/2 - pad
    }

    # Close loop for lines of text
  }

}



