#' Add Lines of Text to a Path Diagram
#'
#' A function to add multiple lines of
#' text simultaneously to an existing path diagram
#'
#' @param string A vector of character strings to
#'   add to an existing plot.
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
#' @param shape.pad The percentage of the text
#'   dimensions to use when padding the width and
#'   height of the box around the text.
#' @param shape.pad_first Logical; if \code{TRUE}
#'   pad width and height of box around text based on
#'   the dimensions of the first line of text. Otherwise,
#'   pad based on the dimensions of the final line of text.
#' @param align A vector giving the alignment of the text,
#'   either \code{left}, \code{right}, or \code{center}.
#'   Values are recycled to match the number of lines.
#' @param cex A vector giving the size of the text.
#'   Values are recycled to match the number of lines.
#' @param col A vector giving the color of the text.
#'   Values are recycled to match the number of lines.
#' @param output Logical; if \code{TRUE} return list
#'   of coordinates for box around text.
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
#' nd = add_lines_of_text(
#'   string, x = .2, y = .7,
#'   cex = c( 3, 2, 1 ),
#'   col = c( 'black', 'red', 'blue' ),
#'   align = c( 'center', 'left', 'right' ),
#'   output = T,
#'   xpd = NA
#' )
#'
#' # Draw box around text
#' add_node_shape( nd, shape.col = NA )
#'
#' @export

add_lines_of_text = function( string,
                              x = .5,
                              y = .5,
                              pad = .65,
                              pad.fixed = F,
                              shape.pad = .5,
                              shape.pad_first = T,
                              align = 'left',
                              cex = 1.1,
                              col = 'black',
                              offset = 0,
                              output = F,
                              ... ) {

  # Number of lines
  n = length( string )

  # Vectorized inputs
  align = rep_len( align, n )
  cex = rep_len( cex, n )
  col = rep_len( col, n )

  # Starting position
  cur_y = y

  # Initialize list with node coordinates
  nd_pos = list(
    bottom = NA, left = NA, top = NA, right = NA,
    bottomleft = NA, topleft = NA, topright = NA, bottomright = NA
  )

  # Initialize variables to determine text box size
  nd_pos$top = c( NA, cur_y )
  x_left_right = c( NA, NA )
  y_top = NA
  first_sh = NA

  # Loop through lines of text
  for ( i in 1:n ) {

    # Determine alignment
    pos = NULL
    if ( align[i] == 'left' ) pos = 4
    if ( align[i] == 'right' ) pos = 2

    # Determine height of current string
    sh = strheight( string[i], cex = cex[i] )
    sw = strwidth( string[i], cex = cex[i] )

    text( x, cur_y, string[i], cex = cex[i], pos = pos,
          col = col[i], offset = offset, ... )

    # Variables to track dimensions of text box
    if ( i == 1 ) {
      # Top of text box
      y_top = y + sh/2
      # Height of initial text
      first_sh = sh

      # Left and right limits of text box
      if ( align[i] == 'left' ) {
        x_left_right[1] = x
        x_left_right[2] = x + sw
      }
      if ( align[i] == 'right' ) {
        x_left_right[1] = x - sw
        x_left_right[2] = x
      }
      if ( !align[i] %in% c( 'left', 'right' ) ) {
        x_left_right[1] = x - sw/2
        x_left_right[2] = x + sw/2
      }

    } else {

      # Left and right limits of text box
      if ( align[i] == 'left' ) {
        x_left_right[1] = min( x_left_right[1], x )
        x_left_right[2] = max( x_left_right[2], x + sw )
      }
      if ( align[i] == 'right' ) {
        x_left_right[1] = min( x_left_right[1], x - sw )
        x_left_right[2] = max( x_left_right[2], x )
      }
      if ( !align[i] %in% c( 'left', 'right' ) ) {
        x_left_right[1] = min( x_left_right[1], x - sw/2 )
        x_left_right[2] = max( x_left_right[2], x + sw/2 )
      }

    }

    # Determine spacing for next line
    if ( i < n ) {
      if ( !pad.fixed ) {
        cur_y = cur_y - sh/2 - sh*pad
      } else {
        cur_y = cur_y - sh/2 - pad
      }
    }

    # Close loop for lines of text
  }

  # Height of final text
  last_sh = sh


  if ( shape.pad_first ) {
    # If spacing is based on first line

    y_top = y_top + first_sh*shape.pad
    y_bottom = cur_y - last_sh/2 - first_sh*shape.pad

    x_left_right =
      x_left_right + c(-1,1)*first_sh*shape.pad

    # Close conditional for first line
  } else {
    # If spacing is based on last line

    y_top = y_top + last_sh*shape.pad
    y_bottom = cur_y - last_sh/2 - last_sh*shape.pad

    x_left_right =
      x_left_right + c(-1,1)*last_sh*shape.pad

    # Close conditional for last line
  }

  # Additional variables for text box specification
  x_left = x_left_right[1]
  x_right = x_left_right[2]
  x_center = x_left + diff( x_left_right )
  y_center = y_top - (y_top - y_bottom)/2


  # Update list of node coordinates for text box
  nd_pos$top = c( x_center, y_top )
  nd_pos$bottom = c( x_center, y_bottom )
  nd_pos$left = c( x_left, y_center )
  nd_pos$right = c( x_right, y_center )

  nd_pos$topleft = c( x_left, y_top )
  nd_pos$bottomright = c( x_right, y_bottom )
  nd_pos$bottomleft = c( x_left, y_bottom )
  nd_pos$topright = c( x_right, y_top )

  # Debugging for text box dimensions
  if ( FALSE ) {
    for ( j in 1:length( nd_pos ) ) {
      points( nd_pos[[j]][1], nd_pos[[j]][2],
              pch = 19, xpd = NA )
    }
  }

  if ( output ) {
    return( nd_pos )
  }
}

