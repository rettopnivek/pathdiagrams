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
#' @param spacing The spacing between lines of text, either
#'   as a percentage of the text height or as a fixed
#'   amount.
#' @param spacing.fixed Logical; if \code{TRUE} spacing
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
#' @param shape The default shape for nodes, either 'box',
#'   or 'circle'.
#' @param shape.col The fill color for the node.
#' @param shape.lwd The line wide for the node border.
#' @param shape.border The mode border color
#'   (\code{NA} will suppress the border).
#' @param shape.lty The line type for the node.
#' @param shape.x The fixed width for the x-axis.
#' @param shape.y The fixed height for the y-axis.
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
#'   '   Indent',
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
                              spacing = .65,
                              spacing.fixed = F,
                              shape.pad = .5,
                              shape.pad_first = T,
                              align = 'left',
                              cex = 1.1,
                              col = 'black',
                              offset = 0,
                              output = F,
                              shape = 'blank',
                              shape.col = 'white',
                              shape.lwd = 2,
                              shape.border = 'black',
                              shape.lty = 1,
                              shape.x = NA,
                              shape.y = NA,
                              ... ) {

  # Number of lines
  n = length( string )

  # Vectorized inputs
  align = rep_len( align, n )
  cex = rep_len( cex, n )
  col = rep_len( col, n )

  # Starting position

  exst_plt = par( "usr" )
  cur_y = exst_plt[3] + (exst_plt[4] - exst_plt[3])/2
  cur_x = exst_plt[1] + (exst_plt[2] - exst_plt[1])/2

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

  # Variables for x and y-axis positions
  x_pos = rep( NA, n )
  y_pos = rep( NA, n )

  # Loop through lines of text
  for ( i in 1:n ) {

    # Determine alignment
    pos = NULL
    if ( align[i] == 'left' ) pos = 4
    if ( align[i] == 'right' ) pos = 2

    # Determine height of current string
    sh = strheight( string[i], cex = cex[i] )
    sw = strwidth( string[i], cex = cex[i] )

    # Save y-axis position for each line of text
    y_pos[i] = cur_y

    # Variables to track dimensions of text box
    if ( i == 1 ) {
      # Top of text box
      y_top = cur_y + sh/2
      # Height of initial text
      first_sh = sh

      # Left and right limits of text box
      if ( align[i] == 'left' ) {
        x_left_right[1] = cur_x
        x_left_right[2] = cur_x + sw
      }
      if ( align[i] == 'right' ) {
        x_left_right[1] = cur_x - sw
        x_left_right[2] = cur_x
      }
      if ( !align[i] %in% c( 'left', 'right' ) ) {
        x_left_right[1] = cur_x - sw/2
        x_left_right[2] = cur_x + sw/2
      }

    } else {

      # Left and right limits of text box
      if ( align[i] == 'left' ) {
        x_left_right[1] = min( x_left_right[1], cur_x )
        x_left_right[2] = max( x_left_right[2], cur_x + sw )
      }
      if ( align[i] == 'right' ) {
        x_left_right[1] = min( x_left_right[1], cur_x - sw )
        x_left_right[2] = max( x_left_right[2], cur_x )
      }
      if ( !align[i] %in% c( 'left', 'right' ) ) {
        x_left_right[1] = min( x_left_right[1], cur_x - sw/2 )
        x_left_right[2] = max( x_left_right[2], cur_x + sw/2 )
      }

    }

    # Determine spacing for next line
    if ( i < n ) {
      if ( !spacing.fixed ) {
        cur_y = cur_y - sh/2 - sh*spacing
      } else {
        cur_y = cur_y - sh/2 - spacing
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

    final_pad = first_sh*shape.pad

    # Close conditional for first line
  } else {
    # If spacing is based on last line

    y_top = y_top + last_sh*shape.pad
    y_bottom = cur_y - last_sh/2 - last_sh*shape.pad

    x_left_right =
      x_left_right + c(-1,1)*last_sh*shape.pad

    final_pad = last_sh*shape.pad

    # Close conditional for last line
  }

  # Center based on user-supplied x and y coordinates
  x_center = x
  y_center = y

  x_left = x_center - abs( diff( x_left_right )/2 )
  x_right = x_center + abs( diff( x_left_right )/2 )

  y_top_bottom = c( y_top, y_bottom )
  y_top = y_center + abs( diff( y_top_bottom ) )/2
  y_bottom = y_center - abs( diff( y_top_bottom ) )/2

  # Update list of node coordinates for text box
  nd_pos$top = c( x_center, y_top )
  nd_pos$bottom = c( x_center, y_bottom )
  nd_pos$left = c( x_left, y_center )
  nd_pos$right = c( x_right, y_center )

  nd_pos$topleft = c( x_left, y_top )
  nd_pos$bottomright = c( x_right, y_bottom )
  nd_pos$bottomleft = c( x_left, y_bottom )
  nd_pos$topright = c( x_right, y_top )

  # Add shape
  pathdiagrams::add_node_shape(
    nd_pos,
    shape = shape,
    shape.col = shape.col,
    shape.lwd = shape.lwd,
    shape.border = shape.border,
    shape.lty = shape.lty,
    shape.x = shape.x,
    shape.y = shape.y
  )

  if ( !is.na( shape.x ) ) {

    nd_pos$left[1] = x_center - shape.x/2
    nd_pos$topleft[1] = x_center - shape.x/2
    nd_pos$bottomleft[1] = x_center - shape.x/2

    nd_pos$right[1] = x_center + shape.x/2
    nd_pos$topright[1] = x_center + shape.x/2
    nd_pos$bottomright[1] = x_center + shape.x/2

  }

  # Loop over lines
  for ( i in 1:n ) {

    # Determine alignment
    pos = NULL
    cur_x = x_center
    if ( align[i] == 'left' ) {
      pos = 4
      cur_x = nd_pos$left[1] + final_pad
    }
    if ( align[i] == 'right' ) {
      pos = 2
      cur_x = nd_pos$right[1] - final_pad
    }

    # Add text
    text( cur_x, (y_pos[i] - y_top_bottom[1]) + y_top,
          string[i], cex = cex[i], pos = pos,
          col = col[i], offset = offset, ... )

    # Close loop over lines
  }

  # Debugging for text box dimensions
  if ( FALSE ) {
    for ( j in 1:length( nd_pos ) ) {
      points( nd_pos[[j]][1], nd_pos[[j]][2],
              pch = 19, xpd = NA )
    }
    points( nd_pos$top[1], nd_pos$left[2], pch = 22 )
  }

  if ( output ) {
    return( nd_pos )
  }
}

