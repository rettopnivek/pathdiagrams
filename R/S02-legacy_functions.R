# Legacy functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2024-09-13

# Table of contents
# 1) create_base_figure
# 2) add_node_shape
# 3) add_lines_of_text
# 4) add_nodes
# 5) size
# 6) sep
# 7) replace_with_values
# 8) distribute
# 9) multiple_node_aes
# 10) plain_bold_italic_text

#### 1) create_base_figure ####
#' Create Base Figure for Diagram
#'
#' A function that generates a base figure,
#' with optional guidelines, for a diagram.
#'
#' @param default Pre-packaged figure sizes, including...
#'   \itemize{
#'     \item 'US letter' or '8.5 x 11' (inches);
#'     \item '3.54 x 3.54' (inches);
#'     \item '5 x 5' (inches);
#'     \item '7.25 x 5' (inches).
#'   }
#' @param w Width (in inches) for the x-axis.
#' @param h Height (in inches) for the y-axis.
#' @param orientation For default specifications, indicates
#'   whether figures should be in \code{landscape} or
#'   \code{portrait} style.
#' @param margin A vector with the margins (in inches) for
#'   the bottom, left, top, and right, respectively.
#' @param guidelines Logical; if \code{TRUE}, includes
#'   guidelines when generating the figure.
#' @param guide_major Vector giving the major guideline
#'   positions (values must be between 0 and 1).
#' @param guide_minor Vector giving the minor guideline
#'   positions (values must be between 0 and 1).
#'   If \code{NULL}, no minor guidelines are included.
#' @param guide_adjust An adjustment controlling the
#'   position on the axes for the guideline numbers.
#' @param new Logical; if \code{TRUE} a new plotting
#'   window is generated.
#'
#' @examples
#' # Default (6 x 6 inches)
#' create_base_figure()
#'
#' # No guidelines
#' create_base_figure( guidelines = FALSE )
#'
#' # US letter size
#' create_base_figure( default = 'US letter' )
#'
#' # US letter (Portrait)
#' create_base_figure( default = 'US letter', orientation = 'portrait' )
#'
#' @export

create_base_figure = function( default = NULL,
                               w = 6, h = 6,
                               orientation = 'landscape',
                               margin = rep( .25, 4 ),
                               guidelines = TRUE,
                               guide_major = seq( .1, .9, .1 ),
                               guide_minor = seq( .05, .95, .1 ),
                               guide_adjust = 1.25,
                               new = FALSE ) {

  # If a default argument is provided
  if ( !is.null( default ) ) {

    # US letter
    if ( default %in% c( 'US letter',
                         '8.5 x 11 in', '8.5 x 11' ) ) {

      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        w = 11
        h = 8.5
      }
      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        w = 8.5
        h = 11
      }

      # Close 'US letter'
    }

    # Science guidelines for figure sizes (small)
    if ( default %in% c( '3.54 x 3.54 in', '3.54 x 3.54', '3.54',
                         '9 x 9 cm', '9' ) ) {

      w = 3.54
      h = 3.54

      # Close 'Science guidelines for figure sizes (small)'
    }

    # Science guidelines for figure sizes (medium)
    if ( default %in% c( '5 x 5 in', '5 x 5', '5',
                         '12.7 x 12.7 cm', '12.7' ) ) {

      w = 5
      h = 5

      # Close 'Science guidelines for figure sizes (medium)'
    }

    # Science guidelines for figure sizes (wide)
    if ( default %in% c( '7.25 x 5 in', '7.25 x 5', '7.25',
                         '18.4 x 12.7 cm', '18.4 x 12.7', '18.4' ) ) {

      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        w = 7.25
        h = 5
      }

      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        w = 5
        h = 7.25
      }

      # Close 'Science guidelines for figure sizes (wide)'
    }

    # Close 'If a default argument is provided'
  }

  # Create plotting window
  if ( new ) x11( width = w, height = h )

  # Specify margins
  par( mai = margin )

  # Create blank plot
  plot(
    c( 0, 1 ), c( 0, 1 ),
    xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '',
    bty = 'n', type = 'n'
  )

  # Add guidelines to figure
  if ( guidelines ) {

    plot_range = list(
      start = rep( 0, length( guide_major ) ),
      end = rep( 1, length( guide_major ) )
    )

    # Grid lines
    segments(  guide_major,
               plot_range$start,
               guide_major,
               plot_range$end,
               col = 'grey85' )
    segments(  plot_range$start,
               guide_major,
               plot_range$end,
               guide_major,
               col = 'grey85' )

    # Lines at minor tick positions
    if ( !is.null( guide_minor ) ) {

      plot_range = list(
        start = rep( 0, length( guide_minor ) ),
        end = rep( 1, length( guide_minor ) )
      )

      # Grid lines
      segments(  guide_minor,
                 plot_range$start,
                 guide_minor,
                 plot_range$end,
                 col = 'grey85',
                 lty = 3 )
      segments(  plot_range$start,
                 guide_minor,
                 plot_range$end,
                 guide_minor,
                 col = 'grey85',
                 lty = 3 )

      # Close 'Lines at minor tick positions'
    }

    # Outer margins
    abline( v = 0, xpd = NA )
    abline( h = 0, xpd = NA )
    abline( v = 1, xpd = NA )
    abline( h = 1, xpd = NA )

    # Plotting dimensions

    # Plot window
    dm_outer = dev.size(); names( dm_outer ) = c( 'x', 'y' )
    # Margins
    dm_margins = par( 'mai' )
    # Plotting window
    dm_inner = c(
      dm_outer[1] - dm_margins[2] - dm_margins[4],
      dm_outer[2] - dm_margins[1] - dm_margins[3]
    )

    text(
      guide_major,
      rep( ( dm_outer[2] - guide_adjust * dm_margins[3] ) / dm_inner[2],
           length( guide_major ) ),
      guide_major,
      xpd = NA
    )

    text(
      rep( 1 - ( dm_outer[1] - guide_adjust * dm_margins[2] ) / dm_inner[1],
           length( guide_major ) ),
      guide_major,
      guide_major,
      xpd = NA
    )

    # Close 'Add guidelines to figure'
  }

  # Reset margins
  par( mar = c( 5, 4, 4, 2 ) + .1 )
}

#### 2) add_node_shape ####

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

  # Draw rectangle
  if ( shape %in% c( 'box', 'rectangle', 'rect', 'square' ) ) {

    # No fixed dimensions for the x-axis
    if ( is.na( shape.x ) ) {

      x_coord = c(
        rep( nd$left[1], 2 ),
        rep( nd$right[1], 2 )
      )

      # Close 'No fixed dimensions for the x-axis'
    } else {

      x_coord = c(
        rep( nd$bottom[1] - shape.x/2, 2 ),
        rep( nd$bottom[1] + shape.x/2, 2 )
      )

      # Close else for 'No fixed dimensions for the x-axis'
    }


    # No fixed dimensions for the y-axis
    if ( is.na( shape.y ) ) {

      y_coord = c(
        nd$top[2],
        rep( nd$bottom[2], 2 ),
        nd$top[2]
      )

      # Close 'No fixed dimensions for the y-axis'
    } else {

      y_coord = c(
        nd$left[2] - shape.y/2,
        rep( nd$left[2] + shape.y/2, 2 ),
        nd$left[2] - shape.y/2
      )

      # Close else for 'No fixed dimensions for the y-axis'
    }

    polygon( x_coord, y_coord,
             col = shape.col,
             lwd = shape.lwd,
             border = shape.border,
             lty = shape.lty,
             xpd = xpd )

    # Close 'Draw rectangle'
  }

  # Draw ellipse
  if ( shape %in% c( 'circle', 'ellipse', 'circ', 'ell' ) ) {

    # No fixed dimensions for the x-axis
    if ( is.na( shape.x ) ) {

      x_coord = c( nd$left[1], nd$right[1] )

      # Close 'No fixed dimensions for the x-axis'
    } else {

      x_coord = nd$center[1] + c( -shape.x/2, shape.x/2 )

      # Close else for 'No fixed dimensions for the x-axis'
    }


    # No fixed dimensions for the y-axis
    if ( is.na( shape.y ) ) {

      y_coord = c( nd$bottom[2], nd$top[2] )

      # Close 'No fixed dimensions for the y-axis'
    } else {

      y_coord = nd$center[2] + c( -shape.y/2, shape.y/2 )

      # Close else for 'No fixed dimensions for the y-axis'
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

    # Close 'Draw ellipse'
  }

}

#### 3) add_lines_of_text ####

#' Add Lines of Text to a Path Diagram
#'
#' A function to add multiple lines of
#' text simultaneously to an existing path diagram.
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
#' @param ignore_asterisk Logical; if \code{TRUE} ignores
#'   asterisks for dimension purposes since they are used
#'   to indicate bold/italic font.
#' @param add Logical; if \code{TRUE} adds nodes (and
#'   paths if specified) to an existing figure.
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
                              spacing = .8,
                              spacing.fixed = F,
                              shape.pad = .5,
                              shape.pad_first = T,
                              align = 'left',
                              cex = 1.1,
                              col = 'black',
                              offset = 0,
                              output = FALSE,
                              shape = 'blank',
                              shape.col = 'white',
                              shape.lwd = 2,
                              shape.border = 'black',
                              shape.lty = 1,
                              shape.x = NA,
                              shape.y = NA,
                              xpd = NA,
                              ignore_asterisk = TRUE,
                              add = TRUE,
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

    # Determine width/height of current string

    # If asterisks should be ignored
    if ( ignore_asterisk ) {

      sh = strheight(
        gsub( '*', '', string[i], fixed = T ),
        cex = cex[i]
      )
      sw = strwidth(
        gsub( '*', '', string[i], fixed = T ),
        cex = cex[i]
      )

      # Close 'If asterisks should be ignored'
    } else {

      sh = strheight( string[i], cex = cex[i] )
      sw = strwidth( string[i], cex = cex[i] )

      # Close else for 'If asterisks should be ignored'
    }

    # Save y-axis position for each line of text
    y_pos[i] = cur_y

    # Variables to track dimensions of text box

    # First line
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

      # Close 'First line'
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

      # Close else for 'First line'
    }

    # Determine spacing for next line
    if ( i < n ) {

      if ( !spacing.fixed ) {
        cur_y = cur_y - sh/2 - sh*spacing
      } else {
        cur_y = cur_y - sh/2 - spacing
      }

      # Close 'Determine spacing for next line'
    }

    # Close 'Loop through lines of text'
  }

  # Height of final text
  last_sh = sh

  # If spacing is based on first line
  if ( shape.pad_first ) {

    y_top = y_top + first_sh*shape.pad
    y_bottom = cur_y - last_sh/2 - first_sh*shape.pad

    x_left_right =
      x_left_right + c(-1,1)*first_sh*shape.pad

    final_pad = first_sh*shape.pad

    # Close 'If spacing is based on first line'
  } else {
    # If spacing is based on last line

    y_top = y_top + last_sh*shape.pad
    y_bottom = cur_y - last_sh/2 - last_sh*shape.pad

    x_left_right =
      x_left_right + c(-1,1)*last_sh*shape.pad

    final_pad = last_sh*shape.pad

    # Close else for 'If spacing is based on first line'
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

  # If specified add shape
  if ( add ) {

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

    # Close 'If specified add shape'
  }

  # If shape was added
  if ( !is.na( shape.x ) ) {

    nd_pos$left[1] = x_center - shape.x/2
    nd_pos$topleft[1] = x_center - shape.x/2
    nd_pos$bottomleft[1] = x_center - shape.x/2

    nd_pos$right[1] = x_center + shape.x/2
    nd_pos$topright[1] = x_center + shape.x/2
    nd_pos$bottomright[1] = x_center + shape.x/2

    # Close 'If shape was added'
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

    # If asterisk found
    if ( grepl( '*', string[i], fixed = T ) &
         is.character( string[i] ) ) {

      # If specified add text
      if ( add ) {

        plain_bold_italic_text(
          cur_x, (y_pos[i] - y_top_bottom[1]) + y_top,
          string[i], cex = cex[i], pos = pos,
          col = col[i], offset = offset, xpd = xpd, ... )

        # Close 'If specified add text'
      }

      # Close 'If asterisk found'
    } else {

      # If specified add text
      if ( add ) {

        text( cur_x, (y_pos[i] - y_top_bottom[1]) + y_top,
              string[i], cex = cex[i], pos = pos,
              col = col[i], offset = offset, xpd = xpd, ... )

        # Close 'If specified add text'
      }

      # Close else for 'If asterisk found'
    }

    # Close 'Loop over lines'
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

#### 4) add_nodes ####

#' Add Multiple Nodes to a Path Diagram
#'
#' Function to add multiple nodes (and associated
#' paths) to an existing path diagram.
#'
#' @param input A vector of labeled character strings; see details
#'   for more information.
#' @param paths An optional vector of labeled character strings;
#'   see details for more information.
#' @param output Logical; if \code{TRUE} returns a list with the
#'   x and y-axis coordinates for each node.
#' @param shape The default shape for nodes, either 'box',
#'   'circle', or 'blank'; options for individual nodes can be
#'   specified with the tag \code{ns=}.
#' @param shape.col The default color for nodes; options
#'   for individual nodes can be specified with the tag \code{nc=}.
#' @param shape.lwd The default line width for node borders;
#'   options for individual nodes can be specified with the
#'   tag \code{nw=}.
#' @param shape.border The default border color for nodes;
#'   options for individual nodes can be specified with the
#'   tag \code{nb=}.
#' @param shape.lty The default border line type for nodes;
#'   options for individual nodes can be specified with the
#'   tag \code{nt=}.
#' @param shape.pad The default space between lines of text
#'   for nodes; options for individual nodes can be specified
#'   with the tag \code{np=}.
#' @param shape.x The default fixed width for nodes;
#'   options for individual nodes can be specified with the
#'   tag \code{nx=}.
#' @param shape.y The default fixed height for nodes;
#'   options for individual nodes can be specified with the
#'   tag \code{ny=}.
#' @param text.size The default size for text content;
#'   options for individual nodes can be specified with the
#'   tag \code{ts=}.
#' @param text.col The default color for text content;
#'   options for individual nodes can be specified with the
#'   tag \code{tc=}.
#' @param text.spacing The space between multiple lines of text;
#'   options for individual nodes can be specified with the
#'   tag \code{th=}.
#' @param path.pad The space between a line or arrow and a node;
#'   options for individual nodes can be specified with the
#'   tag \code{lp=}.
#' @param path.lwd The default line width for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{lw=}.
#' @param path.col The default line color for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{lc=}.
#' @param path.length The default arrowhead length for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{ll=}.
#' @param path.angle The default angle of arrowheads for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{la=}.
#' @param path.lty The default line type for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{lt=}.
#' @param path.code The default arrow direction for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{cd=}. Directions
#'   are specified with the symbols \code{->}, \code{<-},
#'   \code{<->}, or \code{-}.
#' @param ignore_asterisk Logical; if \code{TRUE} ignores
#'   asterisks for dimension purposes since they are used
#'   to indicate bold/italic font.
#' @param add Logical; if \code{TRUE} adds nodes (and
#'   paths if specified) to an existing figure.
#'
#' @details
#'
#' Each node is specified via a text string in the format:
#'
#' \code{"Text content|x=value|y=value|..."}
#'
#' substituting \code{value} with the respective x and
#' y-axis coordinate positions for the node, and \code{...}
#' referring to additional options controlling node
#' aesthetics.
#'
#' Options are specified as a tag followed by a value; for
#' example, to draw a node as an ellipse, one uses the
#' combined tag and value: \code{ns=circle}. Multiple
#' options can be specified by separating them with the
#' \code{|} symbol.
#'
#' Paths (lines or arrows) can be drawn between existing
#' nodes whose string input is labeled, via the format:
#'
#' \code{"Label|coordinate|Label|coordinate|..."}
#'
#' where \code{Label} is the label in the string
#' vector (i.e., the name attribute), and \code{coordinate}
#' is one of eight values: \code{bottom}, \code{left},
#' \code{top}, \code{right}, \code{bottomleft}, \code{topleft},
#' \code{topright}, or \code{bottomright}.
#'
#' As before, additional aesthetic options can be specified
#' via a tag and value, with multiple cases separated by the
#' \code{|} symbol. For example, the width of a path line
#' can be set via the tag and value: \code{lt=2}.
#'
#' @examples
#' # Define vector of string inputs for nodes to draw
#' input = c(
#'   # [Node label] = "Text|x=value|y=value|..."
#'   N01 = 'Node-01|x=.2|y=.33',
#'   # Set node shape to ellipse; resize and color text
#'   N02 = 'Node-02|x=.5|y=.66|ns=circle|ts=2|tc=blue',
#'   # Color node and remove border
#'   N03 = 'Node-03|x=.8|y=.33|nc=grey80|nb=NA'
#' )
#'
#' # Define vector of string inputs to draw paths
#' # between labeled nodes specified in 'input'
#' paths = c(
#'   # Start of path    End of path      Options
#'   # Label|coordinate|Label|coordinate|...
#'   'N01|right|N03|left',
#'   # Connect various nodes and coordinates
#'   'N02|right|N03|top',
#'   'N02|bottomright|N03|topleft',
#'   # Orange dashed thick line
#'   'N01|top|N02|bottom|lc=orange|lt=2|lw=4',
#'   # Blue double-headed arrow with small arrowhead
#'   'N01|topright|N02|bottomleft|lc=blue|ll=.1|cd=<->'
#' )
#'
#' # Create empty figure
#' create_base_figure()
#'
#' # Add nodes and paths
#' add_nodes(
#'   input, paths = paths
#' )
#'
#' @export

add_nodes = function( input,
                      paths = NULL,
                      output = F,
                      # Default values
                      #   Node shape
                      shape = 'box',
                      shape.col = 'white',
                      shape.lwd = 2,
                      shape.border = 'black',
                      shape.lty = 1,
                      shape.pad = .5,
                      shape.x = NA,
                      shape.y = NA,
                      #   Node text
                      text.size = 1.25,
                      text.col = 'black',
                      text.spacing = NULL,
                      #   Path line
                      path.pad = .025,
                      path.lwd = 2,
                      path.col = 'black',
                      path.length = .1,
                      path.angle = 30,
                      path.lty = 1,
                      path.code = '->',
                      #  Misc. options
                      xpd = NA,
                      ignore_asterisk = TRUE,
                      add = TRUE ) {

  # Default options for text spacing
  if ( is.null( text.spacing ) ) {

    # Default
    text.spacing = .05

    # Scale text spacing
    current_spacing = text.spacing

    # Text height with default cex = 1.25
    def_text_h = strheight( 'A', cex = 1.25 )

    # Specified height
    cur_text_h = strheight( 'A', cex = text.size )

    text.spacing = current_spacing * ( cur_text_h / def_text_h )

    # Close 'Default options for text spacing'
  }

  # Specify default settings for node aesthetics
  lod = list(
    # Node shape
    shape = shape,
    shape.col = shape.col,
    shape.lwd = shape.lwd,
    shape.border = shape.border,
    shape.lty = shape.lty,
    shape.pad = shape.pad,
    shape.x = shape.x,
    shape.y = shape.y,
    # Node text
    text.size = text.size,
    text.col = text.col,
    text.spacing = text.spacing,
    # Path line
    path.pad = path.pad,
    path.lwd = path.lwd,
    path.col = path.col,
    path.length = path.length,
    path.angle = path.angle,
    path.lty = path.lty,
    path.code = path.code
  )

  # Initialize list with node coordinates
  nd_pos = list(
    bottom = NA, left = NA, top = NA, right = NA,
    bottomleft = NA, topleft = NA, topright = NA, bottomright = NA
  )
  nd = lapply( 1:length( input ), function(x) nd_pos )
  names( nd ) = names( input )

  # Loop over inputs
  for ( i in 1:length( input ) ) {

    # Extract details on current node
    input_parts = strsplit( input[ i ], split = '|', fixed = T )[[1]]

    # Check for additional options

    # Node options
    shape = multiple_node_aes( 'ns=', input_parts, lod )
    shape.col = multiple_node_aes( 'nc=', input_parts, lod )
    shape.lwd = multiple_node_aes( 'nw=', input_parts, lod )
    shape.border = multiple_node_aes( 'nb=', input_parts, lod )
    shape.lty = multiple_node_aes( 'nt=', input_parts, lod )
    shape.pad = multiple_node_aes( 'np=', input_parts, lod )
    shape.x = multiple_node_aes( 'nx=', input_parts, lod )
    shape.y = multiple_node_aes( 'ny=', input_parts, lod )

    # Text options
    text.size = multiple_node_aes( 'ts=', input_parts, lod )
    text.color = multiple_node_aes( 'tc=', input_parts, lod )
    text.spacing = multiple_node_aes( 'th=', input_parts, lod )

    # At a minimum
    # Text | x-axis coordinates | y-axis coordinates

    # Extract coordinates
    xp = as.numeric( gsub( 'x=', '', input_parts[2] ) )
    yp = as.numeric( gsub( 'y=', '', input_parts[3] ) )

    # Determine width/height of text

    # If asterisks should be ignored
    if ( ignore_asterisk ) {

      sh = strheight(
        gsub( '*', '', input_parts[1], fixed = T ),
        cex = text.size
      )
      sw = strwidth(
        gsub( '*', '', input_parts[1], fixed = T ),
        cex = text.size
      )

      # Close 'If asterisks should be ignored'
    } else {

      sw = strwidth( input_parts[1], cex = text.size )
      sh = strheight( input_parts[1], cex = text.size )

      # Close else for 'If asterisks should be ignored'
    }

    # Pad dimensions of node to be slightly
    # larger than text content
    adj = sh * shape.pad

    # x-axis lower and upper boundaries
    if ( is.na( shape.x ) ) {
      # If no fixed dimensions are provided

      # Size based on string dimensions
      xb = c( xp - sw/2 - adj,
              xp + sw/2 + adj )

      # Close 'x-axis lower and upper boundaries'
    } else {

      xb = c( xp - shape.x/2,
              xp + shape.x/2 )

      # Close else for 'x-axis lower and upper boundaries'
    }

    # y-axis lower and upper boundaries
    if ( is.na( shape.y ) ) {
      # If no fixed dimensions are provided

      # Size based on string dimensions
      yb = c( yp - sh/2 - adj,
              yp + sh/2 + adj )

      # Close 'y-axis lower and upper boundaries'
    } else {

      yb = c( yp - shape.y/2,
              yp + shape.y/2 )

      # Close else for 'y-axis lower and upper boundaries'
    }

    # Coordinates for node
    nd[[ i ]]$left = c( xb[1], yp )
    nd[[ i ]]$right = c( xb[2], yp )

    nd[[ i ]]$top = c( xp, yb[2] )
    nd[[ i ]]$bottom = c( xp, yb[1] )

    nd[[ i ]]$bottomleft = c( xb[1], yb[1] )
    nd[[ i ]]$bottomright = c( xb[2], yb[1] )

    nd[[ i ]]$topleft = c( xb[1], yb[2] )
    nd[[ i ]]$topright = c( xb[2], yb[2] )

    # Check if single line
    if ( !grepl( '\n', input_parts[1], fixed = T ) ) {

      # Add shape around node

      # Draw a rectangle around the text
      if ( shape %in% c( 'box', 'rectangle', 'rect', 'square' ) ) {

        # Draw shape
        if ( add ) {

          polygon(
            xb[c(1,1,2,2)],
            yb[c(1,2,2,1)],
            col = shape.col,
            border = shape.border,
            lwd = shape.lwd,
            lty = shape.lty,
            xpd = xpd
          )

          # Close 'Draw shape'
        }

        # Close 'Draw a rectangle around the text'
      }

      # Draw an ellipse around node
      if ( shape %in% c( 'circle', 'ellipse', 'circ', 'ell' ) ) {

        # Distance of center to foci
        ctf = diff( xb )/2

        # Semi-latus rectum
        slr = diff(yb)/2

        # Semi-major axis
        smja = sqrt( ( ctf )^2 + ( slr )^2 )

        # Semi-minor axis
        smna = sqrt( smja^2 - ctf^2 )

        # x and y coordinates for ellipse
        pts = seq( 0, 2 * pi, length.out = 100 )
        xv = smja * cos( pts ) + xp
        yv = smna * sin( pts ) + yp

        # Draw shape
        if ( add ) {

          polygon(
            xv,
            yv,
            col = shape.col,
            border = shape.border,
            lwd = shape.lwd,
            lty = shape.lty,
            xpd = xpd
          )

          # Close 'Draw shape'
        }

        # Close 'Draw an ellipse around node'
      }

      # If asterisk found
      if ( grepl( '*', input_parts[1], fixed = T ) &
           is.character( input_parts[1] ) ) {

        # If specified add text
        if ( add ) {

          plain_bold_italic_text(
            xp, yp,
            input_parts[1], cex = text.size,
            col = text.color, xpd = xpd
          )

          # Close 'If specified add text'
        }

        # Close 'If asterisk found'
      } else {

        # If specified add text
        if ( add ) {

          text(
            xp, yp,
            input_parts[1],
            cex = text.size,
            col = text.color,
            xpd = xpd
          )

          # Close 'If specified add text'
        }

        # Close else for 'If asterisk found'
      }

      # Close 'Check if single line'
    } else {
      # Multiple lines

      string_vector = strsplit( input_parts[1],
                                split = '\n', fixed = T )[[1]]

      nd[[ i ]] = pathdiagrams::add_lines_of_text(
        string_vector,
        x = xp, y = yp,
        spacing = text.spacing,
        spacing.fixed = T,
        cex = text.size,
        col = text.color,
        shape = shape,
        shape.col = shape.col,
        shape.border = shape.border,
        shape.lwd = shape.lwd,
        shape.lty = shape.lty,
        shape.x = shape.x,
        shape.y = shape.y,
        xpd = xpd,
        output = TRUE,
        add = add
      )
      # spacing
      # spacing.fixed

      # Close else for 'Check if single line'
    }

    # Debugging for node dimensions
    if ( FALSE ) {
      for ( k in 1:length( nd[[ i ]] ) ) {
        points( nd[[i]][[k]][1],
                nd[[i]][[k]][2],
                pch = 19, cex = .75, xpd = xpd )
      }
    }

    # Close 'Loop over inputs'
  }

  # If inputs for paths found
  if ( !is.null( paths ) ) {

    # Loop over inputs
    for ( i in 1:length( paths ) ) {

      # Extract details on current arrow
      path_parts = strsplit( paths[ i ], split = '|', fixed = T )[[1]]

      # Check for additional options
      path.pad = multiple_node_aes( 'lp=', path_parts, lod )
      path.lwd = multiple_node_aes( 'lw=', path_parts, lod )
      path.col = multiple_node_aes( 'lc=', path_parts, lod )
      path.length = multiple_node_aes( 'll=', path_parts, lod )
      path.angle = multiple_node_aes( 'la=', path_parts, lod )
      path.lty = multiple_node_aes( 'lt=', path_parts, lod )
      path.code = multiple_node_aes( 'cd=', path_parts, lod )

      # At a minimum
      # Node label - start | Node coordinate - start
      # ... Node label - end | Node coordinate - end

      # If a node name and coordinate are provided

      if ( path_parts[1] %in% names( input ) ) {
        start_pos = nd[[ path_parts[1] ]][[ path_parts[2] ]]
      }

      if ( path_parts[3] %in% names( input ) ) {
        end_pos = nd[[ path_parts[3] ]][[ path_parts[4] ]]
      }

      # If raw x and y-axis coordinates are provided

      if ( path_parts[1] == 'x,y' ) {
        start_pos = as.numeric(
          strsplit( path_parts[2], split = ',', fixed = T )[[1]]
        )
      }

      if ( path_parts[3] == 'x,y' ) {
        end_pos = as.numeric(
          strsplit( path_parts[4], split = ',', fixed = T )[[1]]
        )
      }

      # Pad start and end-points of line

      # Start point
      pp = 2

      if ( path_parts[pp] == 'bottom' ) {
        start_pos[2] = start_pos[2] - path.pad
      }
      if ( path_parts[pp] == 'top' ) {
        start_pos[2] = start_pos[2] + path.pad
      }
      if ( path_parts[pp] == 'right' ) {
        start_pos[1] = start_pos[1] + path.pad
      }
      if ( path_parts[pp] == 'left' ) {
        start_pos[1] = start_pos[1] - path.pad
      }
      if ( path_parts[pp] == 'bottomleft' ) {
        start_pos[1] = start_pos[1] - path.pad
        start_pos[2] = start_pos[2] - path.pad
      }
      if ( path_parts[pp] == 'topleft' ) {
        start_pos[1] = start_pos[1] - path.pad
        start_pos[2] = start_pos[2] + path.pad
      }
      if ( path_parts[pp] == 'topright' ) {
        start_pos[1] = start_pos[1] + path.pad
        start_pos[2] = start_pos[2] + path.pad
      }
      if ( path_parts[pp] == 'bottomright' ) {
        start_pos[1] = start_pos[1] + path.pad
        start_pos[2] = start_pos[2] - path.pad
      }

      # End point
      pp = 4

      if ( path_parts[pp] == 'bottom' ) {
        end_pos[2] = end_pos[2] - path.pad
      }
      if ( path_parts[pp] == 'top' ) {
        end_pos[2] = end_pos[2] + path.pad
      }
      if ( path_parts[pp] == 'right' ) {
        end_pos[1] = end_pos[1] + path.pad
      }
      if ( path_parts[pp] == 'left' ) {
        end_pos[1] = end_pos[1] - path.pad
      }
      if ( path_parts[pp] == 'bottomleft' ) {
        end_pos[1] = end_pos[1] - path.pad
        end_pos[2] = end_pos[2] - path.pad
      }
      if ( path_parts[pp] == 'topleft' ) {
        end_pos[1] = end_pos[1] - path.pad
        end_pos[2] = end_pos[2] + path.pad
      }
      if ( path_parts[pp] == 'topright' ) {
        end_pos[1] = end_pos[1] + path.pad
        end_pos[2] = end_pos[2] + path.pad
      }
      if ( path_parts[pp] == 'bottomright' ) {
        end_pos[1] = end_pos[1] + path.pad
        end_pos[2] = end_pos[2] - path.pad
      }

      # Determine type of line
      if ( path.code == '->' ) {
        arrow_type = 2
      }
      if ( path.code == '<-' ) {
        arrow_type = 1
      }
      if ( path.code == '<->' ) {
        arrow_type = 3
      }
      if ( path.code == '-' ) {
        arrow_type = 0
      }

      # Draw path
      if ( add ) {

        arrows(
          start_pos[1], start_pos[2],
          end_pos[1], end_pos[2],
          code = arrow_type,
          length = path.length,
          angle = path.angle,
          col = path.col,
          lty = path.lty,
          lwd = path.lwd,
          xpd = xpd
        )

        # Close 'Draw path'
      }

      # Close 'Loop over inputs'
    }

    # Close 'If inputs for paths found'
  }

  # If specified return output
  if ( output ) {
    return( nd )
  }
}

#### 5) size ####

#' Determine Width and Height of a Node
#'
#' Determine the size (width and height)
#' of a node in an existing path diagram.
#'
#' @param node A list with x and y
#'   coordinates for the bottom, left,
#'   top, right, and associated corners
#'   for a node (e.g., see output of
#'   \code{\link{add_nodes}} or
#'   \code{\link{add_lines_of_text}}).
#'
#' @examples
#' # Base figure
#' create_base_figure()
#'
#' # Add nodes
#' nodes <- add_nodes(
#'   c( N1 = 'Node-1|x=.25|y=.5',
#'      N2 = 'Node-2\n  Indent|x=.75|y=.5'
#'   ),
#'   output = TRUE
#' )
#'
#' # Dimension of nodes in diagram
#' size( nodes$N1 )
#' size( nodes$N2 )
#'
#' @export

size = function( node ) {

  w = node$right[1] - node$left[1]
  h = node$top[2] - node$bottom[2]

  return( c( width = w, height = h ) )
}

#### 6) sep ####
#' Determine Separation Between Nodes
#'
#' Determine the distance between x-axis
#' or y-axis coordinates for two nodes in
#' a path diagram.
#'
#' @param nodes A list of lists, each list
#'   giving the x and y coordinates for the
#'   bottom, left, top, right and associated
#'   corners for a given node (e.g., see output
#'   of \code{\link{add_nodes}} or
#'   \code{\link{add_lines_of_text}}).
#' @param labels Either...
#'   \itemize{
#'     \item A character vector of length 2, giving
#'       the names of the lists for the two nodes to
#'       compare;
#'     \item A integer vector of length 2, giving
#'       the positions in the list of lists for
#'       the two nodes to compare.
#'     }
#' @param dimension The set of coordinates to
#'   compare between nodes, either \code{'x-axis'}
#'   or \code{'y-axis'}.
#' @param desired An optional value specifying
#'   the desired distance between the two
#'   nodes; when provided, the function outputs
#'   the new x or y-axis coordinate needed
#'   for the second node to have the desired
#'   distance from the first node.
#'
#' @examples
#' # Create simple path diagram
#' create_base_figure()
#' # Add nodes
#' nodes <- add_nodes(
#'   c( N1 = 'Node-01|x=.3|y=.7',
#'      N2 = 'Node-02|x=.7|y=.3' ),
#'   output = TRUE )
#'
#' # Distance from bottom of node 'N1'
#' # and top of node 'N2'
#' sep( nodes, c( 'N1', 'N2' ) )
#'
#' # Determine new y-axis coordinate for 'N2' so
#' # that distance between nodes would be 0.1
#' sep( nodes, c( 'N1', 'N2' ), desired = .1 )[2]
#' # Add new node using proposed y-axis coordinate
#' add_nodes( c( N3 = 'Node-03|x=.3|y=.534' ) )
#'
#' # Determine new y-axis coordinate for 'N1' so
#' # that distance between nodes would be 0.1
#' sep( nodes, c( 'N2', 'N1' ), desired = .1 )[2]
#' # Add new node using proposed y-axis coordinate
#' add_nodes( c( N4 = 'Node-04|x=.7|y=.466' ) )
#'
#' # Distance from left edge of 'N1' and
#' # right edge of 'N2'
#' sep( nodes, c( 'N1', 'N2' ), dimension = 'x-axis' )
#'
#' # Determine new x-axis coordinate for 'N2' so
#' # that distance between nodes would be 0.1
#' sep( nodes, c( 'N1', 'N2' ), 'x-axis', desired = .1 )[2]
#' # Add new node using proposed x-axis coordinate
#' add_nodes( c( N5 = 'Node-05|x=.589|y=.7' ) )
#'
#' @export

sep <- function( nodes, labels,
                 dimension = 'y-axis',
                 desired = NULL ) {

  # Initialize output
  out <- NULL

  # Extract list of coordinates for nodes of interest
  n1 <- nodes[[ labels[1] ]]
  n2 <- nodes[[ labels[2] ]]

  #< Comparison between y-axis coordinates
  if ( dimension %in% c( 'Height', 'height', 'H', 'h', '1',
                         'y-axis', 'y' ) ) {

    # Determine which node is higher
    highest <- which.max( c( n1$top[2], n2$top[2] ) )

    pst <- c( 'bottom', 'top' )

    if ( highest == 2 ) {
      pst <- rev( pst )
    }

    # Compute distance
    out <- n1[[ pst[1] ]][2] - n2[[ pst[2] ]][2]

    #<< If new y-axis coordinate requested
    if ( !is.null( desired ) ) {

      #<<< If 2nd node is higher
      if ( pst[1] == 'top' ) {

        new_pos <- n1[[ pst[1] ]][2] + desired
        adj <- n2[[ pst[2] ]][2] - new_pos

        out <- c( out, new_y = n2$left[2] - adj )

        #>>> Close conditional on higher 2nd node
      } else {

        new_pos <- n1[[ pst[1] ]][2] - desired
        adj <- new_pos - n2[[ pst[2] ]][2]

        out <- c( out, new_y = n2$left[2] + adj )

        #>>> Close conditional on higher 1st node
      }

      #>> Close conditional over new y-axis coordinate
    }

    #> Close conditional over y-axis coordinates
  }

  #< Comparison between x-axis coordinates
  if ( dimension %in% c( 'Width', 'width', 'W', 'w', '2',
                         'x-axis', 'x' ) ) {

    # Determine which node is closer to left side
    closest = which.min( c( n1$left[1], n2$left[1] ) )

    pst <- c( 'right', 'left' )

    if ( closest == 2 ) {
      pst <- rev( pst )
    }

    # Compute distance
    out <- n2[[ pst[2] ]][1] - n1[[ pst[1] ]][1]

    #<< If new x-axis coordinate requested
    if ( !is.null( desired ) ) {

      #<<< If 2nd node is closer
      if ( pst[1] == 'left' ) {

        new_pos <- n1[[ pst[1] ]][1] - desired
        adj <- new_pos - n2[[ pst[2] ]][1]

        out <- c( out, new_y = n2$top[1] + adj )

        #>>> Close conditional on closer 2nd node
      } else {

        new_pos <- n1[[ pst[1] ]][1] + desired
        adj <- n2[[ pst[2] ]][1] - new_pos

        out <- c( out, new_y = n2$top[1] - adj )

        #>>> Close conditional on closer 1st node
      }

      #>> Close conditional over new x-axis coordinate
    }

    #> Close conditional over x-axis coordinates
  }

  return( out )
}

#### 7) replace_with_values ####
#' Replace Placeholders with Values
#'
#' Replaces placeholder text in a
#' character vector with user specified
#' values.
#'
#' @param x A character vector
#' @param values A vector of values with
#'   which to replace placeholder text.
#' @param placeholder A function defining
#'   how to identify placeholder text based
#'   on the index position of the vector
#'   \code{values}.
#' @param ... Additional parameters for the
#'   \code{placeholder} function.
#'
#' @examples
#' # Character vector with placeholder text
#' # '[[i]]' for the ith element to replace
#' x <- c( 'Value: [[1]]', 'Value: [[2]]' )
#' replace_with_values( x, c( 1, 2 ) )
#'
#' # Custom function to replace different
#' # placeholder text
#' placeholder <- function( i ) paste0( '***', LETTERS[i] )
#' x <- c( 'Value: ***A', 'Value: ***B' )
#' replace_with_values( x, c( 1, 2 ), placeholder )
#'
#' @export

replace_with_values <- function( x, values,
                                 placeholder = function(i) {
                                   paste0( '[[', i, ']]' )
                                 },
                                 ... ) {

  out <- x
  n <- length( values )

  #< Loop over values
  for ( i in 1:n ) {

    # Replace '[[i]]' with ith value
    out <- gsub(
      placeholder( i, ... ),
      values[i],
      out,
      fixed = T
    )

    #> Close loop over values
  }

  return( out )
}

#### 8) distribute ####
#' Evenly Distribute Nodes
#'
#' Function that determines new y or x-axis
#' values for the centers of a set of nodes
#' in order to evenly distribute nodes
#' vertically or horizontally.
#'
#' @param nodes A list of lists, each list
#'   giving the x and y coordinates for the
#'   bottom, left, top, right and associated
#'   corners for a given node (e.g., see output
#'   of \code{\link{add_nodes}} or
#'   \code{\link{add_lines_of_text}}).
#' @param vertical Logical; if \code{TRUE} adjusts
#'   y-axis positions to have even spaces; otherwise
#'   adjusts x-axis positions to have even spaces.
#' @param flush Logical; if \code{TRUE} the spacing for
#'   nodes is calculated such that the first and last
#'   nodes are flush with the margins.
#' @param space An optional value for the space between
#'   nodes; if \code{NULL} automatically calculated.
#' @param digits Number of digits to round output vector.
#'
#' @return A vector of values, either the new x or
#' y-axis values for the centers of the nodes.
#'
#' @examples
#' # Example for vertical spacing
#' create_base_figure( new = FALSE )
#'
#' # Uneven spacing for y-axis positions
#' x_y <- c(
#'   # x-axis
#'   c( .3, .3, .3 ),
#'   # y-axis
#'   c( .9, .65, .2 )
#' )
#'
#' # Input with placeholders for x and y-axis values
#' draft_inputs <- c(
#'   N01 = 'Line 1\nLine 2|x=[[1]]|y=[[4]]',
#'   N02 = 'Line 1|x=[[2]]|y=[[5]]',
#'   N03 = 'Line 1\nLine 2\nLine 3|x=[[3]]|y=[[6]]'
#' )
#' # Update with values from 'x_y'
#' inputs <- replace_with_values( draft_inputs, x_y )
#'
#' # Add nodes to figure
#' nodes <- add_nodes( inputs, output = TRUE )
#'
#' # Determine even y-axis positions
#' new_y <- distribute( nodes )
#' # Update 'inputs' with new values
#' x_y[1:3] <- .7; x_y[4:6] <- new_y
#' inputs <- replace_with_values( draft_inputs, x_y )
#'
#' # Add nodes to figure
#' nodes <- add_nodes( inputs, output = TRUE )
#'
#' @export

distribute <- function( nodes,
                        vertical = TRUE, flush = FALSE,
                        space = NULL, digits = 4 ) {

  N <- length( nodes )

  if ( N == 1 ) {
    stop( 'Must have more than one node' )
  }

  # Vertical spacing
  if ( vertical ) {

    top <- sapply( 1:N, function(n) nodes[[n]]$top[2] )
    center <- sapply( 1:N, function(n) nodes[[n]]$left[2] )
    bottom <- sapply( 1:N, function(n) nodes[[n]]$bottom[2] )

    H <- top - bottom

    if ( is.null( space ) ) {
      space <- 1 - sum( H )

      dnm <- N+1
      if ( flush ) dnm <- N-1

      space <- space / dnm
    }

    new_center <- rep( NA, N )

    if ( flush ) {
      start_position <- 1
    } else {
      start_position <- 1 - space
    }
    for ( n in 1:N ) {

      new_center[n] <- start_position - H[n]/2

      start_position <-
        start_position - H[n] - space

    }

    if ( !is.null( digits ) ) {
      new_center <- round( new_center, digits )
    }

    return( new_center )

    # Close 'Vertical spacing'
  } else {

    left <- sapply( 1:N, function(n) nodes[[n]]$left[1] )
    center <- sapply( 1:N, function(n) nodes[[n]]$top[1] )
    right <- sapply( 1:N, function(n) nodes[[n]]$right[1] )

    W <- right - left

    if ( is.null( space ) ) {
      space <- 1 - sum( W )

      dnm <- N+1
      if ( flush ) dnm <- N-1

      space <- space / dnm
    }

    new_center <- rep( NA, N )

    if ( flush ) {
      start_position <- 0
    } else {
      start_position <- space
    }
    for ( n in 1:N ) {

      new_center[n] <- start_position + W[n]/2

      start_position <-
        start_position + W[n] + space

    }

    if ( !is.null( digits ) ) {
      new_center <- round( new_center, digits )
    }

    return( new_center )

    # Close else for 'Vertical spacing'
  }

}

#### 9) multiple_node_aes ####
# Specify Node Aesthetics
#
# Internal function to match the contents
# of a string input to a given tag and
# extract the value used when specifying
# aesthetic options.
#
# param tag: A character string.
# param input_parts: A vector of character
#   strings after splitting the user-supplied
#   input for a node or path by the "|" symbol
# param lod: A list of defaults for options.
#
# return: The value extracted from the
#   relevant input part.

multiple_node_aes = function( tag,
                              input_parts,
                              lod ) {

  # Default output

  # Node shape
  if ( tag == 'ns=' ) out = lod$shape
  if ( tag == 'nc=' ) out = lod$shape.col
  if ( tag == 'nb=' ) out = lod$shape.border
  if ( tag == 'nw=' ) out = lod$shape.lwd
  if ( tag == 'nt=' ) out = lod$shape.lty
  if ( tag == 'np=' ) out = lod$shape.pad
  if ( tag == 'nx=' ) out = lod$shape.x
  if ( tag == 'ny=' ) out = lod$shape.y

  # Node text
  if ( tag == 'ts=' ) out = lod$text.size
  if ( tag == 'tc=' ) out = lod$text.col
  if ( tag == 'tf=' ) out = lod$text.font
  if ( tag == 'th=' ) out = lod$text.spacing

  # Path line
  if ( tag == 'lp=' ) out = lod$path.pad
  if ( tag == 'lw=' ) out = lod$path.lwd
  if ( tag == 'lc=' ) out = lod$path.col
  if ( tag == 'll=' ) out = lod$path.length
  if ( tag == 'la=' ) out = lod$path.angle
  if ( tag == 'lt=' ) out = lod$path.lty
  if ( tag == 'cd=' ) out = lod$path.code

  check = grepl( tag, input_parts, fixed = T )

  #< Check for any inputted tags
  if ( any( check ) ) {

    #<< Character output tags
    if ( tag %in% c( 'ns=', 'nc=', 'nb=', 'tc=', 'tf=',
                     'cd=', 'lc=' ) ) {

      val = input_parts[ check ]

      val = gsub( tag, '', val, fixed = T )

      if ( val == 'NA' ) val = NA

      out = val

      #>> Close conditional for node shape
    }

    #<< Numeric output tags
    if ( tag %in% c( 'nw=', 'nt=', 'ts=', 'lp=', 'lw=',
                     'll=', 'la=', 'lt=', 'th=', 'nx=',
                     'ny=', 'np=' ) ) {

      val = input_parts[ check ]

      val = gsub( tag, '', val, fixed = T )

      if ( val == 'NA' ) val = NA

      out = as.numeric( val )

      #>> Close conditional for node shape
    }

    #> Close conditional for any inputted tags
  }

  return( out )
}

#### 10) plain_bold_italic_text ####
# Add Bold or Italic Text to a Figure
#
# Internal function that adds bold or
# or italic text to an existing figure.
#
# param x: The x-axis coordinate at which
#   to draw the text.
# param y: The y-axis coordinate at which
#   to draw the text.
# param txt: The text content. Characters
#   embedded between '**' are bolded,
#   while those embedded between '*'
#   are italicized.
# param cex: The text size.
# param pos: The alignment of the text
#   (NULL is centered, 2 is left-aligned,
#   4 is right-aligned)
# param ...: Additional parameters for the
#   'text' function.

plain_bold_italic_text <- function( x, y, txt, cex = 1,
                                    pos = NULL, offset = 0,
                                    ... ) {

  # fin_txt <- gsub( '*', '', txt, fixed = T )
  # strwidth( fin_txt, cex = cex )

  each_elem <- strsplit( txt, split = '*', fixed = T )[[1]]
  each_elem[ each_elem != "" ]

  each_elem = each_elem[ each_elem != "" ]

  elem_type = rep( 1, length( each_elem ) )
  for ( i in 1:length( elem_type ) ) {


    if ( grepl( paste0( '*', each_elem[i] ), txt, fixed = T ) &
         !grepl( '* ', paste0( '*', each_elem[i] ), fixed = T ) ) {
      elem_type[i] = 3
    }

    if ( grepl( paste0( '**', each_elem[i] ), txt, fixed = T ) &
         !grepl( '** ', paste0( '**', each_elem[i] ), fixed = T ) ) {
      elem_type[i] = 2
    }

  }

  elem_sz = strwidth( each_elem, cex = cex )

  # half_elem_sz = elem_sz/2

  # pst <- rep( NA, length( elem_sz ) )
  pst = cumsum( elem_sz )
  if ( is.null( pos ) ) {
    x_pst = pst + x - sum( elem_sz )/2# - elem_sz[1]/2
  } else {
    if ( pos == 2 ) {
      x_pst = pst + x - sum( elem_sz ) - elem_sz[1]
    }
    if ( pos == 4 ) {
      x_pst = pst + x
    }
  }

  txt_h = strheight(each_elem,
                    cex = cex,
                    font = elem_type )
  y_pst = y - max( txt_h )/2

  for ( i in 1:length( each_elem ) ) {

    # Add text
    text( x_pst[i], y_pst + txt_h[i]/2,
          each_elem[i], cex = cex, pos = 2,
          font = elem_type[i], offset = offset, ... )

  }

}


