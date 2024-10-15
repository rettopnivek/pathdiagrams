# Functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2024-10-15

# Table of contents
# 1) Internal functions
#   1.1) pd_int_add.node_shape
#   1.2) pd_int_add.node_text
#   1.3) pd_int_add.args
# 2) Path diagram functions
#   2.1) pd_base_figure
#   2.2) pd_node
#   2.3) pd_node_dimensions
#   2.4) pd_draw_nodes
#     2.4.1) Update xy
#     2.4.2) Add paths
#     2.4.3) Add nodes
# 3) Helper functions
#   3.1) xy
#   3.2) wh
#   3.3) pd_node_template

#### 1) Internal functions ####

#### 1.1) pd_int_add.node_shape ####
# Add Node Shape
#
# Draws the node shape on an existing figure.
#
# @param xy A named vector where xy['x']
#   indicates the x-axis position and
#   xy['y'] indicates the y-axis position
#   for the center of the node.
# @param wh A named vector where wh['w']
#   indicates the width and wh['h']
#   indicates the height of the node.
# @param shape A character string,
#   the shape of the node which can be
#   either 'rectangle' or 'ellipse'.
# @param args.polygon An optional named
#   list of additional arguments to pass
#   to the [graphics::polygon] function.

pd_int_add.node_shape <- function(
    xy,
    wh,
    shape = 'rectangle',
    args.polygon = NULL ) {

  lst_shapes <- list(
    rectangle = c(
      'Rectangle',
      'rectangle',
      'Rect',
      'rect',
      'Box',
      'box',
      'Square',
      'square'
    ),
    ellipse = c(
      'Ellipse',
      'ellipse',
      'Circle',
      'circle',
      'Circ',
      'circ'
    )
  )

  lst_polygon <- args.polygon

  # Ensure list of inputs for polygon()
  if ( is.null( lst_polygon ) ) {

    lst_polygon <- list()

    # Close 'Ensure list of inputs for polygon()'
  }

  # Check is list
  if ( !is.list(lst_polygon) ) {

    chr_error <- paste0(
      "Argument 'polygon' must be NULL or a named list ",
      "with arguments for the polyon() function"
    )

    stop( chr_error )

    # Close 'Check is list'
  }

  # Draw a rectangle
  if ( shape %in% lst_shapes$rectangle ) {

    lst_polygon$x <-
      xy['x'] + wh['w']*c( -.5, -.5, .5, .5 )
    lst_polygon$y <-
      xy['y'] + wh['h']*c( -.5, .5, .5, -.5 )

    # Close 'Draw a rectangle'
  }

  # Draw an ellipse
  if ( shape %in% lst_shapes$ellipse ) {

    # Distance of center to foci
    num_ctf <- wh['w']/2

    # Semi-latus rectum
    num_slr <- wh['h']/2

    # Semi-major axis
    num_smja <- sqrt( ( num_ctf )^2 + ( num_slr )^2 )

    # Semi-minor axis
    num_smna <- sqrt( num_smja^2 - num_ctf^2 )

    # x and y coordinates for ellipse
    num_pts <- seq( 0, 2 * pi, length.out = 100 )
    lst_polygon$x <- num_smja * cos( num_pts ) + xy['x']
    lst_polygon$y <- num_smna * sin( num_pts ) + xy['y']

    # Close 'Draw an ellipse'
  }

  # By default allow plotting outside boundaries
  if ( is.null(lst_polygon$xpd) )
    lst_polygon$xpd <- NA

  do.call(
    polygon, lst_polygon
  )

}

#### 1.2) pd_int_add.node_text ####
# Add Node Text
#
# Adds text for a node to an existing figure.
#
# @param xy A named vector where xy['x']
#   indicates the x-axis position and
#   xy['y'] indicates the y-axis position
#   for the center of the node.
# @param wh A named vector where wh['w']
#   indicates the width and wh['h']
#   indicates the height of the node. If
#   NULL is determined from the text content.
#   Can specify one dimension and set the
#   other to NA.
# @param cex A numeric value, the text size
#   (see [graphics::par]).
# @param spacing A numeric value, the spacing
#   between lines of text and node boundaries.
#   Values are relative to the height of the
#   text (e.g., a value of .5 indicates a gap
#   equal to half of the text height).
# @param align A character string, the alignment
#   of the text, either 'center', 'left', or 'right'.
# @param col A character string, the color of
#   the text.
# @param ignore_asterisk A logical value, if TRUE
#   interprets asterisks as denoting italic or
#   bold text.
# @param add A logical value, if TRUE adds text
#   to an existing figure.

pd_int_add.node_text <- function(
    string,
    xy,
    wh,
    cex = 1,
    spacing = .5,
    align = 'center',
    col = 'black',
    ignore_asterisk = TRUE,
    add = TRUE ) {

  # Width/height not specified
  if ( is.null(wh) ) {

    wh <- c( w = NA, h = NA )

    # Close 'Width/height not specified'
  }

  # Default values given null inputs
  if ( is.null(cex) ) cex <- 1
  if ( is.null(spacing) ) spacing <- 0.5
  if ( is.null(align) ) align <- 'center'
  if ( is.null(col) ) col = 'black'
  if ( is.null(ignore_asterisk) ) ignore_asterisk <- TRUE

  lst_lines <- lapply(
    strsplit( string, split = '\n', fixed = TRUE )[[1]],
    function(s) {

      chr_line <- strsplit( s, split = '', fixed = TRUE )[[1]]

      int_font <- rep( 1, length(chr_line) )

      # If bold/italic via asterisk
      if ( ignore_asterisk ) {

        # If even number for asterisk
        if ( any( chr_line == '*' ) &
             ( sum( chr_line == '*' ) %% 2 ) == 0 ) {

          # Locate intervals
          int_ast <- which( chr_line == '*' )
          int_ast <- int_ast[ c( 2, diff( int_ast ) ) > 1 ]

          # Matrix with start and end
          mat_font <- matrix(
            int_ast,
            length(int_ast)/2, 2,
            byrow = TRUE
          )

          # Loop over rows
          for ( r in 1:nrow(mat_font) ) {

            # Update as italic
            int_font[ mat_font[r, 1]:mat_font[r, 2] ] <- 3

            # Close 'Loop over rows'
          }

          # Check for bold
          int_ast <- which( chr_line == '*' )
          int_ast <- int_ast[ c( 2, diff( int_ast ) ) == 1 ]

          # If any to bold
          if ( length(int_ast) > 0 ) {

            mat_font <- matrix(
              int_ast,
              length(int_ast)/2, 2,
              byrow = TRUE
            )

            # Loop over rows
            for ( r in 1:nrow(mat_font) ) {

              # Update as bold
              int_font[ mat_font[r, 1]:mat_font[r, 2] ] <- 2

              # Close 'Loop over rows'
            }

            # Close 'If any to bold'
          }

          int_font <- int_font[ chr_line != '*' ]
          chr_line <- chr_line[ chr_line != '*' ]

          # Close 'If even number for asterisk'
        }

        # Close 'If bold/italic via asterisk'
      }

      num_x <- sapply(
        seq_along(chr_line), function(i) {
          return( strwidth(chr_line[i], cex = cex, font = int_font[i]) )
        }
      )

      num_w <- sum( num_x )

      # Specify x-axis position for each word
      if ( length(num_x) > 1 ) {

        num_x <- c(
          num_x[1]/2,
          cumsum(num_x)[-length(num_x)] + num_x[-1]/2
        )

        # Close 'Specify x-axis position for each word'
      } else {

        num_x <- num_x[1]/2

        # Close else for 'Specify x-axis position for each word'
      }

      return(

        list(
          line = chr_line,
          font = int_font,
          x = num_x,
          w = num_w
        )

      )

    }
  )

  int_lines <- length(lst_lines)
  num_H <- max( strheight( lst_lines[[1]]$line, cex = cex) )

  num_WT <- sapply( seq_along(lst_lines), function(l) {
    lst_lines[[l]]$w + num_H*spacing*2
  } ) |> max()

  num_HT <-
    num_H*int_lines +
    num_H*spacing*(int_lines + 1)

  num_HL <- rep( -num_H*spacing, int_lines )

  # if more than one line
  if ( int_lines > 1 ) {

    # Loop over lines
    for (l in 2:int_lines) {

      num_HL[l] <-
        num_HL[l-1] - num_H - num_H*spacing

      # Close 'Loop over lines'
    }

    # Close 'if more than one line'
  }

  # If width not specified
  if ( is.na( wh['w'] ) ) {

    wh['w'] <- num_WT

    # Close 'If width not specified'
  }

  # If height not specified
  if ( is.na( wh['h'] ) ) {

    wh['h'] <- num_HT

    # Close 'If height not specified'
  }

  lst_nuisance <- lapply(
    seq_along(lst_lines), function(l) {

      # Center alignment
      if ( align %in% c( 'center', 'centre' ) ) {

        num_adj <- -lst_lines[[l]]$w/2

        # Close 'Center alignment'
      }

      # Left alignment
      if ( align %in% c( 'left' ) ) {

        num_adj <- -num_WT/2 + num_H*spacing

        # Close 'Center alignment'
      }

      # Right alignment
      if ( align %in% c( 'right' ) ) {

        num_adj <- num_WT/2 - num_H*spacing - lst_lines[[l]]$w

        # Close 'Center alignment'
      }

      # Add text to figure
      if ( add ) {

        # print(cex)
        text(
          xy['x'] + num_adj + lst_lines[[l]]$x,
          xy['y'] + num_HL[l] + num_HT/2,
          lst_lines[[l]]$line,
          cex = cex,
          font = lst_lines[[l]]$font,
          pos = 1,
          col = col,
          xpd = NA,
          offset = 0
        )

        # Close 'Add text to figure'
      }

    }
  )

  return( wh )
}

#### 1.3) pd_int_add.args ####
# Add Arguments to a List
#
# Function to add new elements to a list without
# overwriting existing elements.
#
# @param x A list.
# @param y A named list.
#
# @returns A list.

pd_int_add.args <- function(
    x,
    y ) {

  # Loop over elements in y
  for ( e in seq_along(y) ) {

    # If element does not exist in x
    if ( is.null( x[[ names(y)[e] ]] ) ) {

      # Add to x
      x[[ names(y)[e] ]] <- y[[e]]

      # Close 'If element does not exist in x'
    }

    # Close 'Loop over elements in y'
  }

  return( x )
}

#### 2) Path diagram functions ####

#### 2.1) pd_base_figure ####
#' Create Base Figure
#'
#' A function that generates a base figure,
#' with optional guidelines, for a diagram.
#'
#' @param default A character string used to specify
#'   pre-packaged figure sizes, including...
#'   \itemize{
#'     \item 'US letter' or '8.5 x 11' (inches);
#'     \item '3.54 x 3.54' (inches);
#'     \item '5 x 5' (inches);
#'     \item '7.25 x 5' (inches).
#'   }
#' @param width A numeric value, the width of the
#'   figure in inches.
#' @param height A numeric value, the height of the
#'   figure in inches.
#' @param orientation A character string, indicates
#'   whether figures should be in \code{'landscape'} or
#'   \code{'portrait'} style.
#' @param margin A numeric vector of 4 values, specifying
#'   in inches the margins for the bottom, left, top, and
#'   right, respectively.
#' @param guidelines A logical value, if \code{TRUE} includes
#'   guidelines when generating the figure.
#' @param guide_major A numeric vector giving the major guideline
#'   positions (values must be between 0 and 1).
#' @param guide_minor A numeric vector giving the minor guideline
#'   positions (values must be between 0 and 1).
#'   If \code{NULL}, no minor guidelines are included.
#' @param guide_adjust A numeric value, the adjustment controlling the
#'   position on the axes for the guideline numbers.
#' @param new A logical value, if \code{TRUE} a new plotting
#'   window is generated.
#'
#' @examples
#' \dontrun{
#' # Default (6 x 6 inches)
#' pd_base_figure()
#'
#' # No guidelines
#' pd_base_figure( guidelines = FALSE )
#'
#' # US letter size
#' pd_base_figure( default = 'US letter' )
#'
#' # US letter (Portrait)
#' pd_base_figure( default = 'US letter', orientation = 'portrait' )
#' }
#'
#' @export

pd_base_figure = function(
    default = NULL,
    width = 6,
    height = 6,
    orientation = 'landscape',
    margin = rep( .1, 4 ),
    guidelines = TRUE,
    guide_major = seq( .1, .9, .1 ),
    guide_minor = seq( .05, .95, .1 ),
    guide_adjust = 0.8,
    new = FALSE ) {

  # If a default argument is provided
  if ( !is.null( default ) ) {

    # US letter
    if ( default %in% c( 'US letter',
                         '8.5 x 11 in', '8.5 x 11' ) ) {

      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        width = 11
        height = 8.5
      }
      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        width = 8.5
        height = 11
      }

      # Close 'US letter'
    }

    # Science guidelines for figure sizes (small)
    if ( default %in% c( '3.54 x 3.54 in', '3.54 x 3.54', '3.54',
                         '9 x 9 cm', '9' ) ) {

      width = 3.54
      height = 3.54

      # Close 'Science guidelines for figure sizes (small)'
    }

    # Science guidelines for figure sizes (medium)
    if ( default %in% c( '5 x 5 in', '5 x 5', '5',
                         '12.7 x 12.7 cm', '12.7' ) ) {

      width = 5
      height = 5

      # Close 'Science guidelines for figure sizes (medium)'
    }

    # Science guidelines for figure sizes (wide)
    if ( default %in% c( '7.25 x 5 in', '7.25 x 5', '7.25',
                         '18.4 x 12.7 cm', '18.4 x 12.7', '18.4' ) ) {

      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        width = 7.25
        height = 5
      }

      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        width = 5
        height = 7.25
      }

      # Close 'Science guidelines for figure sizes (wide)'
    }

    # Close 'If a default argument is provided'
  }

  # Create plotting window
  if ( new ) x11( width = width, height = height )

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

#### 2.2) pd_node ####
#' Add Node to List
#'
#' Adds specifications to draw a new node to
#' an existing list that can then be passed to
#' the [pathdiagrams::pd_draw_nodes] function.
#'
#' @param lst A list.
#' @param node A character string, the name of the
#'   node (note that the period symbol should not be used
#'   when naming nodes).
#' @param xy A named numeric vector where \code{xy['x']}
#'   specifies the x-axis value and \code{xy['y']} specifies
#'   the y-axis value for the center of the node. Values
#'   should range from 0 to 1 (the fraction of the plotting
#'   window). If left as \code{NULL}, users can use the
#'   argument \code{from} to specify the node position instead.
#' @param wh A named numeric vector where \code{wh['w']}
#'   specifies the width and \code{wh['h']} specifies
#'   the height of the node. Values should range from 0
#'   to 1 (the fraction of the plotting window). Setting
#'   values to \code{NA} will result in the function
#'   inferring the relevant dimension size from the
#'   node's text content.
#' @param string A character string, the text content
#'   for the node. Text can be bracketed by one or
#'   two asterisks to produce italics or bold text,
#'   respectively. Set \code{ignore_asterisk} to
#'   \code{FALSE} to suppress this behavior.
#' @param shape A character string, either \code{'rectangle'}
#'   or \code{'ellipse'}, indicating the shape of the node
#'   to draw.
#' @param from An optional named vector of two values, giving
#'   the offset from a specified node for the x and y-axis.
#'   Units are relative. Height is relative to a node using
#'   one line of text. Width is relative to the referent
#'   node (e.g., a value of -2 for the y-axis will place the
#'   new node to the left of the referent node based on
#'   twice its width). Names for the vector are the referent
#'   node, followed by a period, followed by the location
#'   on the referent node to use as reference (e.g.,
#'   'N01.bottom'). If no referent node is specified
#'   uses absolute x and y-axis values instead.
#' @param to A character vector, where each string
#'   indicates the arrows from the current node
#'   and where they should connect to. Format is
#'   \code{'<location> - <node>.<location>'} where
#'   location is either top, topright, right, bottomright,
#'   bottom, bottomleft, left, or topleft, and node is the
#'   node name to connect to. Users can indicate the
#'   type of arrow to draw using \code{'-'} (line),
#'   \code{'->'} (arrow to new node), \code{'<-'}
#'   (arrow to old node), \code{'<->'} (arrows on
#'   both ends).
#' @param args.polygon A named list with additional
#'   arguments to pass to the [graphics::polygon]
#'   function.
#' @param args.text A named list with additional
#'   arguments governing text (\code{cex} for text
#'   size, \code{col} for color, and \code{spacing}
#'   for gap between text).
#' @param args.arrow A named list with additional
#'   arguments to pass to the [graphics::arrows]
#'   function.
#' @param ignore_asterisk A logical value, if
#'   \code{FALSE} suppresses behavior determining
#'   italic/bold text via asterisks.
#'
#' @returns A list with a new element named \code{node}.
#'
#' @examples
#' pd_base_figure( default = '5 x 5' )
#'
#' lst_inputs <- list() |>
#'   pd_node( 'N01', c(x = .5, y = .8), string = 'Node-1' ) |>
#'   pd_node( 'N02', string = 'Node-2',
#'            from = c( N01.center = 0, N01.bottom = -1.5 ),
#'            to = 'top - N01.bottom' )
#' pd_draw_nodes( lst_inputs )
#'
#' @export

pd_node <- function(
    lst,
    node,
    xy = NULL,
    wh = NULL,
    string = '',
    shape = 'rectangle',
    from = NULL,
    to = NULL,
    args.polygon = NULL,
    args.text = NULL,
    args.arrows = NULL,
    ignore_asterisk = TRUE ) {

  # Initialize xy
  if ( is.null(xy) ) {

    if ( is.null(from) ) stop( "Must specify either 'xy' or 'from'" )

    xy <- c( x = NA, y = NA )

    # Close 'Initialize xy'
  }

  # Default arguments for text
  if ( is.null(args.text) ) {

    args.text <- list()

    # Close 'Default arguments for text'
  }

  # Initialize wh
  if ( is.null(wh) ) {

    wh <- c( w = NA, h = NA )

    # Close 'Initialize wh'
  }

  lst[[ node ]] <- list(
    xy = xy,
    wh = wh,
    string = string,
    shape = shape,
    from = from,
    to = to,
    args.polygon = args.polygon,
    args.text = args.text,
    args.arrows = args.arrows
  )

  return( lst )
}

#### 2.3) pd_node_dimensions ####
#' Node Dimensions
#'
#' Function to compute the dimensions
#' of a node, providing values for the
#' top, topright, right, bottomright,
#' bottom, bottomleft, left, and topleft
#' x and y-axis coordinates.
#'
#' @param xy A named vector with the
#'   x and y-axis coordinates for the
#'   center of the node.
#' @param wh A named vector with the
#'   width and height of the node.
#'
#' @returns A list with the assorted coordinates
#' for the node.
#'
#' @examples
#' pd_node_dimensions( c(x = .5, y = .5), wh = c( w = .1, h = .1 ) )
#'
#' @export

pd_node_dimensions <- function(
    xy, wh ) {

  x = xy['x']
  y = xy['y']
  w = wh['w']/2
  h = wh['h']/2

  lst_coordinates <- list(
    topleft  = c( x - w, y + h ),
    top = c( x, y + h ),
    topright = c( x + w, y + h ),
    right = c( x + w, y ),
    bottomright = c( x + w, y - h ),
    bottom = c( x, y - h ),
    bottomleft = c( x - w, y - h ),
    left = c( x - w, y ),
    center = c( x, y ),
    width = w*2,
    height = h*2
  )

  return( lst_coordinates )
}

#### 2.4) pd_draw_nodes ####
#' Draw Nodes on a Diagram
#'
#' Function that takes a list with node details
#' and draws them on an existing figure (see
#' [pathdiagrams::pd_base_figure]). Details
#' for nodes can be added to a list via
#' [pathdiagrams::pd_node].
#'
#' @param inputs A named list, where each element
#'   corresponds to a node to add to the figure.
#' @param args.polygon A named list, additional
#'   arguments to pass to the [graphics::polygon]
#'   function to apply to all nodes without
#'   pre-existing options already specified.
#'
#' @examples
#' pd_base_figure( default = '5 x 5' )
#'
#' lst_inputs <- list() |>
#'   pd_node( 'N01', c(x = .5, y = .8), string = 'Node-1' ) |>
#'   pd_node( 'N02', string = 'Node-2',
#'            from = c( N01.center = 0, N01.bottom = -1.5 ),
#'            to = 'top - N01.bottom' )
#' pd_draw_nodes( lst_inputs )
#'
#' @export

pd_draw_nodes <- function(
    inputs,
    args.polygon = NULL,
    args.text = NULL,
    args.arrows = NULL ) {

  chr_nodes <- names(inputs)

  #### 2.4.1) Update wh ####

  # Loop over nodes
  for ( n in seq_along(inputs) ) {

    # Update arguments for text
    if ( !is.null( args.text ) ) {

      inputs[[n]]$args.text <- pd_int_add.args(
        inputs[[n]]$args.text,
        args.text
      )

      # Close 'Update arguments for text'
    }

    # Width or height not specified
    if ( any( is.na( inputs[[n]]$wh ) ) ) {

      inputs[[n]]$wh <- pd_int_add.node_text(
        string = inputs[[n]]$string,
        xy = inputs[[n]]$xy,
        wh = inputs[[n]]$wh,
        cex = inputs[[n]]$args.text$cex,
        col = inputs[[n]]$args.text$col,
        align = inputs[[n]]$args.text$align,
        spacing = inputs[[n]]$args.text$spacing,
        add = F
      )

      # Close 'Width or height not specified'
    }

    # Close 'Loop over nodes'
  }

  #### 2.4.2) Update xy ####

  # Determine number of lines of text per node
  int_lines <- sapply(
    seq_along(inputs),
    function(n) sum( grepl( '\n', inputs[[n]]$string, fixed = TRUE ) )
  ) + 1

  # Extract node heights
  num_node_h <- sapply(
    seq_along(inputs), function(n) inputs[[n]]$wh[2]
  )
  names( num_node_h ) <- names(inputs)

  # Determine height of one line of text
  if ( any(int_lines == 1) ) {

    num_node_h_1 <- mean(
      num_node_h[ int_lines == 1 ]
    )

    # Close 'Determine height of one line of text'
  } else {

    num_node_h_1 <- mean(
      num_node_h[ int_lines > 0 ] / int_lines[ int_lines > 0 ]
    )

    # Close else for 'Determine height of one line of text'
  }

  # Update xy as needed
  for ( n in seq_along(inputs) ) {

    num_xy <- inputs[[n]]$xy

    # If xy not specified
    if ( any( is.na( num_xy ) ) ) {

      num_from <- inputs[[n]]$from
      chr_from <- names( num_from )
      chr_node_from <- sapply(
        chr_from, function(s) {

          # If a node is given
          if ( !s %in% c( 'x', 'y' ) ) {

            chr_out <- strsplit(
              s, split = '.', fixed = TRUE
            )[[1]][1]

            # Close 'If a node is given'
          } else {

            chr_out <- s

            # Close else for 'If a node is given'
          }

        }
      )

      # Loop over x and y
      for ( i in 1:2 ) {

        lgc_no_nodes <- TRUE
        lgc_nodes <- chr_nodes %in% chr_node_from[i]

        # If specified from any nodes
        if ( any( lgc_nodes ) ) {

          # Check if specified from a given node
          if ( chr_node_from[i] == chr_nodes[lgc_nodes] ) {

            lst_node_from <- pd_node_dimensions(
              inputs[[ chr_nodes[lgc_nodes] ]]$xy,
              inputs[[ chr_nodes[lgc_nodes] ]]$wh
            )

            chr_pos <- strsplit(
              chr_from[i], split = '.', fixed = TRUE
            )[[1]][2]

            num_size <- c(
              w = lst_node_from$right[1] - lst_node_from$left[1],
              h = lst_node_from$top[2] - lst_node_from$bottom[2]
            )
            # print( lst_node_from[[ chr_pos ]] )


            # Specify width
            if ( i == 1 ) {

              num_xy[i] <-
                lst_node_from[[ chr_pos ]][i] +
                num_from[i]*num_size[i]

              # Close 'Specify height'
            }

            # Specify height
            if ( i == 2 ) {

              num_xy[i] <-
                lst_node_from[[ chr_pos ]][i] +
                num_from[i]*num_node_h_1 +
                .5*inputs[[n]]$wh[i]*sign(num_from[i])

              # Close 'Specify height'
            }

            # print( chr_pos )

            lgc_no_nodes <- FALSE

            # Close 'Check if specified from a given node'
          }

          # Close 'If specified from any nodes'
        }

        # If not from a specific node
        if ( lgc_no_nodes ) {

          num_xy[i] <- num_from[i]

          # Close else from 'If not from a specific node'
        }

        # Close 'Loop over x and y'
      }

      names(num_xy) <- c('x', 'y')
      inputs[[n]]$xy <- num_xy
      # print( num_xy )

      # Close 'If xy not specified'
    }

    # Close 'Update xy as needed'
  }

  #### 2.4.3) Add paths ####

  # Add paths
  for ( n in seq_along(inputs) ) {

    lst_node_current <- pd_node_dimensions(
      inputs[[n]]$xy,
      inputs[[n]]$wh
    )

    # If path was specified
    if ( !is.null( inputs[[n]]$to ) ) {

      # Loop over paths
      for (p in seq_along( inputs[[n]]$to ) ) {

        chr_path <- strsplit(
          inputs[[n]]$to[p], split = ' ', fixed = TRUE
        )[[1]]

        num_path <- c(
          x0 = lst_node_current[[ chr_path[1] ]]['x'],
          x1 = NA,
          y0 = lst_node_current[[ chr_path[1] ]]['y'],
          y1 = NA
        )
        # print( num_path )

        # to = 'left - x=N03_2.center,y=N05_2.left'

        # If x|y coordinates specified
        if ( grepl( ',', chr_path[3], fixed = TRUE ) ) {

          chr_path_xy <- strsplit(
            chr_path[3], split = ',', fixed = TRUE
          )[[1]]
          chr_path_xy[1] <- gsub( 'x=', '', chr_path_xy[1], fixed = TRUE )
          chr_path_xy[2] <- gsub( 'y=', '', chr_path_xy[2], fixed = TRUE )

          # Node specified
          if ( grepl( 'e', chr_path_xy[1] ) |
               grepl( 'o', chr_path_xy[1] ) ) {

            chr_node_parts <- strsplit(
              chr_path_xy[1], split = '.', fixed = TRUE
            )[[1]]

            lst_node_to <- pd_node_dimensions(
              inputs[[ chr_node_parts[1] ]]$xy,
              inputs[[ chr_node_parts[1] ]]$wh
            )

            num_path['x1'] <- lst_node_to[[ chr_node_parts[2] ]]['x']

            # Close 'Node specified'
          } else {

            num_path['x1'] <- as.numeric( chr_path_xy[1] )

            # Close else for 'Node specified'
          }

          # Node specified
          if ( grepl( 'e', chr_path_xy[2] ) |
               grepl( 'o', chr_path_xy[2] ) ) {

            chr_node_parts <- strsplit(
              chr_path_xy[2], split = '.', fixed = TRUE
            )[[1]]

            lst_node_to <- pd_node_dimensions(
              inputs[[ chr_node_parts[1] ]]$xy,
              inputs[[ chr_node_parts[1] ]]$wh
            )

            num_path['y1'] <- lst_node_to[[ chr_node_parts[2] ]]['y']

            # Close 'Node specified'
          } else {

            num_path['y1'] <- as.numeric( chr_path_xy[2] )

            # Close else for 'Node specified'
          }

          # Close 'If x|y coordinates specified'
        } else {

          chr_node_parts <- strsplit(
            chr_path[3], split = '.', fixed = TRUE
          )[[1]]

          lst_node_to <- pd_node_dimensions(
            inputs[[ chr_node_parts[1] ]]$xy,
            inputs[[ chr_node_parts[1] ]]$wh
          )

          num_path['x1'] <- lst_node_to[[ chr_node_parts[2] ]]['x']
          num_path['y1'] <- lst_node_to[[ chr_node_parts[2] ]]['y']

          # print( lst_node_to )

          # Close else for 'If x|y coordinates specified'
        }
        names( num_path ) <- c( 'x0', 'x1', 'y0', 'y1' )

        int_code <- 0
        if ( chr_path[2] == '->' ) int_code <- 2
        if ( chr_path[2] == '<-' ) int_code <- 1
        if ( chr_path[2] == '<->' ) int_code <- 3

        lst_arrows <- args.arrows

        # Initialize arguments
        if ( is.null(lst_arrows) ) {

          lst_arrows <- list()

          # Close 'Initialize arguments'
        }

        lst_arrows$x0 <- num_path['x0']
        lst_arrows$x1 <- num_path['x1']
        lst_arrows$y0 <- num_path['y0']
        lst_arrows$y1 <- num_path['y1']
        lst_arrows$code <- int_code

        do.call( arrows, lst_arrows )

        # Close 'Loop over paths'
      }

      # Close 'If path was specified'
    }

    # Close 'Add paths'
  }

  #### 2.4.4) Add nodes ####

  # Add nodes
  for ( n in seq_along(inputs) ) {

    inputs[[n]]$args.polygon <- pd_int_add.args(
      inputs[[n]]$args.polygon,
      args.polygon
    )

    pd_int_add.node_shape(
      xy = inputs[[n]]$xy,
      wh = inputs[[n]]$wh,
      shape = inputs[[n]]$shape,
      args.polygon = inputs[[n]]$args.polygon
    )

    num_wh <- pd_int_add.node_text(
      string = inputs[[n]]$string,
      xy = inputs[[n]]$xy,
      wh = inputs[[n]]$wh,
      cex = inputs[[n]]$args.text$cex,
      col = inputs[[n]]$args.text$col,
      align = inputs[[n]]$args.text$align,
      spacing = inputs[[n]]$args.text$spacing,
      add = TRUE
    )
    # print( num_wh )

    # Close 'Add nodes'
  }

  invisible( inputs )
}

#### 3) Helper functions ####

#### 3.1) xy ####
#' Vector for X and Y Coordinates
#'
#' Function to produce named vector with x and
#' y coordinates to pass to [pathdiagrams::pd_node].
#'
#' @param x A numeric value between 0 and 1.
#' @param y A numeric value between 0 and 1.
#'
#' @returns A named vector.
#'
#' @export

xy <- function(
    x,
    y ) {

  num_xy <- c( x = x, y = y )

  return( num_xy )
}

#### 3.2) wh ####
#' Vector for width and height
#'
#' Function to produce named vector with width
#' and height to pass to [pathdiagrams::pd_node].
#'
#' @param w A numeric value between 0 and 1.
#' @param h A numeric value between 0 and 1.
#'
#' @returns A named vector.
#'
#' @export

wh <- function(
    w,
    h ) {

  num_wh <- c( w = w, h = h )

  return( num_wh )
}

#### 3.3) pd_node_template ####
#' Create Template for Nodes
#'
#' Function to generate template code for
#' creating nodes in a path diagram.
#'
#' @param n A integer value, the number of nodes
#'   to include in the template code.
#'
#' @returns A message to the console window with the
#' template code.
#'
#' @export

pd_node_template <- function(
    n ) {

  chr_start <-
    'lst_nodes <- list() |>\n'

  chr_nodes <- as.character(
    1:n
  )
  chr_nodes[ (1:n) < 10 ] <-
    paste0( '0', chr_nodes[ (1:n) < 10] )
  chr_nodes <- paste0( 'N', chr_nodes )

  chr_node <- sapply(
    1:n, function(i) {

      if ( i == 1 ) {

        chr_current <- paste0(
          "  pd_node(\n",
          "    '", chr_nodes[i], "', xy = xy(0.5, 0.9), ",
          "wh = wh( 0.15, NA ),\n",
          "    string = 'Placeholder',\n",
          "    args.polygon = list( col = 'white' )\n",
          "  )"
        )

      } else {

        chr_current <- paste0(
          "  pd_node(\n",
          "    '", chr_nodes[i], "', from = c(",
          chr_nodes[i-1], ".center = 0, ",
          chr_nodes[i-1], ".bottom = -1.5), ",
          "wh = wh( 0.15, NA ),\n",
          "    string = 'Placeholder',\n",
          "    to = 'top - ",
          chr_nodes[i-1], ".bottom',\n",
          "    args.polygon = list( col = 'white' )\n",
          "  )"
        )

      }

    }
  )

  message(
    paste0(
      chr_start,
      paste( chr_node, collapse = ' |>\n' )
    )
  )

}

# pd_base_figure( new = TRUE, guidelines = FALSE )
#
# lst_nodes <- list() |>
#   pd_node(
#     'N01', xy = xy(0.5, 0.8),
#     string = 'Node-01'
#   ) |>
#   pd_node(
#     'N02', from = c(N01.center = -1.25, N01.bottom = -1.5),
#     string = 'Node-02\nLine 2',
#     to = 'top - N01.bottom'
#   ) |>
#   pd_node(
#     'N03', from = c(N01.center = 0, N01.bottom = -1.5),
#     string = 'Node-03',
#     to = 'top - N01.bottom'
#   ) |>
#   pd_node(
#     'N04', from = c(N01.center = 1.25, N01.bottom = -1.5),
#     string = 'Node-04',
#     to = 'top - N01.bottom'
#   )
#
# pd_draw_nodes( lst_nodes, args.polygon = list( col = 'white' ),
#                args.text = list( cex = .8 ) )

