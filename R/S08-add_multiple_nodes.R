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
                     'ny=' ) ) {

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
#' add_multiple_nodes(
#'   input, paths = paths
#' )
#'
#' @export

add_multiple_nodes = function( input,
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
                               path.length = .25,
                               path.angle = 30,
                               path.lty = 1,
                               path.code = '->' ) {

  #< Default options for text spacing
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

    #< Close conditional for default options for text spacing
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

  #< Loop over inputs
  for ( i in 1:length( input ) ) {

    # Extract details on current node
    input_parts = strsplit( input[ i ], split = '|', fixed = T )[[1]]

    # Check for additional options

    # Node options
    shape = multiple_node_aes( 'ns=', input_parts, lod )
    shape.col = multiple_node_aes( 'nc=', input_parts, lod )
    shape.width = multiple_node_aes( 'nw=', input_parts, lod )
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
    sw = strwidth( input_parts[1], cex = text.size )
    sh = strheight( input_parts[1], cex = text.size )

    # Pad dimensions of node to be slightly
    # larger than text content
    adj = sh * shape.pad

    #<< x-axis lower and upper boundaries
    if ( is.na( shape.x ) ) {
      # If no fixed dimensions are provided

      # Size based on string dimensions
      xb = c( xp - sw/2 - adj,
              xp + sw/2 + adj )

      #>> Close conditional on string dimensions
    } else {

      xb = c( xp - shape.x/2,
              xp + shape.x/2 )

      #>> Close conditional on fixed dimensions
    }

    #<< y-axis lower and upper boundaries
    if ( is.na( shape.y ) ) {
      # If no fixed dimensions are provided

      # Size based on string dimensions
      yb = c( yp - sh/2 - adj,
              yp + sh/2 + adj )

      #>> Close conditional on string dimensions
    } else {

      yb = c( yp - shape.y/2,
              yp + shape.y/2 )

      #>> Close conditional on fixed dimensions
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

    #<< Check if single line
    if ( !grepl( '\n', input_parts[1], fixed = T ) ) {

      # Add shape around node

      #<<< Draw a rectangle around the text
      if ( shape %in% c( 'box', 'rectangle', 'rect', 'square' ) ) {

        # Draw shape
        polygon( xb[c(1,1,2,2)],
                 yb[c(1,2,2,1)],
                 col = shape.col,
                 border = shape.border,
                 lwd = shape.lwd,
                 lty = shape.lty )

        #>>> Close conditional for rectangle
      }

      #<<< Draw an ellipse around node
      if ( shape %in% c( 'circle', 'ellipse', 'circ', 'ell' ) ) {

        # Distence of center to foci
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
        polygon( xv,
                 yv,
                 col = shape.col,
                 border = shape.border,
                 lwd = shape.lwd,
                 lty = shape.lty )

        #>>> Close conditional for ellipse
      }

      # Add text content
      text( xp, yp,
            input_parts[1],
            cex = text.size,
            col = text.color )

      #>> Close conditional on single line
    } else {

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
        output = T
      )
      # spacing
      # spacing.fixed

      #>> Close conditional on multiple lines
    }

    # Debugging for node dimensions
    if ( FALSE ) {
      for ( k in 1:length( nd[[ i ]] ) ) {
        points( nd[[i]][[k]][1],
                nd[[i]][[k]][2],
                pch = 19, cex = .75 )
      }
    }

    #> Close loop over node inputs
  }

  # Check if vector of inputs for arrows
  # was provided
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
      arrows( start_pos[1], start_pos[2],
              end_pos[1], end_pos[2],
              code = arrow_type,
              length = path.length,
              angle = path.angle,
              col = path.col,
              lty = path.lty,
              lwd = path.lwd )

      # Close loop over inputs
    }

    # Close conditional on 'paths' argument
  }

  # If specified return output
  if ( output ) {
    return( nd )
  }
}

