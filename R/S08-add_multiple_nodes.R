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
  if ( tag == 'ns-' ) out = lod$node_shape
  if ( tag == 'nc-' ) out = lod$node_shape.col
  if ( tag == 'nb-' ) out = lod$node_shape.border
  if ( tag == 'nw-' ) out = lod$node_shape.lwd
  if ( tag == 'np-' ) out = lod$node_shape.pad

  # Node text
  if ( tag == 'ts-' ) out = lod$node_text.size
  if ( tag == 'tc-' ) out = lod$node_text.col
  if ( tag == 'tf-' ) out = lod$node_text.font

  # Path line
  if ( tag == 'lp-' ) out = lod$path_line.pad
  if ( tag == 'lw-' ) out = lod$path_line.lwd
  if ( tag == 'lc-' ) out = lod$path_line.col
  if ( tag == 'll-' ) out = lod$path_line.length
  if ( tag == 'la-' ) out = lod$path_line.angle
  if ( tag == 'lt-' ) out = lod$path_line.lty
  if ( tag == 'cd' )  out = lod$path_line.code

  check = grepl( tag, input_parts, fixed = T )

  if ( any( check ) ) {


    # Character output tags
    if ( tag %in% c( 'ns-', 'nc-', 'nb-', 'tc-', 'tf-',
                     'cd', 'lc-' ) ) {

      val = input_parts[ check ]

      val = gsub( tag, '', val, fixed = T )

      if ( val == 'NA' ) val = NA

      out = val

      # Close conditional for node shape
    }

    # Numeric output tags
    if ( tag %in% c( 'nw-', 'ts-', 'lp-', 'lw-', 'll-',
                     'la-', 'lt-' ) ) {

      val = input_parts[ check ]

      val = as.numeric( gsub( tag, '', val, fixed = T ) )
      print( val )

      out = val

      # Close conditional for node shape
    }

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
#' @param node_shape The default shape for nodes, either 'box',
#'   'circle', or 'blank'; options for individual nodes can be
#'   specified with the tag \code{ns-}.
#' @param node_shape.col The default color for nodes; options
#'   for individual nodes can be specified with the tag \code{nc-}.
#' @param node_shape.lwd The default line width for node borders;
#'   options for individual nodes can be specified with the
#'   tag \code{nw-}.
#' @param node_shape.border The default border color for nodes;
#'   options for individual nodes can be specified with the
#'   tag \code{nb-}.
#' @param node_text.size The default size for text content;
#'   options for individual nodes can be specified with the
#'   tag \code{ts-}.
#' @param node_text.col The default color for text content;
#'   options for individual nodes can be specified with the
#'   tag \code{tc-}.
#' @param node_text.font The default font for text content;
#'   options for individual nodes can be specified with the
#'   tag \code{tf-}.
#' @param path_line.pad ...
#' @param path_line.lwd The default line width for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{lw-}.
#' @param path_line.col The default line color for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{lc-}.
#' @param path_line.length The default arrowhead length for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{ll-}.
#' @param path_line.angle The default angle of arrowheads for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{la-}.
#' @param path_line.lty The default line type for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{lt-}.
#' @param path_line.code The default arrow direction for paths;
#'   options for individual nodes can be specified with the
#'   tag \code{cd} (note the lack of hyphen). Directions
#'   are specified with the symbols \code{->}, \code{<-},
#'   \code{<->}, or \code{-}.
#'
#' @details
#'
#' Each node is specified via a text string in the format:
#'
#' \code{"Text content|x-[value]|y-[value]|..."}
#'
#' substituting \code{[value]} with the respective x and
#' y-axis coordinate positions for the node, and \code{...}
#' referring to additional options controlling node
#' aesthetics.
#'
#' Options are specified as a tag followed by a value; for
#' example, to draw a node as an ellipse one combines
#' the tag \code{ns-} and value \code{circle}. Multiple
#' options can be specified by separating them with the
#' \code{|} symbol.
#'
#' Paths (lines or arrows) can be drawn between existing
#' nodes whose string input is labeled, via the format:
#'
#' \code{ Starting point   Ending point}
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
#' can be set via the tag and value: \code{lt-2}.
#'
#' @examples
#' # Define vector of string inputs for nodes to draw
#' input = c(
#'   # [Node label] = "Text|x-[value]|y-[value]|..."
#'   N01 = 'Node-01|x.2|y.33',
#'   # Set node shape to ellipse; resize and color text
#'   N02 = 'Node-02|x.5|y.66|ns-circle|ts-2|tc-blue',
#'   # Color node and remove border
#'   N03 = 'Node-03|x.8|y.33|nc-grey80|nb-NA'
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
#'   'N02|bottomright|N03|topleft'
#'   # Orange dashed thick line
#'   'N01|top|N02|bottom|lc-orange|lt-2|lw-4',
#'   # Blue double-headed arrow with small arrowhead
#'   'N01|topright|N02|bottomleft|lc-blue|ll-.1|cd<->'
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
                               node_shape = 'box',
                               node_shape.col = 'white',
                               node_shape.lwd = 2,
                               node_shape.border = 'black',
                               node_shape.pad = .5,
                               #   Node text
                               node_text.size = 1.25,
                               node_text.col = 'black',
                               node_text.font = NULL,
                               #   Path line
                               path_line.pad = .025,
                               path_line.lwd = 2,
                               path_line.col = 'black',
                               path_line.length = .25,
                               path_line.angle = 30,
                               path_line.lty = 1,
                               path_line.code = '->' ) {

  # Specify default settings for node aesthetics
  lod = list(
    # Node shape
    node_shape = node_shape,
    node_shape.col = node_shape.col,
    node_shape.lwd = node_shape.lwd,
    node_shape.border = node_shape.border,
    node_shape.pad = node_shape.pad,
    # Node text
    node_text.size = node_text.size,
    node_text.col = node_text.col,
    node_text.font = node_text.font,
    # Path line
    path_line.pad = path_line.pad,
    path_line.lwd = path_line.lwd,
    path_line.col = path_line.col,
    path_line.length = path_line.length,
    path_line.angle = path_line.angle,
    path_line.lty = path_line.lty,
    path_line.code = path_line.code
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
    node_shape = multiple_node_aes( 'ns-', input_parts, lod )
    node_shape.col = multiple_node_aes( 'nc-', input_parts, lod )
    node_shape.width = multiple_node_aes( 'nw-', input_parts, lod )
    node_shape.border = multiple_node_aes( 'nb-', input_parts, lod )
    node_shape.pad = multiple_node_aes( 'np-', input_parts, lod )
    node_text.size = multiple_node_aes( 'ts-', input_parts, lod )
    node_text.color = multiple_node_aes( 'tc-', input_parts, lod )
    node_text.font = multiple_node_aes( 'tf-', input_parts, lod )

    # At a minimum
    # Text | x-axis coordinates | y-axis coordinates

    # Extract coordinates
    xp = as.numeric( gsub( 'x', '', input_parts[2] ) )
    yp = as.numeric( gsub( 'y', '', input_parts[3] ) )

    # Determine width/height of text
    sw = strwidth( input_parts[1], cex = node_text.size )
    sh = strheight( input_parts[1], cex = node_text.size )

    # Pad dimensions of node to be slightly
    # larger than text content
    adj = sh * node_shape.pad

    # x-axis lower and upper boundaries
    xb = c( xp - sw/2 - adj,
            xp + sw/2 + adj )
    # y-axis lower and upper boundaries
    yb = c( yp - sh/2 - adj,
            yp + sh/2 + adj )

    # Coordinates for node
    nd[[ i ]]$left = c( xb[1], yp )
    nd[[ i ]]$right = c( xb[2], yp )

    nd[[ i ]]$top = c( xp, yb[2] )
    nd[[ i ]]$bottom = c( xp, yb[1] )

    nd[[ i ]]$bottomleft = c( xb[1], yb[1] )
    nd[[ i ]]$bottomright = c( xb[2], yb[1] )

    nd[[ i ]]$topleft = c( xb[1], yb[2] )
    nd[[ i ]]$topright = c( xb[2], yb[2] )

    # Add shape around node

    # Draw a rectangle around the text
    if ( node_shape %in% c( 'box', 'rectangle', 'rect', 'square' ) ) {

      # Draw shape
      polygon( xb[c(1,1,2,2)],
               yb[c(1,2,2,1)],
               col = node_shape.col,
               border = node_shape.border,
               lwd = node_shape.lwd )

      # Close conditional for rectangle
    }

    # Draw an ellipse around node
    if ( node_shape %in% c( 'circle', 'ellipse', 'circ', 'ell' ) ) {

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
               col = node_shape.col,
               border = node_shape.border,
               lwd = node_shape.lwd )

      # Close conditional for ellipse
    }

    # Add text content
    text( xp, yp,
          input_parts[1],
          cex = node_text.size,
          col = node_text.color,
          font = node_text.font )



    # Close loop over node inputs
  }

  # Check if vector of inputs for arrows
  # was provided
  if ( !is.null( paths ) ) {

    # Loop over inputs
    for ( i in 1:length( paths ) ) {

      # Extract details on current arrow
      path_parts = strsplit( paths[ i ], split = '|', fixed = T )[[1]]

      # Check for additional options
      path_line.pad = multiple_node_aes( 'lp-', path_parts, lod )
      path_line.lwd = multiple_node_aes( 'lw-', path_parts, lod )
      path_line.col = multiple_node_aes( 'lc-', path_parts, lod )
      path_line.length = multiple_node_aes( 'll-', path_parts, lod )
      path_line.angle = multiple_node_aes( 'la-', path_parts, lod )
      path_line.lty = multiple_node_aes( 'lt-', path_parts, lod )
      path_line.code = multiple_node_aes( 'cd', path_parts, lod )

      # At a minimum
      # Node label - start | Node coordinate - start
      # ... Node label - end | Node coordinate - end

      start_pos = nd[[ path_parts[1] ]][[ path_parts[2] ]]
      end_pos = nd[[ path_parts[3] ]][[ path_parts[4] ]]

      positions = c(
        'bottom',     # [1]
        'top',        # [2]
        'right',      # [3]
        'left',       # [4]
        'bottomleft', # [5]
        'topleft',    # [6]
        'topright',   # [7]
        'bottomright' # [8]
      )

      # Pad start and end-points of line

      # Start point
      pp = 2

      if ( path_parts[pp] == 'bottom' ) {
        start_pos[2] = start_pos[2] - path_line.pad
      }
      if ( path_parts[pp] == 'top' ) {
        start_pos[2] = start_pos[2] + path_line.pad
      }
      if ( path_parts[pp] == 'right' ) {
        start_pos[1] = start_pos[1] + path_line.pad
      }
      if ( path_parts[pp] == 'left' ) {
        start_pos[1] = start_pos[1] - path_line.pad
      }
      if ( path_parts[pp] == 'bottomleft' ) {
        start_pos[1] = start_pos[1] - path_line.pad
        start_pos[2] = start_pos[2] - path_line.pad
      }
      if ( path_parts[pp] == 'topleft' ) {
        start_pos[1] = start_pos[1] - path_line.pad
        start_pos[2] = start_pos[2] + path_line.pad
      }
      if ( path_parts[pp] == 'topright' ) {
        start_pos[1] = start_pos[1] + path_line.pad
        start_pos[2] = start_pos[2] + path_line.pad
      }
      if ( path_parts[pp] == 'bottomright' ) {
        start_pos[1] = start_pos[1] + path_line.pad
        start_pos[2] = start_pos[2] - path_line.pad
      }

      # End point
      pp = 4

      if ( path_parts[pp] == 'bottom' ) {
        end_pos[2] = end_pos[2] - path_line.pad
      }
      if ( path_parts[pp] == 'top' ) {
        end_pos[2] = end_pos[2] + path_line.pad
      }
      if ( path_parts[pp] == 'right' ) {
        end_pos[1] = end_pos[1] + path_line.pad
      }
      if ( path_parts[pp] == 'left' ) {
        end_pos[1] = end_pos[1] - path_line.pad
      }
      if ( path_parts[pp] == 'bottomleft' ) {
        end_pos[1] = end_pos[1] - path_line.pad
        end_pos[2] = end_pos[2] - path_line.pad
      }
      if ( path_parts[pp] == 'topleft' ) {
        end_pos[1] = end_pos[1] - path_line.pad
        end_pos[2] = end_pos[2] + path_line.pad
      }
      if ( path_parts[pp] == 'topright' ) {
        end_pos[1] = end_pos[1] + path_line.pad
        end_pos[2] = end_pos[2] + path_line.pad
      }
      if ( path_parts[pp] == 'bottomright' ) {
        end_pos[1] = end_pos[1] + path_line.pad
        end_pos[2] = end_pos[2] - path_line.pad
      }

      # Determine type of line
      if ( path_line.code == '->' ) {
        arrow_type = 2
      }
      if ( path_line.code == '<-' ) {
        arrow_type = 1
      }
      if ( path_line.code == '<->' ) {
        arrow_type = 3
      }
      if ( path_line.code == '-' ) {
        arrow_type = 0
      }

      # Draw path
      arrows( start_pos[1], start_pos[2],
              end_pos[1], end_pos[2],
              code = arrow_type,
              length = path_line.length,
              angle = path_line.angle,
              col = path_line.col,
              lty = path_line.lty,
              lwd = path_line.lwd )

      # Close loop over inputs
    }

    # Close conditional on 'paths' argument
  }

  # If specified return output
  if ( output ) {
    return( nd )
  }
}

