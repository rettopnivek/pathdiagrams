#' Add a Node to a Diagram
#'
#' Adds a node to an existing diagram (see
#' \code{\link{create_base_figure}}.
#'
#' @param content A list governing the position,
#'   appearance, and content of the node (see the
#'   helper function \code{\link{node_content}}).
#' @param check_for_style_guide Logical; if \code{TRUE},
#'   checks if a list called \code{style_guide}
#'   exists in the global environment; if so,
#'   default options for the node generation
#'   are based on the contents of this list.
#' @param ... Additional arguments for the
#'   \code{\link[graphics]{polygon}} function.
#'
#' @return A named list giving the x and y-axis coordinates for
#'   the \code{center}, \code{left}, \code{right}, \code{top},
#'   \code{bottom}, \code{topleft}, \code{bottomleft},
#'   \code{topright}, and \code{bottomright}.
#'
#' @examples
#' # Create diagram
#' create_base_figure( guidelines = F )
#' # Add node
#' nd = add_node( node_content( x = .5, y = .9, text = 'Hello world' ) )
#'
#' @export

add_node = function( content,
                     check_for_style_guide = T,
                     ... ) {

  # 1) Extract content elements

  x = content$x; y = content$y
  txt = content$text
  dmn = content$dimensions
  shape = content$shape
  spacing = content$spacing
  border_color = content$border_color
  border_width = content$border_width
  node_color = content$node_color
  text_size = content$text_size
  text_color = content$text_color
  alignment = content$alignment

  # 2) Defaults


  # Based on style guide
  if ( check_for_style_guide ) {

    if ( exists( 'style_guide', envir = .GlobalEnv ) ) {

      if ( is.null( shape ) )
        shape = style_guide$node$shape

      if ( is.null( spacing ) )
        spacing = style_guide$node$spacing

      if ( is.null( border_color ) )
        border_color = style_guide$node$border_color

      if ( is.null( border_width ) )
        border_width = style_guide$node$border_width

      if ( is.null( node_color ) )
        node_color = style_guide$node$node_color

      if ( is.null( text_size ) )
        text_size = style_guide$node$text_size

      if ( is.null( text_color ) )
        text_color = style_guide$node$text_color

      if ( is.null( alignment ) )
        alignment = style_guide$node$alignment

    }

  }

  # Based on internal function options

  if ( is.null( shape ) )
    shape = 'rectangle'

  if ( is.null( spacing ) )
    spacing = 0.5

  if ( is.null( border_color ) )
    border_color = 'black'

  if ( is.null( border_width ) )
    border_width = 2

  if ( is.null( node_color ) )
    node_color = 'white'

  if ( is.null( text_size ) )
    text_size = 1

  if ( is.null( text_color ) )
    text_color = 'black'

  if ( is.null( alignment ) )
    alignment = 'center'

  # 3) Text content

  # Example construction
  #        | left | special | right | font
  # Line 1 | ''   | 'Hello' | ''    | 2
  # Line 2 | '  ' | 'World' | '!'   | 3

  # Set text size
  par( cex = text_size )

  # In a character vector is provided, convert to list format
  if ( is.character( txt ) ) {

    n_lines = length( txt )
    txt = list(
      left = txt,
      special = rep( '', n_lines ),
      right = rep( '', n_lines ),
      font = rep( 1, n_lines )
    )

  }

  if ( is.list( txt ) ) {

    if ( all( names( txt ) %in% c( 'left', 'special', 'right', 'font' ) ) ) {

      # Determine number of lines for text to add
      n_lines = length( txt$right )
      # Height of text
      h = max( sapply( txt$right, strheight ) )
      # Width of text based on type
      w = matrix( NA, n_lines, 3 )
      colnames( w ) = c( 'left', 'special', 'right' )

      w[,'left'] = sapply( txt$left, strwidth )
      w[,'special'] = sapply( txt$special, strwidth )
      w[,'right'] = sapply( txt$right, strwidth )

    }

  } else {
    stop( '' )
  }

  # spacing
  # 1. Line 1 ~
  #    spacing
  # 2. Line 2 ~
  # spacing

  # Total height and width based on text size
  h_space = h * spacing
  h.total = h * n_lines + ( n_lines + 1) * h_space
  w.total = max( rowSums( w ) )

  # 4) Add background shape

  # By default, size of shape is based on
  # text
  if ( is.null( dmn ) ) {
    dmn = c(
      w.total + h * spacing,
      h.total
    )
  }

  if ( shape == "rectangle" ) {

    xa = x + dmn[1] * c(-.5, -.5, .5, .5)
    ya = y + dmn[2] * c(-1, 0, 0, -1)
    polygon( xa, ya,
             col = node_color,
             border = border_color,
             lwd = border_width,
             ... )

  }

  # 5) Add text

  # Determine position for x-axis
  if ( alignment == 'center' ) {
    x.pos = x - rowSums( w )/2
  }
  if ( alignment == 'left' ) {
    x.pos = rep( x - w.total/2, n_lines )
  }
  positioning = 4

  # Determine position for y-axis
  y.pos = seq( y - h_space - h/2,
               ( y - h.total ) + h_space + h/2,
               length.out = n_lines )
  # y.pos = seq( y, y - h.total, length.out = n_lines + 2 )
  # y.pos = y.pos[ -c( 1, length( y.pos ) ) ]

  # Add text
  for ( i in 1:n_lines ) {

    if ( w[i,1] > 0 ) {
      text( x.pos[i], y.pos[i],
            txt$left[i],
            pos = positioning, offset = 0 )
    }
    if ( w[i,2] > 0 ) {
      text( x.pos[i] + w[i,1], y.pos[i],
            txt$special[i],
            col = text_color,
            pos = positioning, offset = 0, font = txt$font[i] )
    }
    if ( w[i,3] > 0 ) {
      text( x.pos[i] + sum( w[i,1:2] ), y.pos[i],
            txt$right[i],
            col = text_color,
            pos = positioning, offset = 0 )
    }

  }

  x.c = x; y.c = y - dmn[2]/2
  w2 = dmn[1]/2
  h2 = dmn[2]/2

  # Reset text size
  par( cex = 1 )

  out = list(
    center = c( x.c, y.c ),
    left = c(x.c - w2, y.c),
    right = c(x.c + w2, y.c),
    top = c(x.c, y.c + h2),
    bottom = c(x.c, y.c - h2),
    topleft = c(x.c - w2, y.c + h2),
    bottomleft = c(x.c - w2, y.c - h2),
    topright = c(x.c + w2, y.c + h2),
    bottomright = c(x.c + w2, y.c - h2)
  )

  return(out)
}

