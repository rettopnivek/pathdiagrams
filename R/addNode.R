# The 'addNode' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-10-14

# Table of contents
# 1) addNode

###
### 1) addNode
###

#' Add Node to an Existing Diagram
#'
#' A function to add a node to an existing diagram.
#'
#' @param x The x-axis position for the center of the
#'   node.
#' @param y The y-axis position for the center of the
#'   node.
#' @param string The label for the node.
#' @param shape A character string indicating the
#'   shape of the node, either \code{'ellipse'} or
#'   \code{'rectangle'}.
#' @param dmn An optional vector with either 1) the
#'   weights to expand the width and height of the node
#'   based on the dimension of the text, or 2) absolute
#'   values controlling the width and height of the node,
#'   irrespective of text dimensions.
#' @param fixed Logical; if \code{TRUE}, indicates
#'   that the input for \code{dmn} represents the
#'   width and height, rather than relative weights.
#' @param text.cex Numeric character expansion factor;
#'   multiplied by par("cex") yields the final character
#'   size. NULL and NA are equivalent to 1.0.
#' @param text.adj One or two values in [0, 1] which
#'   specify the x (and optionally y) adjustment of
#'   the labels. On most devices values outside that
#'   interval will also work..
#' @param text.col the color of the font.
#' @param text.vfont \code{NULL} for the current font
#'   family, or a character vector of length 2 for
#'   Hershey vector fonts. The first element of the
#'   vector selects a typeface and the second
#'   element selects a style.
#' @param ... Additional parameters for the
#'   \code{\link[graphics]{polygon}} function.
#'
#' @return A list giving the Cartesian coordinates for
#'   different locations of the node.
#'
#' @examples
#' blankPlot()
#' n1 <- addNode( .5, .5, 'Center' )
#' n2 <- addNode( .25, .25, 'Bottom left', shape = 'rectangle' )
#'
#' @export

addNode = function( x, y, string,
                    shape = 'ellipse',
                    dmn = NULL,
                    fixed = FALSE,
                    text.cex = 1,
                    text.adj = NULL,
                    text.col = NULL,
                    text.vfont = NULL,
                    text.srt = NULL,
                    ... ) {

  if ( !fixed ) {

    # Default values
    if ( is.null( dmn ) ) {
      dmn = c( 1.6, 1.6 )
    }

    # Determine dimensions
    h = strheight( string )
    w = strwidth( string )
    w2 = dmn[1]*w/2
    h2 = dmn[2]*h/2

  } else {

    # Default values
    if ( is.null( dmn ) ) {
      dmn = c( .15, .05 )
    }

    # Determine dimensions
    w2 = dmn[1]/2
    h2 = dmn[2]/2

  }

  # Draw node shape
  if ( shape == 'ellipse' ) {
    utilityf::drawEllipse( w2, h2, Xc = x, Yc = y, ... )
  }
  if ( shape == 'rectangle' ) {
    xa = x + w2*c( -1, -1, 1, 1 )
    ya = y + h2*c( -1, 1, 1, -1 )
    polygon( xa, ya, ... )
  }

  # Add text to node
  text( x, y, string,
        cex = text.cex,
        adj = text.adj,
        col = text.col,
        vfont = text.vfont,
        srt = text.srt )

  # Return output
  out = list(
    center = c( x, y ),
    left = c( x - w2, y ),
    right = c( x + w2, y ),
    top = c( x, y + h2 ),
    bottom = c( x, y - h2 ),
    topleft = c( x - w2, y + h2 ),
    bottomleft = c( x - w2, y - h2 ),
    topright = c( x + w2, y + h2 ),
    bottomright = c( x + w2, y - h2 )
  )

  return( out )
}


