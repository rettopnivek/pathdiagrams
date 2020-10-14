# The 'addArrow' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-10-15

# Table of contents
# 1) addNodeLine

###
### 1) addArrow
###

#' Add an Arrow Linking Nodes
#'
#' A function that adds an arrow pointing
#' from a starting node to an end node on
#' an existing diagram.
#'
#' @param node1,node2 Output from the
#'   \code{\link{addNode}} function.
#' @param pad A vector with the adjustment to
#'   1) the left/right and 2) top/bottom
#'   points from which the arrow starts and ends.
#' @param width A vector with the width for 1)
#'   the band and 2) the arrow head.
#' @param arrow A proportion controlling where the
#'   arrow head begins.
#' @param positions A vector giving the points on
#'   \code{node1} and \code{node2}, respectively,
#'   to link via the arrow.
#' @param ... Additional parameters for the
#'   \code{\link[graphics]{polygon}} function.
#'
#' @examples
#' blankPlot()
#' n1 <- addNode( .25, .25, 'Start' )
#' n2 <- addNode( .75, .75, 'End' )
#' addArrow( n1, n2 )
#'
#' @export

addArrow = function( node1, node2,
                        pad = c( .02, 0 ),
                        width = c( .025, .04 ),
                        arrow = .8,
                        positions = c( 'right',
                                       'left' ),
                        ... ) {

  # Convert abbreviations
  positions[ positions == 'tr' ] = 'topright'
  positions[ positions == 'tl' ] = 'topleft'
  positions[ positions == 'br' ] = 'bottomright'
  positions[ positions == 'bl' ] = 'bottomleft'
  positions[ positions == 'l' ] = 'left'
  positions[ positions == 'r' ] = 'right'
  positions[ positions == 't' ] = 'top'
  positions[ positions == 'b' ] = 'bottom'

  # Determine cartesian coordinates
  # for line between nodes
  cc = c(
    node1[[ positions[1] ]][1],
    node1[[ positions[1] ]][2],
    node2[[ positions[2] ]][1],
    node2[[ positions[2] ]][2]
  )

  # Determine relative position of nodes
  x_node_1_first = node1$center[1] < node2$center[1]
  y_node_1_first = node1$center[2] < node2$center[2]

  # Adjust x-axis for nodes
  if ( x_node_1_first ) {
    cc[1] = cc[1] + pad[1]
    cc[3] = cc[3] - pad[1]
  } else {
    cc[1] = cc[1] - pad[1]
    cc[3] = cc[3] + pad[1]
  }
  # Adjust y-axis for nodes
  if ( y_node_1_first ) {
    cc[2] = cc[2] + pad[2]
    cc[4] = cc[4] - pad[2]
  } else {
    cc[2] = cc[2] - pad[2]
    cc[4] = cc[4] + pad[2]
  }

  # Arrow consists of 7 points
  pts = data.frame(
    point = 1:7,
    x = NA,
    y = NA
  )

  # Trigonometry (Center line)

  # Height
  b = cc[4] - cc[2]
  # Length
  a = cc[3] - cc[1]
  # Hypontenuse
  h = sqrt( a^2 + b^2 )
  # Angle
  A = asin( a/h )

  # Angle needed for points
  A1 = 90*pi/180 - A

  # Compute corners for starting point
  c1 = width[1]/2
  b1 = cos(A1)*c1
  a1 = sin(A1)*c1

  # Adjust angle based on lowest node
  if ( y_node_1_first ) {
    b1 = -b1
  }

  pts$x[1] = cc[1] - a1
  pts$y[1] = cc[2] - b1
  pts$x[7] = cc[1] + a1
  pts$y[7] = cc[2] + b1

  # Compute corners for arrow head

  # Cartesian coordinates for
  # start of arrow
  c2 = h*arrow
  b2 = cos(A)*c2
  a2 = sin(A)*c2
  if ( !y_node_1_first ) {
    b2 = -b2
  }
  x_arr = cc[1] + a2
  y_arr = cc[2] + b2

  # Start of arrow head

  c3 = width[1]/2
  b3 = cos(A1)*c3
  a3 = sin(A1)*c3

  # Adjust angle based on lowest node
  if ( y_node_1_first ) {
    b3 = -b3
  }

  pts$x[2] = x_arr - a3
  pts$y[2] = y_arr - b3
  pts$x[6] = x_arr + a3
  pts$y[6] = y_arr + b3

  # Edges of arrow head

  c4 = width[2]/2
  b4 = cos(A1)*c4
  a4 = sin(A1)*c4

  # Adjust angle based on lowest node
  if ( y_node_1_first ) {
    b4 = -b4
  }

  pts$x[3] = x_arr - a4
  pts$y[3] = y_arr - b4
  pts$x[5] = x_arr + a4
  pts$y[5] = y_arr + b4

  # Point of arrow head
  pts$x[4] = cc[3]
  pts$y[4] = cc[4]

  polygon( pts$x, pts$y, ... )

}
