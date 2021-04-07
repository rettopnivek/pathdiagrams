#' Determine Width and Height of a Node
#'
#' Determine the size (width and height)
#' of a node in an existing path diagram.
#'
#' @param node A list with x and y
#'   coordinates for the bottom, left,
#'   top, right, and associated corners
#'   for a node (e.g., see output of
#'   \code{\link{add_multiple_nodes}} or
#'   \code{\link{add_lines_of_text}}).
#'
#' @examples
#' # Base figure
#' create_base_figure()
#'
#' # Add nodes
#' nodes <- add_multiple_nodes(
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

#' Determine Separation Between Nodes
#'
#' ...
#'
#' @param nodes ...
#' @param labels ...
#' @param dimension ...
#' @param desired ...
#'
#' @examples
#' # Forthcoming
#'
#'
#' @export

sep = function( nodes, labels,
                dimension = 'height',
                desired = NULL ) {

  out = NULL

  n1 = nodes[[ labels[1] ]]
  n2 = nodes[[ labels[2] ]]

  if ( dimension %in% c( 'Height', 'height', 'H', 'h', '1' ) ) {

    highest = which.max( c( n1$top[2], n2$top[2] ) )
    if ( highest == 2 ) {
      nn = n1; n1 = n2; n2 = nn
    }
    out = n1$bottom[2] - n2$top[2]

    if ( !is.null( desired ) ) {

      new_top = n1$bottom[2] - desired
      adj = new_top - n2$top[2]

      out = c( out, I2 = n2$left[2] + adj )
      if ( highest == 2 ) names( out )[2] = 'I1'

    }

  }

  if ( dimension %in% c( 'Width', 'width', 'W', 'w', '2' ) ) {

    closest = which.min( c( n1$left[1], n2$left[1] ) )
    if ( closest == 2 ) {
      nn = n1; n1 = n2; n2 = nn
    }
    out = n1$right[1] - n2$left[1]

    if ( !is.null( desired ) ) {

      new_left = n1$right[1] + desired
      adj = new_left - n2$left[1]

      out = c( out, I2 = n2$top[1] - adj )
      if ( closest == 2 ) names( out )[2] = 'I1'

    }

  }

  return( out )
}

