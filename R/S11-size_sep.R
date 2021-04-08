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
#' Determine the distance between x-axis
#' or y-axis coordinates for two nodes in
#' a path diagram.
#'
#' @param nodes A list of lists, each list
#'   giving the x and y coordinates for the
#'   bottom, left, top, right and associated
#'   corners for a given node (e.g., see output
#'   of \code{\link{add_multiple_nodes}} or
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
#' nodes <- add_multiple_nodes(
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
#' add_multiple_nodes( c( N3 = 'Node-03|x=.3|y=.534' ) )
#'
#' # Determine new y-axis coordinate for 'N1' so
#' # that distance between nodes would be 0.1
#' sep( nodes, c( 'N2', 'N1' ), desired = .1 )[2]
#' # Add new node using proposed y-axis coordinate
#' add_multiple_nodes( c( N4 = 'Node-04|x=.7|y=.466' ) )
#'
#' # Distance from left edge of 'N1' and
#' # right edge of 'N2'
#' sep( nodes, c( 'N1', 'N2' ), dimension = 'x-axis' )
#'
#' # Determine new x-axis coordinate for 'N2' so
#' # that distance between nodes would be 0.1
#' sep( nodes, c( 'N1', 'N2' ), 'x-axis', desired = .1 )[2]
#' # Add new node using proposed x-axis coordinate
#' add_multiple_nodes( c( N5 = 'Node-05|x=.589|y=.7' ) )
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

