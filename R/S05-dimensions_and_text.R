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


