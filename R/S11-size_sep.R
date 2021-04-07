#' ...
#'
#' ...
#'
#' @param node ...
#'
#' @examples
#' # Forthcoming
#'
#'
#' @export

size = function( node ) {

  w = node$right[1] - node$left[1]
  h = node$top[2] - node$bottom[2]

  return( c( width = w, height = h ) )
}

#' ...
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

