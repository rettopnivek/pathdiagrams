#' Create Padding for Character Strings
#'
#' Given a vector specifying number of spaces, will
#' create indents to pass in to the 'left' argument
#' of the 'node_content' function.
#'
#' @param n_spaces A vector giving the number of spaces to
#'   use when creating indents.
#'
#' @return A character vector.
#'
#' @examples
#' padding( c( 0, 2, 5 ) )
#'
#' @export

padding = function( n_spaces ) {

  l = length( n_spaces )
  out = rep( '', length( n_spaces ) )

  for ( i in 1:l ) {

    if ( n_spaces[i] > 0 ) {
      out[i] = paste( rep( ' ', n_spaces[i] ), collapse = '' )
    }

  }

  return( out )
}
