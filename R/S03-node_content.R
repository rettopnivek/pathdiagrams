#' Initialize List for Node Content
#'
#' Generates a list with all of the elements
#' used to generate the content of a node.
#' A helper function for \code{\link{add_node}}.
#'
#' @param x The x-axis position of the node (the center).
#' @param y The y-axis position of the node (the top).
#' @param text_content Either 1) a character vector with
#'   each line of text, or 2) a named list with the elements...
#'
#' @param dimensions An optional vector giving the width
#'   and height of the node (assuming a relative plot width
#'   and height of 1).
#' @param shape The shape of the node, with options for...
#' \itemize{
#'   \item 'rectangle'.
#' }
#' @param spacing The relative space between text lines
#'   (defaults to .5 of the text height).
#' @param border_color The color of the border for
#'   the node.
#' @param node_color The fill color for the node.
#' @param text_size The size of the node text.
#' @param text_color The color of the text.
#' @param alignment The alignment of the text, either...
#'   \itemize{
#'     \item 'center';
#'     \item 'left'.
#'   }
#'
#' @return A list.
#'
#' @export

node_content = function( x = .5, y = .5,
                         text = '',
                         dimensions = NULL,
                         shape = NULL,
                         spacing = NULL,
                         border_color = NULL,
                         border_width = 2,
                         node_color = NULL,
                         text_size = NULL,
                         text_color = NULL,
                         alignment = NULL ) {

  content = list(
    x = x, y = y,
    text = text,
    dimensions = dimensions,
    shape = shape,
    spacing = spacing,
    border_color = border_color,
    border_width = border_width,
    node_color = node_color,
    text_size = text_size,
    text_color = text_color,
    alignment = alignment
  )

  return( content )
}

