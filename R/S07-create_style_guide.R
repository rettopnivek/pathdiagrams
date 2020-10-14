#' Create a Style Guide to Change Global Diagram Defaults
#'
#' Creates a list called \code{stye_guide} in the global
#' environment that governs the defaults for
#' \code{\link{add_node}}), \code{\link{add_line}}), and
#' \code{\link{add_arrow}}). Allows widespread changes
#' to the appearance of nodes, lines, and arrows.
#'
#' @param node.shape Specify the default shape for nodes.
#' @param node.spacing Specify the default spacing between
#'   text lines in nodes.
#' @param node.border_color Specify the default line color
#'   for node borders.
#' @param node.border_width Specify the default line
#'   width for node borders.
#' @param node.node_color Specify the default fill color
#'   for nodes.
#' @param node.text_size Specify the default text size
#'   for nodes.
#' @param node.text_color Specify the default text color
#'   for nodes.
#' @param node.alignment Specify the default text
#'   alignment for nodes.
#' @param line.col Specify the default line
#'   color for lines between nodes.
#' @param line.lwd Specify the default line width
#'   for lines between nodes.
#' @param arrow.col Specify the default line
#'   color for arrows.
#' @param arrow.lwd Specify the default line
#'   width for arrows.
#' @param arrow.length Specify the default
#'   length of the arrow heads.
#'
#' @return Creates a named list called \code{style_guide}
#'   in the global environment.
#'
#' @examples
#' # Two-panel figure
#' layout( cbind( 1, 2 ) )
#'
#' # Standard defaults
#' create_base_figure( guidelines = F, new = F )
#' nd = add_node( node_content( text = 'Hello world' ) )
#' add_line( start = nd$bottom, end = c( .5, .1 ) )
#'
#' create_style_guide( node.text_size = 2, line.lwd = 3 )
#' create_base_figure( guidelines = F, new = F )
#'
#' nd = add_node( node_content( text = 'Hello world' ) )
#' add_line( start = nd$bottom, end = c( .5, .1 ) )
#'
#' @export

create_style_guide = function( node.shape = 'rectangle',
                               node.spacing = 0.5,
                               node.border_color = 'black',
                               node.border_width = 2,
                               node.node_color = 'white',
                               node.text_size = 1,
                               node.text_color = 'black',
                               node.alignment = 'center',
                               line.col = 'black',
                               line.lwd = 2,
                               arrow.col = 'black',
                               arrow.lwd = 2,
                               arrow.length = .25 ) {


  style_guide <<- list(
    node = list(
      shape = node.shape,
      spacing = node.spacing,
      border_color = node.border_color,
      border_width = node.border_width,
      node_color = node.node_color,
      text_size = node.text_size,
      text_color = node.text_color,
      alignment = node.alignment
    ),
    line = list(
      col = line.col,
      lwd = line.lwd
    ),
    arrow = list(
      col = arrow.col,
      lwd = arrow.lwd,
      length = arrow.length
    )
  )

}
