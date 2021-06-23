#' Create Base Figure for Diagram
#'
#' A function that generates a base figure,
#' with optional guidelines, for a diagram.
#'
#' @param default Pre-packaged figure sizes, including...
#'   \itemize{
#'     \item 'US letter' (11 x 8.5 inches);
#'     \item '4.6' (4.6 x 4.6 inches);
#'     \item '9.2 x 4.6' (in inches).
#'   }
#' @param w Width (in inches) for the x-axis.
#' @param h Height (in inches) for the y-axis.
#' @param orientation For default specifications, indicates
#'   whether figures should be in \code{landscape} or
#'   \code{portrait} style.
#' @param margin A vector with the margins (in inches) for
#'   the bottom, left, top, and right, respectively.
#' @param guidelines Logical; if \code{TRUE}, includes
#'   guidelines when generating the figure.
#' @param guide_major Vector giving the major guideline
#'   positions (values must be between 0 and 1).
#' @param guide_minor Vector giving the minor guideline
#'   positions (values must be between 0 and 1).
#'   If \code{NULL}, no minor guidelines are included.
#' @param guide_adjust An adjustment controlling the
#'   position on the axes for the guideline numbers.
#' @param new Logical; if \code{TRUE} a new plotting
#'   window is generated.
#'
#' @examples
#' # Default (6 x 6 inches)
#' create_base_figure()
#'
#' # No guidelines
#' create_base_figure( guidelines = FALSE )
#'
#' # US letter size
#' create_base_figure( default = 'US letter' )
#'
#' # US letter (Portrait)
#' create_base_figure( default = 'US letter', orientation = 'portrait' )
#'
#' @export

create_base_figure = function( default = NULL,
                               w = 6, h = 6,
                               orientation = 'landscape',
                               margin = rep( .25, 4 ),
                               guidelines = T,
                               guide_major = seq( .1, .9, .1 ),
                               guide_minor = seq( .05, .95, .1 ),
                               guide_adjust = 1.25,
                               new = F ) {

  if ( !is.null( default ) ) {

    if ( default %in% 'US letter' ) {
      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        w = 11
        h = 8.5
      }
      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        w = 8.5
        h = 11
      }
    }

    if ( default %in% c( '4.6 x 4.6 in', '4.6' ) ) {
      w = 4.6
      h = 4.6
    }

    if ( default %in% '9.2 x 4.6' ) {
      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        w = 9.2
        h = 4.6
      }
      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        w = 4.6
        h = 9.2
      }
    }

  }

  # Create plotting window
  if ( new ) x11( width = w, height = h )

  # Specify margins
  par( mai = margin )

  # Create blank plot
  plot(
    c( 0, 1 ), c( 0, 1 ),
    xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '',
    bty = 'n', type = 'n'
  )

  if ( guidelines ) {

    plot_range = list(
      start = rep( 0, length( guide_major ) ),
      end = rep( 1, length( guide_major ) )
    )

    # Grid lines
    segments(  guide_major,
               plot_range$start,
               guide_major,
               plot_range$end,
               col = 'grey85' )
    segments(  plot_range$start,
               guide_major,
               plot_range$end,
               guide_major,
               col = 'grey85' )

    if ( !is.null( guide_minor ) ) {

      plot_range = list(
        start = rep( 0, length( guide_minor ) ),
        end = rep( 1, length( guide_minor ) )
      )

      # Grid lines
      segments(  guide_minor,
                 plot_range$start,
                 guide_minor,
                 plot_range$end,
                 col = 'grey85',
                 lty = 3 )
      segments(  plot_range$start,
                 guide_minor,
                 plot_range$end,
                 guide_minor,
                 col = 'grey85',
                 lty = 3 )

    }

    # Outer margins
    abline( v = 0, xpd = NA )
    abline( h = 0, xpd = NA )
    abline( v = 1, xpd = NA )
    abline( h = 1, xpd = NA )

    # Plotting dimensions

    # Plot window
    dm_outer = dev.size(); names( dm_outer ) = c( 'x', 'y' )
    # Margins
    dm_margins = par( 'mai' )
    # Plotting window
    dm_inner = c(
      dm_outer[1] - dm_margins[2] - dm_margins[4],
      dm_outer[2] - dm_margins[1] - dm_margins[3]
    )

    text(
      guide_major,
      rep( ( dm_outer[2] - guide_adjust * dm_margins[3] ) / dm_inner[2],
           length( guide_major ) ),
      guide_major,
      xpd = NA
    )

    text(
      rep( 1 - ( dm_outer[1] - guide_adjust * dm_margins[2] ) / dm_inner[1],
           length( guide_major ) ),
      guide_major,
      guide_major,
      xpd = NA
    )

  }

  # Reset margins
  par( mar = c( 5, 4, 4, 2 ) + .1 )
}

