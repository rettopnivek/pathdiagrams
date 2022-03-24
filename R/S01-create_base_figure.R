#' Create Base Figure for Diagram
#'
#' A function that generates a base figure,
#' with optional guidelines, for a diagram.
#'
#' @param default Pre-packaged figure sizes, including...
#'   \itemize{
#'     \item 'US letter' or '8.5 x 11' (inches);
#'     \item '3.54 x 3.54' (inches);
#'     \item '5 x 5' (inches);
#'     \item '7.25 x 5' (inches).
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
                               guidelines = TRUE,
                               guide_major = seq( .1, .9, .1 ),
                               guide_minor = seq( .05, .95, .1 ),
                               guide_adjust = 1.25,
                               new = FALSE ) {

  # If a default argument is provided
  if ( !is.null( default ) ) {

    # US letter
    if ( default %in% c( 'US letter',
                         '8.5 x 11 in', '8.5 x 11' ) ) {

      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        w = 11
        h = 8.5
      }
      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        w = 8.5
        h = 11
      }

      # Close 'US letter'
    }

    # Science guidelines for figure sizes (small)
    if ( default %in% c( '3.54 x 3.54 in', '3.54 x 3.54', '3.54',
                         '9 x 9 cm', '9' ) ) {

      w = 3.54
      h = 3.54

      # Close 'Science guidelines for figure sizes (small)'
    }

    # Science guidelines for figure sizes (medium)
    if ( default %in% c( '5 x 5 in', '5 x 5', '5',
                         '12.7 x 12.7 cm', '12.7' ) ) {

      w = 5
      h = 5

      # Close 'Science guidelines for figure sizes (medium)'
    }

    # Science guidelines for figure sizes (wide)
    if ( default %in% c( '7.25 x 5 in', '7.25 x 5', '7.25',
                         '18.4 x 12.7 cm', '18.4 x 12.7', '18.4' ) ) {

      if ( orientation %in% c( 'Landscape', 'landscape', 'wide', 'Wide' ) ) {
        w = 7.25
        h = 5
      }

      if ( orientation %in% c( 'Portrait', 'portrait', 'Tall', 'tall' ) ) {
        w = 5
        h = 7.25
      }

      # Close 'Science guidelines for figure sizes (wide)'
    }

    # Close 'If a default argument is provided'
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

  # Add guidelines to figure
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

    # Lines at minor tick positions
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

      # Close 'Lines at minor tick positions'
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

    # Close 'Add guidelines to figure'
  }

  # Reset margins
  par( mar = c( 5, 4, 4, 2 ) + .1 )
}

