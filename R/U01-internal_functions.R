# Specify Node Aesthetics
#
# Internal function to match the contents
# of a string input to a given tag and
# extract the value used when specifying
# aesthetic options.
#
# param tag: A character string.
# param input_parts: A vector of character
#   strings after splitting the user-supplied
#   input for a node or path by the "|" symbol
# param lod: A list of defaults for options.
#
# return: The value extracted from the
#   relevant input part.

multiple_node_aes = function( tag,
                              input_parts,
                              lod ) {

  # Default output

  # Node shape
  if ( tag == 'ns=' ) out = lod$shape
  if ( tag == 'nc=' ) out = lod$shape.col
  if ( tag == 'nb=' ) out = lod$shape.border
  if ( tag == 'nw=' ) out = lod$shape.lwd
  if ( tag == 'nt=' ) out = lod$shape.lty
  if ( tag == 'np=' ) out = lod$shape.pad
  if ( tag == 'nx=' ) out = lod$shape.x
  if ( tag == 'ny=' ) out = lod$shape.y

  # Node text
  if ( tag == 'ts=' ) out = lod$text.size
  if ( tag == 'tc=' ) out = lod$text.col
  if ( tag == 'tf=' ) out = lod$text.font
  if ( tag == 'th=' ) out = lod$text.spacing

  # Path line
  if ( tag == 'lp=' ) out = lod$path.pad
  if ( tag == 'lw=' ) out = lod$path.lwd
  if ( tag == 'lc=' ) out = lod$path.col
  if ( tag == 'll=' ) out = lod$path.length
  if ( tag == 'la=' ) out = lod$path.angle
  if ( tag == 'lt=' ) out = lod$path.lty
  if ( tag == 'cd=' ) out = lod$path.code

  check = grepl( tag, input_parts, fixed = T )

  #< Check for any inputted tags
  if ( any( check ) ) {

    #<< Character output tags
    if ( tag %in% c( 'ns=', 'nc=', 'nb=', 'tc=', 'tf=',
                     'cd=', 'lc=' ) ) {

      val = input_parts[ check ]

      val = gsub( tag, '', val, fixed = T )

      if ( val == 'NA' ) val = NA

      out = val

      #>> Close conditional for node shape
    }

    #<< Numeric output tags
    if ( tag %in% c( 'nw=', 'nt=', 'ts=', 'lp=', 'lw=',
                     'll=', 'la=', 'lt=', 'th=', 'nx=',
                     'ny=', 'np=' ) ) {

      val = input_parts[ check ]

      val = gsub( tag, '', val, fixed = T )

      if ( val == 'NA' ) val = NA

      out = as.numeric( val )

      #>> Close conditional for node shape
    }

    #> Close conditional for any inputted tags
  }

  return( out )
}

# Add Bold or Italic Text to a Figure
#
# Internal function that adds bold or
# or italic text to an existing figure.
#
# param x: The x-axis coordinate at which
#   to draw the text.
# param y: The y-axis coordinate at which
#   to draw the text.
# param txt: The text content. Characters
#   embedded between '**' are bolded,
#   while those embedded between '*'
#   are italicized.
# param cex: The text size.
# param pos: The alignment of the text
#   (NULL is centered, 2 is left-aligned,
#   4 is right-aligned)
# param ...: Additional parameters for the
#   'text' function.

plain_bold_italic_text <- function( x, y, txt, cex = 1,
                                    pos = NULL, ... ) {

  # fin_txt <- gsub( '*', '', txt, fixed = T )
  # strwidth( fin_txt, cex = cex )

  each_elem <- strsplit( txt, split = '*', fixed = T )[[1]]
  each_elem[ each_elem != "" ]

  each_elem = each_elem[ each_elem != "" ]

  elem_type = rep( 1, length( each_elem ) )
  for ( i in 1:length( elem_type ) ) {


    if ( grepl( paste0( '*', each_elem[i] ), txt, fixed = T ) &
         !grepl( '* ', paste0( '*', each_elem[i] ), fixed = T ) ) {
      elem_type[i] = 3
    }

    if ( grepl( paste0( '**', each_elem[i] ), txt, fixed = T ) &
         !grepl( '** ', paste0( '**', each_elem[i] ), fixed = T ) ) {
      elem_type[i] = 2
    }

  }

  elem_sz = strwidth( each_elem, cex = cex )

  # half_elem_sz = elem_sz/2

  # pst <- rep( NA, length( elem_sz ) )
  pst = cumsum( elem_sz )
  if ( is.null( pos ) ) {
    x_pst = pst + x - sum( elem_sz )/2
  } else {
    if ( pos == 2 ) {
      x_pst = pst + x - sum( elem_sz ) - elem_sz[1]
    }
    if ( pos == 4 ) {
      x_pst = pst + x
    }
  }

  txt_h = strheight(each_elem,
                    cex = cex,
                    font = elem_type )
  y_pst = y - max( txt_h )/2

  for ( i in 1:length( each_elem ) ) {

    # Add text
    text( x_pst[i], y_pst + txt_h[i]/2,
          each_elem[i], cex = cex, pos = 2,
          font = elem_type[i], ... )

  }

}
