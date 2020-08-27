#' Create asciiart image
#'
#' Creates an ASCII-art/text-art image where pixel intensity is mapped to the density of character glyphs
#'
#' A uniform grid image can be written to a device (png, pdf etc...) and an approximation of the same image
#' can be printed to the console (or the output of calling \code{asciiart()} could be written to a text
#' file with \code{readr::write_lines()}). The text written to the console by setting \code{print_text = TRUE} will not
#' maintain the exact same aspect ratio as the original image
#'
#' @param file Path to image file or array returned from \code{jpeg::readJPEG(), png::read_PNG()}
#' @param width Width of output image in text characters. Use NA for original image width
#' @param text_scaling Text scaling factor for tweaking text size (default = 1)
#' @param out_width Output image width in inches
#' @param out_name File path including name for saved image
#' @param chars Character vector set for ascii art (ordered from dark to light)
#' @param text_col Colour of text (default = "black")
#' @param print_text Logical. Should the text making the picture by printed to the console?
#'   This is hacky, every other row is removed from the image to roughly correct for vertical stretching. This
#'   text output will not have the same aspect ratio as the original image
#'
#' @export
asciiart <- function(file,
                     width = 80,
                     text_scaling = 1,
                     out_width = 8,
                     out_name = NULL,
                     text_col = "black",
                     chars = c("@","%","#","*","+","=","-",":","."," "),
                     print_text = FALSE
                     ){

  if(is.matrix(file) | is.array(file)){
    raw <- file
  } else {
    if(grepl('.png', file)) raw <- png::readPNG(file)
    if(grepl('.jpg|.jpeg', file)) raw <- jpeg::readJPEG(file)
  }

  # Extract dimensions of image
  dim_y <- dim(raw)[1]
  dim_x <- dim(raw)[2]

  # Convert image to greyscale vector
  # If it's rgb, average first
  if(length(dim(raw)) == 3){
    # If three dimensions, average r, g and b
    g <- rowMeans(raw, dims=2) %>% as.vector()
  } else if(length(dim(raw)) == 2){
    g <- as.vector(raw)
  }

  # Create aspect ratio
  asp <- dim_y/dim_x

  # Compute height in text characters that maintains original aspect
  if(is.na(width)){width <- dim_x}
  height <- asp * width

  # Compute dataframe
  d <-
    expand.grid(y = dim_y:1, x = 1:dim_x) %>%
    dplyr::mutate(g = g) %>%
    dplyr::mutate(xbin = cut(x, width, labels = FALSE, include.lowest = T),
                  ybin = cut(y, height, labels = FALSE, include.lowest = T)) %>%
    dplyr::group_by(xbin, ybin) %>%
    dplyr::summarise(g = mean(g), .groups = "drop") %>%
    dplyr::mutate(gbin = cut(g, length(chars), labels=FALSE),
                  char = chars[gbin])

  p <-
    d %>%
    ggplot2::ggplot(ggplot2::aes(xbin, ybin))+
    # geom_raster(aes(fill=g))+
    ggplot2::geom_text(ggplot2::aes(label = char),
                       size = (((out_width*asp)/height)/(1/72)) * 0.35 * text_scaling,
                       col = text_col)+
    ggplot2::coord_equal()+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "")

  print(p)

  if(!is.null(out_name)) ggplot2::ggsave(out_name, plot = p, width=out_width, height=out_width*asp)

  # Write text file of ascii art
  # TODO
  if(print_text){
    t <-
      d %>%
      # Filter every other row to stop vertical stretching in text
      # This is a hack and not precise (so text output does not have exactly the same aspect as original)
      dplyr::filter(!ybin %% 2 == 0) %>%
      dplyr::arrange(desc(ybin), xbin) %>%
      dplyr::group_by(ybin) %>%
      dplyr::summarise(line = paste0(char, collapse=""), .groups="drop") %>%
      dplyr::arrange(desc(ybin)) %>%
      dplyr::pull(line)

    return(t)
  }

  }
