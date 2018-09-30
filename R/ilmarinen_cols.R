#' Function to extract Ilmarinen colors as hex codes
#'
#' @param ... Character names of ilmarinen_colors
#' @export

ilmarinen_cols <- function(...) {

  # Ilmarinen corporate colors
  ilmarinen_colors <- c(
    `dark blue` = "#00274B",
    `pink` = "#F37B8A",
    `light blue` = "#84D2DF",
    `yellow` = "#FFE180",
    `orange` = "#DE6328",
    `blue` = "#3A8DA9",
    `grey` = "#A6A6A6")

  cols <- c(...)

  if (is.null(cols))
    return (ilmarinen_colors)

  ilmarinen_colors[cols]
}

#' Return function to interpolate Ilmarinen color palette
#'
#' @param palette Character name of palette in ilmarinen_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export

ilmarinen_pal <- function(palette = "main", reverse = FALSE, ...) {

  # Ilmarinen palettes
  ilmarinen_palettes <- list(
    `main`  = ilmarinen_cols("dark blue", "pink", "light blue", "yellow"),
    `cool`  = ilmarinen_cols("dark blue", "blue", "light blue"),
    `hot`   = ilmarinen_cols("orange", "pink", "yellow"),
    `mixed` = ilmarinen_cols("light blue", "yellow", "pink"),
    `all`   = ilmarinen_cols("dark blue", "pink", "light blue", "yellow", "orange", "blue", "grey")
  )


  pal <- ilmarinen_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export

scale_color_ilmarinen <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ilmarinen_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("ilmarinen_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for Ilmarinen colors
#'
#' @param palette Character name of palette in ilmarinen_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export

scale_fill_ilmarinen <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ilmarinen_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("ilmarinen_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
