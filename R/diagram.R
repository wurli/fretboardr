#' Make a fretboard diagram
#'
#' @param title The title for the diagram
#' @param ... Points to place on the fretboard - constructed using calls to 
#'   `fngr()`, `thmb()`, `barr()` and `mute()`
#' @param frets The number of frets to show
#' @param strings The number of strings to show
#' @param flip Mirroring of the output - either `"none"`, `"x"`, `"y"` or
#'   `"both"`
#' @param rotate If `TRUE` the fretboard will be shown in a horizontal layout
#' @param auto_open If `TRUE`, strings which don't have any markers specified
#'   will be marked as 'open'.
#' @param debug If `TRUE` some diagnostic stuff will be shown.
#'
#' @return A ggplot
#' @export
diagram <- function(title = NULL,
                    ...,
                    frets = NULL,
                    strings = 6,
                    flip = c("none", "x", "y", "both"),
                    rotate = FALSE,
                    auto_open = TRUE,
                    debug = FALSE) {

  points <- combine_points(...)
  frets <- frets %||% max(max(points$fr) + 2, 5)
  
  stopifnot(all(1 <= points$str & points$str <= strings))

  fretboard(
    title = title,
    frets = frets,
    strings = strings,
    flip = flip,
    rotate = rotate
  ) +
    points(
      ...,
      frets = frets,
      strings = strings,
      auto_open = auto_open,
      flip = flip,
      rotate = rotate,
      debug = debug
    )

}
