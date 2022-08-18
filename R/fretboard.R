#' Plot a fretboard
#'
#' This provides the base for each new chord chart
#'
#' @param title A title for the chord chart
#' @param frets The number of frets to include
#' @param strings The number of strings to include
#'
#' @return A ggplot
fretboard <- function(title = NULL,
                      frets = 5,
                      strings = 6,
                      flip = c("none", "x", "y", "both"),
                      rotate = FALSE) {

  flip <- rlang::arg_match(flip)

  flip <- list(
    x = flip %in% c("x", "both"),
    y = flip %in% c("y", "both")
  )

  if (rotate) {
    flip$y <- `!`(flip$y)
  }

  if (is.numeric(strings)) {
    str         <- seq_len(strings)
    axis_text_x <- element_blank()
    x_scale     <- scale_x_continuous(
      expand = expansion(add = 2),
      trans = if (flip$x) "reverse" else "identity"
    )
  } else {
    str         <- seq_along(strings)
    axis_text_x <- element_text()
    x_scale     <- scale_x_discrete(
      limits = strings,
      expand = expansion(add = 2),
      trans = if (flip$x) "reverse" else "identity"
    )
  }

  data <- dplyr::tibble(
    str = str,
    str_size = seq(1, 0.5, length.out = length(str))
  ) |>
    dplyr::left_join(
      dplyr::tibble(
        fr = seq(0, frets, by = 1),
        fr_pos = (fct ^ (12 - fr)) * 30,
        fr_midpoint = ifelse(
          fr == 0,
          fr_pos,
          (dplyr::lag(fr_pos) + fr_pos) / 2
        )
      ),
      by = character()
    ) |>
    dplyr::mutate(
      marker_1 = fr %in% c(3, 5, 7, 9),
      marker_2 = fr %in% c(12),
      nut = fr == 0
    )

  data |>
    ggplot(aes(if (rotate) fr_pos else str, if (rotate) str else fr_pos)) +
    geom_line(aes(group = fr), size = 0.8, colour = "grey50") +
    geom_line(aes(group = str, size = str_size), lineend = "round") +

    # Single-point for fret marker 1
    geom_point(
      data = ~ . |>
        dplyr::mutate(str = 0.3, fr_pos = fr_midpoint) |>
        dplyr::filter(marker_1),
      size = 0.8, colour = "grey30"
    ) +
    # Double-point for fret marker 2
    geom_point(
      data = ~ . |>
        dplyr::mutate(str = 0.3, fr_pos = fr_midpoint + 0.3 * c(-1, 1)) |>
        dplyr::filter(marker_2),
      size = 0.8, colour = "grey30"
    ) +
    geom_line(
      aes(group = fr),
      data = ~ . |> dplyr::filter(nut),
      size = 3, lineend = "round"
    ) +
    geom_text(
      aes(label = fr),
      data = ~ . |>
        dplyr::mutate(str = -0.2, fr_pos = fr_midpoint) |>
        dplyr::filter(marker_1 | marker_2),
      family = "Roboto Mono", hjust = 1
    ) +
    scale_y_continuous(trans = if (flip$y) "reverse" else "identity") +
    x_scale +
    theme_void() +
    coord_fixed() +
    labs(title = title) +
    theme(
      text = element_text("Roboto Mono"),
      plot.title = element_text(hjust = 0.56, size = 20),
      axis.text.x = axis_text_x
    ) +
    scale_size_identity()

}


#' Add points to a fretboard
#'
#' Aliases `fngr()`, `thmb()` and `barr()` are provided because code looks nicer
#' when things align.
#'
#' @param str The string(s) to put the point on
#' @param fr The fret to put the point on. Use `-1` to place a point behind the
#'   nut - this is common notation for muting.
#' @param label The label for the point
#' @param shape,colour,fill,size Used to change the appearance of the point -
#'   passed to `ggplot2::geom_point()`
#'
#' @return A named list
#' @export
#' @rdname points
fngr <- function(str, fr, label = "", shape = "circle", colour = "black", fill = "black", size = 5) {
  do.call(make_fretboard_marker, capture_environment())
}

#' @export
#' @rdname points
finger <- fngr

#' @export
#' @rdname points
thmb <- function(str, fr, label = "T", shape = "diamond", colour = "black", fill = "black", size = 5) {

  if (!out$str %in% 1:2) {
    cli::cli_warn("Only freaks can fret string #{out$str} with their thumbs")
  }

  do.call(make_fretboard_marker, capture_environment())

}

#' @export
#' @rdname points
thumb <- thmb

#' @export
#' @rdname points
barr <- function(str, fr, label = "", shape = "circle", colour = "black", fill = "black", size = 5) {
  multi_string <- TRUE
  do.call(make_fretboard_marker, capture_environment())
}

#' @export
#' @rdname points
barre <- barr

#' @export
#' @rdname points
mute <- function(str, fr = -1, shape = "cross", colour = "black", fill = "black", size = 5) {
  do.call(make_fretboard_marker, capture_environment())
}

make_fretboard_marker <- function(str, fr,
                                  label = "",
                                  shape = "cross",
                                  colour = "black",
                                  fill = "black",
                                  size = 5,
                                  multi_string = FALSE) {

  stopifnot(
    is.logical(multi_string),
    length(multi_string) == 1,
    length(str) == 1 | multi_string,
    length(shape) == 1,
    length(fr) == 1,
    length(label) == 1,
    fr >= -1
  )

  if (is.character(str)) {
    opts <- getOption("fretboardr.string_names") %||% c(
      "E1", "A", "D", "G", "B", "E2"
    )
    stopifnot(str %in% opts)
    str <- which(opts == str)
  }

  size <- switch(
    shape,
    diamond = size * 7 / 5,
    cross = size * 3 / 5,
    size
  )

  capture_environment(except = "opts")

}
