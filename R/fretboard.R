#' Plot a fretboard
#'
#' This provides the base for each new chord chart
#'
#' @param title A title for the chord chart
#' @param frets The number of frets to include
#' @param strings The number of strings to include
#'
#' @return
fretboard <- function(title = NULL, frets = 12, strings = 6, flip = c("none", "x", "y", "all"), rotate = FALSE) {

  flip <- match.arg(flip)

  flip <- list(
    x = flip %in% c("x", "all"),
    y = flip %in% c("y", "all")
  )

  if (rotate) {
    flip$y <- `!`(flip$y)
  }

  if (is.numeric(strings)) {
    str         <- seq_len(strings)
    axis_text_x <- ggplot2::element_blank()
    x_scale     <- ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(add = 2),
      trans = if (flip$x) "reverse" else "identity"
    )
  } else {
    str         <- seq_along(strings)
    axis_text_x <- ggplot2::element_text()
    x_scale     <- ggplot2::scale_x_discrete(
      limits = strings,
      expand = ggplot2::expansion(add = 2),
      trans = if (flip$x) "reverse" else "identity"
    )
  }

  data <- dplyr::tibble(
    str = str,
    str_size = seq(1, 0.5, length.out = length(str))
  ) %>%
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
    ) %>%
    dplyr::mutate(
      marker_1 = fr %in% c(3, 5, 7, 9),
      marker_2 = fr %in% c(12),
      nut = fr == 0
    )

  data %>%
    ggplot2::ggplot(ggplot2::aes(if (rotate) fr_pos else str, if (rotate) str else fr_pos)) +
    ggplot2::geom_line(ggplot2::aes(group = fr), size = 0.8, colour = "grey50") +
    ggplot2::geom_line(ggplot2::aes(group = str, size = str_size), lineend = "round") +

    # Single-point for fret marker 1
    ggplot2::geom_point(
      data = . %>%
        dplyr::mutate(str = 0.3, fr_pos = fr_midpoint) %>%
        dplyr::filter(marker_1),
      size = 0.8, colour = "grey30"
    ) +
    # Double-point for fret marker 2
    ggplot2::geom_point(
      data = . %>%
        dplyr::mutate(str = 0.3, fr_pos = fr_midpoint + 0.3 * c(-1, 1)) %>%
        dplyr::filter(marker_2),
      size = 0.8, colour = "grey30"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = fr),
      data = . %>% dplyr::filter(nut),
      size = 3, lineend = "round"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = fr),
      data = . %>%
        dplyr::mutate(str = -0.2, fr_pos = fr_midpoint) %>%
        dplyr::filter(marker_1 | marker_2),
      family = "Roboto Mono", hjust = 1
    ) +
    ggplot2::scale_y_continuous(trans = if (flip$y) "reverse" else "identity") +
    x_scale +
    ggplot2::theme_void() +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      text = ggplot2::element_text("Roboto Mono"),
      plot.title = ggplot2::element_text(hjust = 0.56, size = 20),
      axis.text.x = axis_text_x
    ) +
    ggplot2::scale_size_identity()

}


fngr <- function(str, fr, label = "", shape = "circle", colour = "black", fill = "black", size = 5) {

  if (is.character(str)) {
    opts <- c("E1", "A", "D", "G", "B", "E2")
    stopifnot(str %in% opts)
    str <- which(str == opts)
  }

  stopifnot(
    length(str) == 1,
    length(fr) == 1,
    str %in% 1:6,
    fr >= 0,
    shape %in% c(
      "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
      "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
      "diamond", paste("diamond", c("open", "filled", "plus")),
      "triangle", paste("triangle", c("open", "filled", "square")),
      paste("triangle down", c("open", "filled")),
      "plus", "cross", "asterisk"
    )
  )

  list(str = str, fr = fr, label = label, shape = shape, colour = colour, fill = fill, size = size, type = "single")

}

thumb <- function(str, fr, label = "T", shape = "diamond", colour = "black", fill = "black", size = 7) {

  out <- fngr(str = str, fr = fr, label = label, shape = shape, colour = colour, fill = fill, size = size)

  if (!out$str %in% 1:2) {
    warning("Only freaks can fret string #", out$str, " with their thumbs", call. = FALSE)
  }

  out
}

barre <- function(str, fr, label = "", shape = "circle", colour = "black", fill = "black", size = 5) {

  if (is.character(str)) {
    opts <- c("E1", "A", "D", "G", "B", "E2")
    stopifnot(all(str %in% opts))
    str <- purrr::map_int(str, ~which(. == opts))
  }

  stopifnot(
    length(fr) == 1,
    all(str %in% 1:6),
    fr >= 0,
    shape %in% c(
      "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
      "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
      "diamond", paste("diamond", c("open", "filled", "plus")),
      "triangle", paste("triangle", c("open", "filled", "square")),
      paste("triangle down", c("open", "filled")),
      "plus", "cross", "asterisk"
    )
  )
  list(str = str, fr = fr, label = label, shape = shape, colour = colour, fill = fill, size = size, type = "barre")
}

mute <- function(str, shape = "cross", colour = "black", fill = "black", size = 3) {

  if (is.character(str)) {
    opts <- c("E1", "A", "D", "G", "B", "E2")
    stopifnot(str %in% opts)
    str <- which(str == opts)
  }

  stopifnot(
    length(str) == 1,
    str %in% 1:6,
    shape %in% c(
      "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
      "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
      "diamond", paste("diamond", c("open", "filled", "plus")),
      "triangle", paste("triangle", c("open", "filled", "square")),
      paste("triangle down", c("open", "filled")),
      "plus", "cross", "asterisk"
    )
  )

  list(str = str, fr = -1, label = "", shape = shape, colour = colour, fill = "black", size = 3, type = "single")

}
