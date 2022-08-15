points <- function(..., frets = 12, auto_open = TRUE, debug = FALSE) {

  data <- list(...) %>%
    purrr::map(dplyr::as_tibble) %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::mutate(id = as.integer(id))

  if (auto_open) {
    open_strings <- dplyr::tibble(
      str = setdiff(1:6, data$str),
      fr = -1,
      label = "",
      shape = "circle open",
      colour = "black",
      fill = "white",
      size = 3,
      type = "single",
      id = max(data$id) + 1
    )
    data <- dplyr::bind_rows(data, open_strings)
  }

  if (debug) print(data)

  data <- data %>%
    dplyr::left_join(
      dplyr::bind_rows(
        dplyr::tibble(
          fr = -1,
          fr_pos = (fct ^ 13) * 30,
          fr_midpoint = 61
        ),
        dplyr::tibble(
          fr = seq(0, frets, by = 1),
          fr_pos = (fct ^ (12 - fr)) * 30,
          fr_midpoint = ifelse(
            fr == 0, fr_pos, (dplyr::lag(fr_pos) + fr_pos) / 2
          )
        )
      ),
      by = "fr"
    )

  invisible(list(
    ggplot2::geom_point(
      ggplot2::aes(str, fr_midpoint, shape = shape, size = size, colour = colour, fill = fill),
      data = data
    ),
    ggplot2::geom_text(
      ggplot2::aes(y = fr_midpoint, label = label),
      data = data %>% dplyr::filter(type == "single"),
      colour = "white", family = "Roboto Mono"
    ),
    ggplot2::geom_path(
      ggplot2::aes(str, fr_midpoint, size = size * 0.9, colour = colour, group = id),
      data = data %>% dplyr::filter(type == "barre"),
      linejoin = "round", lineend = "round"
    ),
    ggplot2::geom_text(
      ggplot2::aes(y = fr_midpoint, label = label),
      data = data %>%
        dplyr::filter(type == "barre") %>%
        dplyr::group_by(across(-str)) %>%
        dplyr::summarise(str = mean(str), .groups = "drop"),
      colour = "white", family = "Roboto Mono"
    ),
    ggplot2::scale_shape_identity(),
    ggplot2::scale_colour_identity(),
    ggplot2::scale_fill_identity()
  ))

}
