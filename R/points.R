points <- function(...,
                   frets = 5,
                   strings = 6,
                   auto_open = TRUE,
                   flip = c("none", "x", "y", "both"),
                   rotate = FALSE,
                   debug = FALSE) {
  
  flip <- rlang::arg_match(flip)

  flip <- list(
    x = flip %in% c("x", "both"),
    y = flip %in% c("y", "both")
  )

  if (rotate) {
    flip$y <- `!`(flip$y)
  }

  dots <- list(...)
  stopifnot("Arguments in `...` must not be named" = is.null(names(dots)))

  data <- combine_points(...)

  if (auto_open) {
    open_strings <- dplyr::tibble(
      str = setdiff(seq_len(strings), data$str),
      fr = -1,
      label = "",
      shape = "circle open",
      colour = "black",
      fill = "white",
      size = 3,
      multi_string = FALSE,
      id = max(data$id %or% 0) + 1
    )
    data <- dplyr::bind_rows(data, open_strings)
  }

  if (debug) print(data)

  data <- data |>
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

    # Single-string markers
    geom_point(
      aes(
        x = if (rotate) fr_midpoint else str,
        y = if (rotate) str         else fr_midpoint,
        shape = shape, size = size, colour = colour, fill = fill
      ),
      data = data
    ),

    # Single-string labels
    geom_text(
      aes(
        x = if (rotate) fr_midpoint else str,
        y = if (rotate) str         else fr_midpoint,
        label = label
      ),
      data = data |> dplyr::filter(!multi_string),
      colour = "white", family = "Roboto Mono"
    ),

    # Multi-string markers
    geom_path(
      aes(
        x = if (rotate) fr_midpoint else str,
        y = if (rotate) str         else fr_midpoint,
        size = size * 0.9, colour = colour, group = id
      ),
      data = data |> dplyr::filter(multi_string),
      linejoin = "round", lineend = "round"
    ),

    # Multi-string labels
    geom_text(
      aes(
        x = if (rotate) fr_midpoint else str,
        y = if (rotate) str         else fr_midpoint,
        label = label
      ),
      data = data |>
        dplyr::filter(multi_string) |>
        dplyr::group_by(across(-str)) |>
        dplyr::summarise(str = mean(str), .groups = "drop"),
      colour = "white", family = "Roboto Mono"
    ),

    # Scales
    scale_shape_identity(),
    scale_colour_identity(),
    scale_fill_identity()
  ))

}

combine_points <- function(...) {
  
  empty <- ...length() == 0
  dots  <- if (!empty) list(...) else list(thmb(1, 1))
  
  dots |>
    purrr::map(dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "id") |>
    dplyr::mutate(id = as.integer(id)) |> 
    dplyr::filter(!empty)
  
}
