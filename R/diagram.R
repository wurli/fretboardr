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
#' 
#' @examples
#' diagram(
#'   "F# (Henrix Style)",
#'   thmb(1, 2),
#'   ring(2, 4),
#'   pnky(3, 4),
#'   mddl(4, 3),
#'   barr(5:6, 2, "I"),
#'   rotate = TRUE
#' )
diagram <- function(title = NULL,
                    ...,
                    frets = NULL,
                    strings = c("E1", "A", "D", "G", "B", "E2"),
                    flip = c("none", "x", "y", "both"),
                    rotate = FALSE,
                    auto_open = TRUE,
                    label_strings = FALSE,
                    debug = FALSE) {
  
  labs_provided <- is.character(strings)
  
  if (is.numeric(strings)) {
    stopifnot(length(strings) == 1)
    strings <- seq_len(strings) |> as.character()
  } 
  
  strings  <- factor(strings, levels = strings)
  flip     <- get_flip(flip[1], rotate)
  n_points <- ...length() 
  points   <- list(...) |> 
    validate_string_values(strings, label_strings, labs_provided) |> 
    combine_points()
  
  if (n_points == 0) {
    frets <- frets %||% 5
  }
  
  frets <- frets %||% max(max(points$fr) + 2, 5)
  
  invalid_frets <- points$fr[!points$fr %in% seq_len(frets)]
  
  if (length(invalid_frets) > 0) {
    cli::cli_abort(c(
      "Invalid fret specification",
      i = "Check {unique(invalid_frets)}"
    ))
  }

  fretboard(
    title = title,
    frets = frets,
    strings = strings,
    label_strings = label_strings,
    flip = flip,
    rotate = rotate
  ) +
    layer_points(
      data = points,
      frets = frets,
      strings = strings,
      auto_open = auto_open,
      flip = flip,
      rotate = rotate,
      debug = debug
    )

}



combine_points <- function(points) {
  
  has_length <- length(points) != 0
  points     <- points %or% list(thmb(1, 1))
  
  points |>
    purrr::map(dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "id") |>
    dplyr::mutate(id = as.integer(id)) |> 
    dplyr::filter(has_length)
  
}


validate_string_values <- function(points, strings, label_strings, labs_provided) {
  
  caller_envir <- caller_env()
  
  character_vals <- points |> 
    purrr::map("str") |> 
    purrr::keep(is.character) |> 
    purrr::flatten()
  
  any_character_vals <- length(character_vals) > 0
  
  # Error if user hasn't specified string names and they're needed
  if (label_strings && !labs_provided) {
    cli::cli_abort(
      c("String labels not provided. Please use a character vector for `strings`"),
      call = caller_envir
    )
  }
  
  # Format all points as factors. Also check that points don't lie on 
  # non-existent strings
  if (labs_provided) {
    points <- points |> 
      purrr::map(purrr::modify_at, "str", function(s) {
        
        out <- if (is.numeric(s)) strings[s] else factor(s, levels = levels(strings))
        
        if (any(is.na(out))) {
          opts <- if (is.numeric(s)) as.integer(strings) else levels(strings)
          cli::cli_abort(
            c("Invalid string {cli::qty(length(s))} value{?s} {s}",
              i = "Valid options are {opts}"),
            call = caller_envir
          )
        }
        
        out
        
      })
  }
  
  points
  
}
