# Warning: doesn't capture promises
capture_environment <- function(except = NULL) {

  out <- as.list(parent.frame())
  out[!names(out) %in% except]

}

`%or%` <- function(x, y) if (length(x) == 0) y else x


opt <- function(option, default = NULL) {
  getOption(paste0("fretboardr.", option), default = default)
}

default_args <- function(fun) {
  fun |> 
    formals() |> 
    purrr::keep(~ !is_missing(.x)) |> 
    lapply(eval)
}

dput_chr <- function(x) {
  con <- textConnection("out", "w")
  dput(x, con)
  close(con)
  out
}
