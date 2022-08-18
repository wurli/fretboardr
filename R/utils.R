# Warning: doesn't capture promises
capture_environment <- function(except = NULL) {

  out <- as.list(parent.frame())
  out[!names(out) %in% except]

}
