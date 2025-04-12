str_items <- function(.items, .quo = "`") {
  if (all(.items == "")) {
    rlang::warn("`.items` is an empty string ('').")
    out <- .items
  } else {
    collap <- paste0(.quo, ", ", .quo)
    out <- paste0(.quo, paste0(.items, collapse = collap), .quo)
  }
  out
}