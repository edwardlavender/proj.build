#' @title `cat` functions for iterative workflows
#' @param .index,.verbose,... Arguments. 
#' @details
#' These functions were motivated by the `patter.workflows` package.
#' @name cats_iter

# Print the time 
cat_time <- function() {
  cat(paste0(Sys.time(), "\n"))
  nothing()
}

#' @rdname cats_iter
#' @export

# Print a line 
cat_line <- function() {
  cat("\n\n\n---------------------------------------------------------------\n")
  nothing()
}

#' @rdname cats_iter
#' @export

# Print the row id 
cat_row <- function(.index) {
  cat(paste0("\n On row ", .index, "...\n"))
  nothing()
}

#' @rdname cats_iter
#' @export

# Initialise cat for a given iteration
cat_next <- function(.index, .verbose) {
  if (.verbose) {
    cat_line()
    cat_row(.index)
  }
  nothing()
}

#' @rdname cats_iter
#' @export

# Optionally run additional cat() calls
cat_do <- function(..., .verbose) {
  if (.verbose) {
    do.call(cat, list(...))
  }
  nothing()
}