#' @title `sink()` functions
#' @param log.txt (optional) A `character` string that specifies the path to a `log.txt` file.
#' @name sinks

#' @rdname sinks
#' @export

sink_open <- function(log.txt = NULL) {
  if (!is.null(log.txt)) {
    # Define connection 
    log.txt <- file(log.txt, open = "wt")
    # Open sink
    sink(log.txt, append = TRUE)
    sink(log.txt, type = "message", append = TRUE)
    # Print start time
    cat_time()
  }
  invisible(log.txt)
}

#' @rdname sinks
#' @export

sink_close <- function(log.txt = NULL) {
  if (!is.null(log.txt)) {
    cat_time()
    sink()
    sink(type = "message")
    close(log.txt)
  }
  invisible(NULL)
}