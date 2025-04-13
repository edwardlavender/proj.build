#' @title Time helpers
#' @param time1,time2 Arguments passed to [`difftime()`].
#' @export

secs <- function(time1, time2) {
  as.numeric(difftime(time1, time2), units = "secs")
}