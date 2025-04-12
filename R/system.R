#' @title Operating systems
#' @name os
NULL

#' @rdname os
#' @export

os_unix <- function() {
  .Platform$OS.type == "unix"
}

#' @rdname os
#' @export

os_windows <- function() {
  .Platform$OS.type == "windows"
}

#' @rdname os
#' @export

os_linux <- function() {
  grepl("linux", tolower(Sys.info()["sysname"]))
}
