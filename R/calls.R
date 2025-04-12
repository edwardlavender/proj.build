#' @title Function call properties
#' @description These functions collate information related to function calls.
#' @param .time,.form,.fun,.start,.end,... Arguments.
#' @author Edward Lavender
#' @name calls

#' @rdname calls
#' @export

# Format a time for cat()
call_time <- function(.time, .form = "%Y-%m-%d %H:%M:%S") {
  format(.time, .form)
}

#' @rdname calls
#' @export

# patter::foo() start statement
call_start <- function(.fun = as.character(sys.call(-1L)), .start = Sys.time()) {
  .fun  <- .fun[1]
  .start <- call_time(.start)
  glue::glue("`patter::{.fun}()` called @ {.start}...",
             .envir = environment())
}

#' @rdname calls
#' @export

# patter::foo() end statement
call_end <- function(.fun = as.character(sys.call(-1L)), .start, .end = Sys.time()) {
  .fun   <- .fun[1]
  .start <- call_time(.start)
  .end   <- call_time(.end)
  glue::glue("`patter::{.fun}()` call ended @ {.end} (duration: ~{call_duration(.start, .end)}).",
             .envir = environment())
}

#' @rdname calls
#' @export

# Format call duration (difftime) statement for cat()
call_duration <- function(.start, .end, ...) {
  # check_dots_used: difftime() used
  dft      <- difftime(.end, .start, ...)
  duration <- round(as.numeric(dft), digits = 2)
  units    <- attr(dft, "units")
  units    <- sub("s$", "", units)
  units    <- paste0(units, "(s)")
  glue::glue("{duration} {units}",
             .envir = environment())
}

#' @rdname calls
#' @export

# Record call timings
call_timings <- function(.start, .end = Sys.time(), ...) {
  # check_dots_used: difftime() used
  rlang::check_installed("data.table")
  data.table::data.table(start = .start,
                         end = .end,
                         duration = difftime(.end, .start, ...))
}
