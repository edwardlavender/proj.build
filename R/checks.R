#' @title `check` functions
#' @description These functions perform generic checks on user inputs.
#' @param input,action,ext,class,req,extract_names,type,arg,ignore_empty,data,new,not_allowed,args,dots,... Function arguments. 
#' @author Edward Lavender
#' @name checks
NULL

#' @rdname checks
#' @export

check_dir_exists <- function(input, action = abort) {
  check_inherits(input, "character")
  if (!dir.exists(input)) {
    action("The directory '{input}' does not exist.",
           .envir = environment())
  }
  invisible(input)
}

#' @rdname checks
#' @export

check_dir_empty <- function(input, action = abort) {
  if (!is.null(input)) {
    check_dir_exists(input)
    if (length(list.files(input) != 0L)) {
      action("The directory '{input}' is not empty.",
             .envir = environment())
    }
  }
  invisible(input)
}

#' @rdname checks
#' @export

check_dir_contents_ext <- function(input, ext, ...) {
  f    <- list.files(input, ...)
  fext <- unique(tools::file_ext(f))
  if (!all(fext %in% ext)) {
    abort("The directory '{input}' contains files with unexpected extensions.",
          .envir = environment())
  }
  invisible(input)
}

#' @rdname checks
#' @export

check_file_exists <- function(input, action = abort) {
  exist <- sapply(input, file.exists)
  if (any(!exist)) {
    action(str_items("These file(s) do not exist:", input[!exist]))
  }
  invisible(exist)
}

#' @rdname checks
#' @export

check_inherits <- function(input, class, action = abort) {
  if (!inherits(input, class)) {
    action("`{deparse(substitute(input))}` must be a {class}.",
          .envir = environment())
  }
  invisible(input)
}

#' @rdname checks
#' @export

check_names <- function(input, req, extract_names = names, type = all, action = abort,
                        arg = deparse(substitute(input))) {
  input_names <- extract_names(input)
  if (!type(req %in% input_names)) {
    req_names_missing <- req[which(!(req %in% input_names))]
    msg <- paste0("'",
      arg, "' does not contain ", deparse(substitute(type)),
      " required names. One or more of the following name(s) are missing: ",
      paste0("'", req_names_missing, collapse = "', "),
      "'."
    )
    action(msg)
  }
  invisible(input)
}

#' @rdname checks
#' @export

check_named_list <- function(input, ignore_empty = TRUE, arg = deparse(substitute(input))) {
  if (!any("list" %in% class(input))) stop(paste0("Argument '", arg, "' must be of class list."), call. = FALSE)
  list_is_empty <- (length(input) == 0)
  if (!list_is_empty | !ignore_empty) {
    if (is.null(names(input)) | any(names(input) %in% "")) {
      msg <- paste0("Argument '", arg, "' must be a named list.")
      stop(msg, call. = FALSE)
    }
  }
  invisible(input)
}

#' @rdname checks
#' @export

check_timeline <- function(input, arg = deparse(substitute(input))) {
  check_inherits(input, "POSIXct")
  input <- check_tz(input = input, arg = arg)
  stopifnot(!is.unsorted(input))
  stopifnot(!any(is.na(input)))
  invisible(input)
}

#' @rdname checks
#' @export

check_POSIXct <- function(input) {
  check_inherits(input, "POSIXct")
  check_tz(input)
}

#' @rdname checks
#' @export

check_tz <-
  function(input, arg = deparse(substitute(input))) {
    if (inherits(input, "Date") | inherits(input, "POSIXct")) {
      rlang::check_installed("lubridate")
      if (lubridate::tz(input) == "") {
        msg <- paste0("Argument '", arg, "' time zone currently ''; tz forced to UTC.")
        warning(msg, immediate. = TRUE, call. = FALSE)
        lubridate::tz(input) <- "UTC"
      }
    }
    invisible(input)
  }

#' @rdname checks
#' @export

check_new_colnames <- function(data, new) {
  bool <- new %in% colnames(data)
  if (any(bool)) {
    replace <- new[which(bool)]
    warn("Column(s) in `data` are being replaced: {paste0(paste0('`', replace, collapse = '`, '), '`')}.",
         .envir = environment())
  }
  NULL
}

#' @rdname checks
#' @export

# Check elements of a list are not NULL
check_not_null <- function(input, req) {
  if (!is.null(req)) {
    sapply(req, \(elm) {
      if (is.null(input[[elm]])) {
        abort("{deparse(substitute(input))}${elm} is required for this function.",
              .envir = environment())
      }
    })
  }
}

#' @rdname checks
#' @export

check_dots_allowed <- function(not_allowed, ...) {
  l <- list(...)
  if (any(names(l) %in% not_allowed)) {
    trouble <- names(l)[names(l) %in% not_allowed]
    msg <- paste0(
      "Additional argument(s) (", paste0("`", trouble, collapse = "`, "),
      "`) have been passed to the function via `...` which are not permitted."
    )
    abort(msg)
  }
}

#' @rdname checks
#' @export

check_dots_for_missing_period <- function(args, dots) {
  # List argument names
  args <- names(args)
  # Drop dots ("...")
  args <- args[!(args %in% "...")]
  # Drop leading period
  args <- sub("^\\.", "", args)
  # Name arguments passed via dots
  dots <- names(dots)
  # Check for any arguments in dots that match args without the periods
  # * This is likely due to the missing period (e.g. 'prompt' instead of '.prompt')
  dots_bool <- dots %in% args
  if (any(dots_bool)) {
    dots_in_args <- dots[dots_bool]
    warn("There are argument(s) passed via `...` that are identical, except for the missing leading period, to the function's main arguments: {paste0(paste0('`', dots_in_args, collapse = '`, '), '`')}. Did you mean to use: {paste0(paste0('`.', dots_in_args, collapse = '`, '), '`')}?",
         .envir = environment())
  }
}
