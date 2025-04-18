% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{checks}
\alias{checks}
\alias{check_dir_exists}
\alias{check_dir_empty}
\alias{check_dir_contents_ext}
\alias{check_file_exists}
\alias{check_inherits}
\alias{check_names}
\alias{check_named_list}
\alias{check_timeline}
\alias{check_POSIXct}
\alias{check_tz}
\alias{check_new_colnames}
\alias{check_not_null}
\alias{check_dots_allowed}
\alias{check_dots_for_missing_period}
\title{\code{check} functions}
\usage{
check_dir_exists(input, action = abort)

check_dir_empty(input, action = abort)

check_dir_contents_ext(input, ext, ...)

check_file_exists(input, action = abort)

check_inherits(input, class, action = abort)

check_names(
  input,
  req,
  extract_names = names,
  type = all,
  action = abort,
  arg = deparse(substitute(input))
)

check_named_list(input, ignore_empty = TRUE, arg = deparse(substitute(input)))

check_timeline(input, arg = deparse(substitute(input)))

check_POSIXct(input)

check_tz(input, arg = deparse(substitute(input)))

check_new_colnames(data, new)

check_not_null(input, req)

check_dots_allowed(not_allowed, ...)

check_dots_for_missing_period(args, dots)
}
\arguments{
\item{input, action, ext, class, req, extract_names, type, arg, ignore_empty, data, new, not_allowed, args, dots, ...}{Function arguments.}
}
\description{
These functions perform generic checks on user inputs.
}
\author{
Edward Lavender
}
