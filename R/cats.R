#' @title `cat` functions
#' @description `cat_*()` functions for user output. 
#' @param .fun A `character` string that defines the name of the parent function.
#' @param .verbose A `logical` variable or a `string` that defines the path to a text file.
#' * `.verbose = FALSE` suppresses user outputs;
#' * `.verbose = TRUE` sends user outputs to the console;
#' * `.verbose = file.path("path", "to", "text", "file.txt")` sends user outputs to a `.txt` file;
#'
#' @details 
#' These functions were initially developed for the `patter` package.
#' * [`cat_setup()`] sets up messages within functions;
#' * [`cat_init()`] defines a [`cat()`] wrapper based on `.verbose`;
#' * [`cat_log_file()`] validates `.verbose` and, if necessary, creates the `.txt` file;
#'  
#' @example man/examples/example-cats.R
#' @author Edward Lavender
#' @name cats

#' @rdname cats
#' @export

# Set up messages for a function
cat_setup <- function(.fun, .verbose) {
  # Define start time
  t_onset <- Sys.time()
  # Define cat_log() function
  cat_log <- cat_init(.verbose = .verbose)
  # Provide startup message
  cat_log(call_start(.fun = .fun, .start = t_onset))
  # Define exit expression
  exit_expr <- expression(
    cat_log(call_end(.fun = .fun, .start = t_onset, .end = Sys.time()))
  )
  # Define a list of outputs
  list(cat = cat_log,
       exit = exit_expr,
       envir = environment())
}

#' @rdname cats
#' @export

# Initiate cat() options and get an appropriate cat() function
cat_init <- function(.verbose) {
  # Define log file
  log_file <- cat_log_file(.verbose = .verbose)
  # Define function to send messages to console or file
  append_messages <- ifelse(log_file == "", FALSE, TRUE)
  function(..., message = !isFALSE(.verbose), file = log_file, append = append_messages) {
    if (message) cat(paste(..., "\n"), file = file, append = append)
  }
}

#' @rdname cats
#' @export

# Create a log.txt file
cat_log_file <- function(.verbose) {
  # Check .verbose input
  if (!is.logical(.verbose) & !is.character(.verbose)) {
    abort("`.verbose` should be a logical variable or a file path.")
  }
  # Return .verbose if TRUE/FALSE
  if (is.logical(.verbose)) {
    return("")
    # Handle file path inputs
  } else if (is.character(.verbose)) {
    # Check .verbose is a text file
    if (tools::file_ext(.verbose) != "txt") {
      abort("`.verbose` ('{.verbose}') should be the path to a text (.txt) file.",
            .envir = environment())
    }
    # Check the directory exists
    if (!dir.exists(dirname(.verbose))) {
      abort("`dirname(.verbose)` ('{dirname(.verbose)}') does not exist.",
            .envir = environment())
    }
    if (!file.exists(.verbose)) {
      # Create the text file
      success <- file.create(.verbose)
      if (!success) {
        abort("Failed to create log file ('{.file}').",
              .envir = environment())
      }
    } else {
      # Warn if the text file exists and is not empty
      if (length(readLines(.verbose)) > 0L) {
        warn("`.verbose` ('{.verbose}`) already exists and is not empty!",
             .envir = environment())
      }
    }
  }
  .verbose
}