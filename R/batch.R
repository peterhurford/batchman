#' Wrap a method and send to it in batches.
#'
#' @param batch_fn function. The method to batch over.
#' @param inputs. The inputs to split into batches.
#' @param ... additional arguments to pass to \code{batch_fn}.
#' @param size numeric. The size of the packets. Default 50.
#' @param verbose logical. Whether or not to announce progress by printing dots.
#' @param stop logical. Whether to stop if an error is raised.
#' @export
batch <- function(batch_fn, inputs, ..., size = 50, verbose = TRUE, stop = FALSE) {
  if (length(inputs) <= size) return(batch_fn(inputs, ...))
  slices <- split(inputs, as.integer((seq_along(inputs) - 1) / size))
  batches <- lapply(seq_along(slices), function(i) {
    if (verbose) cat(".")
    if (stop) batch_fn(slices[[i]], ...)
    else {
      tryCatch(
        batch_fn(slices[[i]], ...),
        error = function(e) {
          warning("Some of the data failed to process because: ", e$message)
          NULL
        }
      )
    }
  })
  combine_by_list(batches)
}
