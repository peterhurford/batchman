#' Wrap a method and send to it in batches.
#'
#' @param batch_fn function. The method to batch over.
#' @param inputs. The inputs to split into batches.
#' @param ... additional arguments to pass to \code{batch_fn}.
#' @param size numeric. The size of the packets. Default 50.
#' @param verbose logical. Whether or not to announce progress by printing dots.
#' @export
batchman <- function(batch_fn, inputs, ..., size = 50, verbose = TRUE) {
  if (length(inputs) <= size) return(batch_fn(inputs, ...))
  slices <- slice(inputs, size)
  batches <- lapply(seq_along(slices), function(i) {
    if (verbose) cat(".")
    tryCatch(
      batch_fn(slices[[i]], ...),
      error = function(e) {
        warning("Some of the data failed to process because: ", e$message)
        NULL
      }
    )
  })
  combine_by_list(batches)
}
