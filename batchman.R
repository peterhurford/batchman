#' Wrap a method and send to it in batches.
#'
#' @param batch_fn function. The method to batch over.
#' @param inputs. The inputs to split into batches.
#' @param ... additional arguments to pass to \code{batch_fn}.
#' @param size numeric. The size of the packets. Default 50.
#' @param verbose logical. Whether or not to announce progress by printing dots.
#' @name batchman
#' @export
batchman <- function(batch_fn, inputs, ..., size = 50, verbose = TRUE) {
  slices <- slice(inputs, size)
  dfs <- lapply(seq_along(slices), function(i) {
    if (verbose) cat(".")
    tryCatch(packetized_method(slices[[i]], ...),
                   error = function(e)
                           { warning("Some of the data failed to process because: ", e$message); NULL })
    # TODO: (RK) Any sneaky way to fit intermediate caching in here?
  })
  do.call(plyr::rbind.fill, Filter(Negate(is.null), dfs))
}})}       '
