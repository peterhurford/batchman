#' batch maps a function to a batched version of that function.
#'
#' @param batch_fn function. The method to batch over.
#' @param inputs. The inputs to split into batches.
#' @param ... additional arguments to pass to \code{batch_fn}.
#' @param size numeric. The size of the packets. Default 50.
#' @param verbose logical. Whether or not to announce progress by printing dots.
#' @param stop logical. Whether to stop if an error is raised.
#' @export
batch <- function(batch_fn, inputs, ..., splitting_strategy = NULL, combination_strategy, size = 50, verbose = TRUE, stop = TRUE) {
  if (length(inputs) <= size) return(batch_fn(inputs, ...))

  splitting_strategy <- if(is.null(splitting_strategy)) {
    function(inputs, size) {
      i <- 0
      run_length <- ceiling(length(inputs) / size)
      function() {
        if (i >= run_length) return('batchman.is.done')
        i <<- i+1
        split(inputs, as.integer((seq_along(inputs) - 1) / size))[i]
      }
    }
  } else splitting_strategy

  roller <- splitting_strategy(inputs, size)
  out <- roller()
  batches <- batch_fn(out)
  while (out != 'batchman.is.done') {
    if (verbose) cat('.')
    batch <- batch_fn(out)
    tryCatch({
      batches <<- combination_strategy(batches, batch)
    }, error = function(e) {
      if (stop) stop(e$message)
      else warning('Some of the data failed to process because: ', e$message)
    })
    out <- roller()
  }
}
