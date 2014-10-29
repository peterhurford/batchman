#' batch maps a function to a batched version of that function.
#'
#' @param batch_fn function. The method to batch over.
#' @param splitting_strategy function. The strategy used to split up inputs.
#' @param combination_strategy function. The strategy used to recombine batches.
#' @param ... additional arguments to pass to \code{batch_fn}.
#' @param size numeric. The size of the packets. Default 50.
#' @param verbose logical. Whether or not to announce progress by printing dots.
#' @param stop logical. Whether to stop if an error is raised.
#' @export
batch <- function(batch_fn, splitting_strategy = NULL, combination_strategy, size = 50, verbose = TRUE, stop = TRUE) {
  function(...) {
    if (length(..1) <= size) return(batch_fn(...))
    else {
      if(verbose) cat('More than', size, 'inputs detected.  Batching...\n')
      splitting_strategy <- if(is.null(splitting_strategy)) {
        function(..1, size) {
          i <- 0
          run_length <- ceiling(length(..1) / size)
          function() {
            if (i >= run_length) return('batchman.is.done')
            i <<- i+1
            split(..1, as.integer((seq_along(..1) - 1) / size))[i]
          }
        }
      } else splitting_strategy
      roller <- splitting_strategy(inputs, size)
      out <- roller()
      while (out != 'batchman.is.done') {
        if (verbose) cat('.')
        arguments <- substitute(alist(...))
        arguments[[2]] <- out
        batch <- do.call(batch_fn, list(arguments))
        tryCatch({
          if (exists('batches'))
            batches <- combination_strategy(batches, batch)
          else
            batches <- batch
        }, error = function(e) {
          if (stop) stop(e$message)
          else warning('Some of the data failed to process because: ', e$message)
        })
        out <- roller()
      }
      batches
    }
  }
}
