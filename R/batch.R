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
batch <- function(batch_fn, keys, splitting_strategy = NULL, combination_strategy, size = 50, verbose = TRUE, stop = TRUE) {
  function(...) {
    splitting_strategy <- if(is.null(splitting_strategy)) batchman:::key_strategy else splitting_strategy
    tryCatch({
      roller <- splitting_strategy(..., batch_fn = batch_fn, keys = keys, size = size, verbose = verbose)
      out <- roller()
      while (!identical(out, 'batchman.is.done')) {
        if (verbose) cat('.')
        arguments <- substitute(alist(...))
        arguments[[1]] <- quote(batch_fn)
        k <- 1
        for (output in out) {
          arguments[[k+1]] <- out[[k]]
          k <- k + 1
        }
        batch <- eval(arguments)
        if (exists('batches'))
          batches <- combination_strategy(batches, batch)
        else
          batches <- batch
        out <- roller()
      }
    }, error = function(e) {
      if (stop) {
        if (verbose) cat('\nERROR... HALTING.\n')
        if(exists('batches')) {
          batchman:::partial_progress$set(batches)
          if (verbose) cat('Partial progress saved to batchman::progress()\n')
        }
        stop(e$message)
      }
      else warning('Some of the data failed to process because: ', e$message)
    })
    if (exists('batches')) batches else out
  }
}

partial_progress <- local({
  .cache <- list()
  structure(list(
    get = function() .cache,
    clear = function() .cache <<- list(),
    set = function(value) .cache <<- value
  ))
})

#' @export
progress <- function() batchman:::partial_progress$get()

key_strategy <- function(..., batch_fn, keys, size, verbose) {
  args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
  if(!any(names(args) %in% keys)) stop('Improper keys.')
  delete <- which(!keys %in% names(args))
  if (length(delete) > 0) keys <- keys[-delete]
  where_the_inputs_at <- which(keys %in% names(args))
  run_length <- eval(bquote(NROW(.(args[[where_the_inputs_at[[1]] + 1]]))))
  if (run_length > size & verbose) cat('More than', size, 'inputs detected.  Batching...\n')
  i <- 1
  function() {
    if (i > run_length) return('batchman.is.done')
    on.exit(i <<- i + size)
    out <- list()
    j <- 1
    for (input in list(...)[where_the_inputs_at]) {
      sliceput <- input[seq(i, min(i + size - 1, run_length))]
      out[[j]] <- sliceput
      j <- j + 1
    }
    out
  }
}
