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
batch <- function(batch_fn, keys, splitting_strategy = 'simple',
  combination_strategy, size = 50, verbose = TRUE, trycatch = FALSE, stop = TRUE) {
  splitting_strategy <- decide_strategy(splitting_strategy)

  inert_wrapper <- function(x, error) x
  wrapper <- if (isTRUE(trycatch)) tryCatch else inert_wrapper

  function(...) {
    batches <- structure(list(), class = "no_batches")
    wrapper(error = default_batch_error, {
      next_batch <- splitting_strategy(..., batch_fn = batch_fn,
        keys = keys, size = size, verbose = verbose
      )

      run_env <- list2env(list(batch_fn = batch_fn), parent = parent.frame())
      new_call <- next_batch()
      while (!identical(new_call, 'batchman.is.done')) {
        if (isTRUE(verbose)) cat('.')
        batch <- eval(new_call, envir = run_env)
        batches <- if (is(batches, "no_batches")) batch else combination_strategy(batches, batch)
        new_call <- next_batch()
      }
    })
    if (is(batches, "no_batches")) new_arguments else batches
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

simple_strategy <- function(..., batch_fn, keys, size, verbose) {
  if (length(keys) > 1) stop('Simple strategy only works for one key.')
  args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
  if (!identical(keys, names(args)[[2]]))
    stop('Simple strategy only works when the key is the first arg.')
  run_length <- eval(bquote(NROW(.(args[[2]]))), envir = parent.frame(2))
  if (run_length > size & verbose)
    cat('More than', size, 'inputs detected.  Batching...\n')
  i <- 1
  
  second_arg <- quote(x[seq(y, z)])
  function() {
    if (i > run_length) return('batchman.is.done')
    second_arg[[2]] <- args[[2]]
    second_arg[[3]][[2]] <- i
    second_arg[[3]][[3]] <- min(i + size - 1, run_length)
    args[[2]] <- second_arg
    i <<- i + size
    args
  }
}

complex_strategy <- function(..., batch_fn, keys, size, verbose) {
  args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
  if(!any(names(args) %in% keys)) stop('Improper keys.')
  delete <- which(!keys %in% names(args))
  if (length(delete) > 0) kes <- keys[-delete]
  where_the_inputs_at <- grep(paste0(keys, collapse='|'), names(args))
  run_length <- eval(bquote(NROW(.(args[[where_the_inputs_at[[1]]]]))), envir = parent.frame(2))
  if (run_length > size & verbose)
    cat('More than', size, 'inputs detected.  Batching...\n')
  i <- 1
  
  second_arg <- quote(x[seq(y, z)])
  function() {
    if (i > run_length) return('batchman.is.done')
    for (j in where_the_inputs_at) {
      second_arg[[2]] <- args[[j]]
      second_arg[[3]][[2]] <- i
      second_arg[[3]][[3]] <- min(i + size - 1, run_length)
      args[[j]] <- second_arg
    }
    i <<- i + size
    args
  }
}

default_strategy <- function(..., batch_fn, keys, size, verbose) {
  args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
  if(!any(names(args) %in% keys)) stop('Improper keys.')
  delete <- which(!keys %in% names(args))
  if (length(delete) > 0) keys <- keys[-delete]
  where_the_inputs_at <- grep(paste0(keys, collapse='|'), names(args))
  run_length <- eval(bquote(NROW(.(args[[where_the_inputs_at[[1]]]]))))
  if (run_length > size & verbose)
    cat('More than', size, 'inputs detected.  Batching...\n')
  i <- 1
  function() {
    if (i > run_length) return('batchman.is.done')
    on.exit(i <<- i + size)
    out <- list()
    j <- 1
    for (input in as.list(args[-1])) {
      out[[j]] <- if (list(input) %in% as.list(args[where_the_inputs_at])) {
        eval(input)[seq(i, min(i + size - 1, run_length))]
      } else { input }
      j <- j + 1
    }
    out
  }
}

decide_strategy <- function(splitting_strategy) {
  if(identical('complex', splitting_strategy)) batchman:::complex_strategy
  else if (identical('simple', splitting_strategy)) batchman:::simple_strategy
  else splitting_strategy
}

default_batch_error <- function(e) {
  if (stop) {
    if (verbose) cat('\nERROR... HALTING.\n')
    #if(exists('batches')) {
    #  batchman:::partial_progress$set(batches)
    #  if (verbose) cat('Partial progress saved to batchman::progress()\n')
    #}
    stop(e$message)
  }
  else warning('Some of the data failed to process because: ', e$message)
}

