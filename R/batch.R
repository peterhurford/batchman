#' batch maps a function to a batched version of that function.
#'
#' @param batch_fn function. The method to batch over.
#' @param keys vector. The names of the keys within the function to batch.
#'   Can be '...' if one is batching a splat function with no keys.
#' @param splitting_strategy function. The strategy used to split up inputs.
#'   Leave NULL to use the versatile default splitting strategy.
#' @param combination_strategy function. The strategy used to recombine batches.
#'   Defaults to class-agnostic combination.
#' @param size numeric. The size of the packets. Default 50.
#' @param verbose logical. Whether or not to announce progress by printing dots.
#' @param trycatch logical. Whether to wrap the function in a tryCatch block.
#'   Can be used to store and retrieve partial progress on an error.
#' @param stop logical. Whether trycatch should stop if an error is raised.
#' @export
batch <- function(batch_fn, keys, splitting_strategy = NULL,
  combination_strategy = batchman::combine, size = 50, verbose = TRUE,
  trycatch = FALSE, stop = TRUE) {
    if (is.batched_fn(batch_fn)) return(batch_fn)
    if (isTRUE(stop)) trycatch <- TRUE
    splitting_strategy <- decide_strategy(splitting_strategy)
    batched_fn <- function(...) {
      body_fn <- make_body_fn(batch_fn, keys, splitting_strategy,
        combination_strategy, size, verbose, trycatch, stop)
      run_the_batches(..., body_fn = body_fn, trycatch = trycatch,
        stop = stop, verbose = verbose)
    }
    attr(batched_fn, 'batched') <- TRUE
    batched_fn
}

make_body_fn <- function(batch_fn, keys, splitting_strategy,
  combination_strategy, size, verbose, trycatch, stop) {
    function(...) {
      next_batch <- splitting_strategy(..., batch_fn = batch_fn,
        keys = keys, size = size, verbose = verbose
      )
      loop(batch_fn, next_batch, combination_strategy, verbose, trycatch)
    }
}

loop <- function(batch_fn, next_batch, combination_strategy, verbose, trycatch) {
  run_env <- list2env(list(batch_fn = batch_fn), parent = parent.frame())
  new_call <- next_batch()
  while (!batchman:::is.done(new_call)) {
    if (isTRUE(verbose)) cat('.')
    batch <- eval(new_call, envir = run_env)
    batches <- if (batchman:::is.no_batches(batches)) batch
      else combination_strategy(batches, batch)
    if (isTRUE(trycatch)) batchman:::partial_progress$set(batches)
    new_call <- next_batch()
  }
  if (!batchman:::is.no_batches(batches)) batches
}

run_the_batches <- function(..., body_fn, trycatch, stop, verbose) {
  if (isTRUE(trycatch))
    tryCatch(body_fn(...),
      error = function(e) default_batch_error(e, stop, verbose)
    )
  else body_fn(...)
}

default_strategy <- function(..., batch_fn, keys, size, verbose) {
  args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
  keys <- clean_keys(args, keys)
  args <- cache_functions(args, keys)
  where_the_inputs_at <- find_inputs(args, keys) 
  run_length <- eval(
    bquote(NROW(.(args[[where_the_inputs_at[[1]]]]))),
    envir = parent.frame(2)
  )
  print_batching_message(run_length, size, verbose)
  generate_batch_maker(run_length, where_the_inputs_at, args, size)
}

find_inputs <- function(args, keys) {
  if(identical(keys, '...')) seq(2, length(args))
  else grep(paste0(keys, collapse='|'), names(args))
}

clean_keys <- function(args, keys) {
  if (!identical(keys, '...')) keys <- keys[keys %in% names(args)]
  keys
}

cache_functions <- function(args, keys) {
  for (key in keys) {
    if (is.call(args[[key]]))
      args[[key]] <- eval(args[[key]], envir = parent.frame(5))
  }
  args
}

print_batching_message <- function(run_length, size, verbose) {
  if (run_length > size & verbose)
    cat('More than', size, 'inputs detected.  Batching...\n')
}

generate_batch_maker <- function(run_length, where_the_inputs_at, args, size) {
  i <- 1
  second_arg <- quote(x[seq(y, z)])
  function() {
    if (i > run_length) return(batchman:::done)
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

default_batch_error <- function(e, stop, verbose) {
  if (stop) {
    if (verbose) cat('\nERROR... HALTING.\n')
    if(exists('batches') && verbose)
      cat('Partial progress saved to batchman::progress()\n')
    stop(e$message)
  }
  else warning('Some of the data failed to process because: ', e$message)
}

decide_strategy <- function(splitting_strategy) {
  if (is.null(splitting_strategy)) batchman:::default_strategy else splitting_strategy
}
