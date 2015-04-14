#' robust_batch re-runs the batch function to be highly resilient to errors.
#'
#' @param batched_fn function.  The batched function to re-run more robustly.
#' @param batchman.retries integer.  The maximum amount of times to retry processing.
#' @export
robust_batch <- function(batched_fn, ..., batchman.verbose = isTRUE(interactive()),
  batchman.retries = 3) {

  robust_batched_fn <- batch(
    batchman::get_before_fn(batched_fn),
    environment(batched_fn)$keys,
    environment(batched_fn)$splitting_strategy,
    environment(batched_fn)$combination_strategy,
    environment(batched_fn)$size,
    batchman.verbose,
    trycatch = TRUE,
    stop = FALSE
  )
  remaining_args <- list(...)

  for (try in seq(batchman.retries)) {
    if (isTRUE(batchman.verbose)) {
      cat(paste0('Trying ', try, ' of ', batchman.retries, '...\n'))
      cat(paste0(length(remaining_args[[1]]), ' remaining...\n'))
    }
    run <- do.call(robust_batched_fn, remaining_args)
    if (length(run) < length(remaining_args)) {
      length(run) <- length(remaining_args)
    }
    remaining_args <- lapply(remaining_args, function(x) x[is.na(run)])
    if (try == 1) { output <- run }
    else {
      if (length(run) > sum(is.na(output))) { length(run) <- sum(is.na(output)) }
      output[is.na(output)] <- run
    }
    if (length(unlist(remaining_args)) == 0) break
  }
  output
}
