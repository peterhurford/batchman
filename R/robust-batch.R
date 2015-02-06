#' robust_batch re-runs the batch function to be highly resilient to errors.
#'
#' @param batched_fn function.  The batched function to re-run more robustly.
#' @export
robust_batch <- function(batched_fn, ...) {
  robust_batched_fn <- batchman::batch(
    batchman::get_before_fn(batched_fn),
    environment(batched_fn)$keys,
    environment(batched_fn)$splitting_strategy,
    environment(batched_fn)$combination_strategy,
    environment(batched_fn)$size,
    environment(batched_fn)$verbose,
    trycatch = TRUE,
    stop = FALSE
  )
  remaining_args <- list(...)
  for (try in seq(3)) {
    run <- do.call(robust_batched_fn, remaining_args)
    remaining_args <- list(unlist(remaining_args)[is.na(run)])
    if (try == 1) output <- run else output[is.na(output)] <- run
  }
  output
}
