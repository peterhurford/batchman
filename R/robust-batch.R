#' robust_batch re-runs the batch function to be highly resilient to errors.
#'
#' @param batched_fn function.  The batched function to re-run more robustly.
#' @param args list.  A list of the arguments to pass to batched_fn.
#' @export
robust_batch <- function(batched_fn, args) {
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
  o <- do.call(robust_batched_fn, args)
}
