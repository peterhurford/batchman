#' robust_batch re-runs the batch function to be highly resilient to errors.
#'
#' @param batched_fn function.  The batched function to re-run more robustly.
#' @param batchman.retries integer.  The maximum amount of times to retry processing.
#' @export
robust_batch <- function(batched_fn, ..., batchman.verbose = isTRUE(interactive()),
  batchman.retries = 3) {
  if(!is(batched_fn, 'batched_function'))
    stop("In order to use robust batch you need to create a ", sQuote("batched_function"))

  remaining_args <- list(...)

  for (try in seq(batchman.retries)) {
    ## On each try decrease the batch size if `batchman.retries << size`
    ## Subsequent retries may become slightly slower because of this,
    ## but this could help eliminate some errors caused by oversized batches
    ## Would be nice to implement something along the lines of TCP,
    ## where the optimal packet size is being determined automatically.
    batch_size <- ifelse(environment(batched_fn)$size > batchman.retries,
      floor(environment(batched_fn)$size / try), environment(batched_fn)$size)
    robust_batched_fn <- batch(
      batchman::get_before_fn(batched_fn),
      environment(batched_fn)$keys,
      environment(batched_fn)$splitting_strategy,
      environment(batched_fn)$combination_strategy,
      batch_size,
      batchman.verbose,
      trycatch = TRUE,
      stop = FALSE
    )

    if (isTRUE(batchman.verbose)) {
      cat(paste0('Trying ', pluralize(try), ' of ', batchman.retries, ' attempts...\n'))
      cat(paste0(length(remaining_args[[1]]), ' remaining...\n'))
      cat('That\'s ', batch_size, "\n")
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
