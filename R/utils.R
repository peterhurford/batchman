batches <- structure(list(), class = 'no_batches')
done <- structure(list(), class = 'batchman.is.done')

#' A helper method to determine if batches doesn't contain batches.
#' @param batches The object to see if it contains batches.
is.no_batches <- function(batches) is(batches, 'no_batches')

#' A helper method to determine if batchman is done batching.
#' @param call The batchman call object.
is.done <- function(call) is(call, 'batchman.is.done')

#' A helper method to determine if a function is already batched.
#' @param fn function. The function to test.
#' @export
is.batched_fn <- function(fn) isTRUE(attr(fn, 'batched'))

#' Print batched functions as they once were.
#' @param fn function. The function to print.
#' @export
print.batched_function <- function(fn) attr(fn, 'batch_fn')
