#' Stores partial progress.
#'
#' If a batch breaks, you don't want to lose all your progress done up until
#' that batch.  Batchman allows partial progress to be stored and retrieved.
#'
#' @export
progress <- function() partial_progress$get()

partial_progress <- local({
  .cache <- list()
  structure(list(
    get = function() .cache,
    clear = function() .cache <<- list(),
    set = function(value) .cache <<- value
  ))
})
