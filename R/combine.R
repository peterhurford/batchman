#' Combine multiple objects into one object, regardless of class.
#' @param ... A list of batches to combine.
#' @return an object of the same class as original, but now including batch.
#' @export
combine <- checkr::ensure(function(...) {
  batchman::combine_by_list(unname(list(...)))
})

#' Combine multiple objects into one object, regardless of class.
#' @param combination_list list. A list of batches to combine.
#' @param ... list. Any number of additional arguments, which are ignored.
#' @return an object of the same class as original, but now including batch.
#' @export
combine_by_list <- checkr::ensure(pre = combination_list %is% list,
  function(combination_list, ...) {
    if (length(combination_list) == 1) return(combination_list[[1]])
    first <- combination_list[[1]]
    fn <- if (is.character(first) & length(first) == 1) { # String
      paste0
    } else if (class(first) %in%
      c("character", "numeric", "list",
        "logical", "integer", "NULL")) { c }        # Vector, List
    else if (is.data.frame(first)) {                # Data frame
      function(...) {
        do.call(plyr::rbind.fill, Filter(Negate(is.null), list(...)))
      }
    } else if (is.matrix(first)) { merge }          # Matrix
    else {
      stop("Class for combine not recognized.")
    }
    do.call(fn, combination_list)
  })
