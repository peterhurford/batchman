#' Recombine a batch onto the original, for all classes.
#' @param original. The object to get merged upon by the batch.
#' @param batch. The batch to merge onto the original. Must be the same class as
#' original.
#' @return an object of the same class as original, but now including batch.
#' @export
recombine <- function(original, batch) {
  if (is.null(original)) { return(batch) }

  if (!identical(class(original), class(batch)))
    stop('Recombine requires matching classes.')

  if (identical(class(original), 'character') & length(original) == 1)
    paste0(original, batch)                              # String
  else if (class(original) %in% c('character', 'numeric', 'list'))
    c(original, batch)                                   # Vector, List
  else if (identical(class(original), 'data.frame'))
    plyr::rbind.fill(original, batch)                    # Data frame
  else if (identical(class(original), 'matrix'))
    merge(original, batch, by = 'row.names', all = TRUE) # Matrix
  else
    stop('Class for recombine not recognized.')
}
