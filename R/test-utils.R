# This is the R Bomb, a weird construct needed for tests.
# It mimics a process that doesn't work the first time, but works the second time.
rbomb <- local({
  .defused <- FALSE
  structure(list(
    class = 'rbomb',
    is.defused = function() .defused,
    defuse = function() .defused <<- TRUE,
    reset = function() .defused <<- FALSE,
    detonate = function(val = 1) {
      if (!rbomb$is.defused()) {
        rbomb$defuse()
        stop('ESPLODZE!')
      } else val
    }
  ))
})

get_expect_error_fn <- function(trycatch, stop) {
  batchman:::partial_progress$clear()
  fncaller <- function(list_fn) list_fn[[1]]()
  batch(fncaller, 'list_fn',
    combination_strategy = function(x,y) unlist(c(x,y)),
    size = 1, verbose = FALSE, trycatch = trycatch, stop = stop
  )
}
