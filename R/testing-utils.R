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

reverse <- function(x, y) c(y, x)

fncaller <- function(list_fn) list_fn[[1]]()

fn1 <- function() 1

get_expect_error_fn <- function(trycatch = TRUE, stop = FALSE, retry = FALSE) {
  batchman:::partial_progress$clear()
  batch(
    fncaller,
    'list_fn',
    combination_strategy = function(x,y) unlist(c(x,y)),
    size = 1,
    batchman.verbose = FALSE,
    trycatch = trycatch,
    stop = stop,
    retry = retry
  )
}
