rbomb <- local({
  .defused <- FALSE  # Whether or not it will explode when evaluated
  .stubborness <- 1  # How many defuses it takes to
  structure(list(
    class = "rbomb",
    is.defused = function() .defused,
    set_stubborness = function(val) .stubborness <<- val,
    defuse = function() {
      if (.stubborness == 1) {
        .defused <<- TRUE
      } else {
        .stubborness <<- .stubborness - 1
      }
      invisible()
    },
    reset = function() {
      .defused <<- FALSE
      .stubbornness <<- 1
      invisible()
    },
    detonate = function(val = 1) {
      if (!rbomb$is.defused()) {
        rbomb$defuse()
        stop("ESPLODZE!")
      } else val
    }
  ))
})

reverse <- function(x, y) c(y, x)

fncaller <- function(list_fn) list_fn[[1]]()

fn1 <- function() 1

get_expect_error_fn <- function(trycatch = TRUE, stop = FALSE, retry = 0, parallel = FALSE) {
  batchman:::partial_progress$clear()
  batch(
    fncaller,
    "list_fn",
    combination_strategy = function(x, y) unlist(c(x, y)),
    size = 1,
    batchman.verbose = FALSE,
    trycatch = trycatch,
    stop = stop,
    retry = retry,
    parallel = parallel
  )
}

error_fn <- function(x) { stop("ERROR") }
