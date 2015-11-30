## `batch` is where all the action happens.  `batch` is a functional -- it takes a function as
## an argument (among many other arguments) and returns a function.  The returned function is
## a modified version of the original function that will process inputs in batch.

#' batch maps a function to a batched version of that function.
#'
#' @param batch_fn function. The method to batch over.
## We need to know what keys will be the ones that will be batched versus the ones that
## are not to be batched.  For example, for `mean`, we would want to batch the vector of numbers
## but we would not like to batch along the optional `na.rm` argument.
#' @param keys vector. The names of the keys within the function to batch.
#'   Can be "..." if one is batching a splat function with no keys.
## A splitting strategy is a methodology to split up the inputs.  Right now the default one
## that comes with batchman seems to handle every scenario well enough, and I've never come up
## with a reason to roll a custom splitting_strategy.  But I leave it to you to decide!
#' @param splitting_strategy function. The strategy used to split up inputs.
#'   Leave NULL to use the versatile default splitting strategy.
## The combination strategy is about how to recombine the inputs once they are processed in
## batch.  The default one will handle pretty much anything, but the trade-off is that doing so
## is slow.  So it's a good idea to pass a more specific combination_strategy if you can.
#' @param combination_strategy function. The strategy used to recombine batches.
#'   Defaults to class-agnostic combination.
#' @param size numeric. The size of the packets. Default 50.
#' @param trycatch logical. Whether to wrap the function in a tryCatch block.
#'   Can be used to store and retrieve partial progress on an error.
#' @param batchman.verbose logical. Whether or not to announce progress by printing dots.
#' @param stop logical. Whether trycatch should stop if an error is raised.
#' @param retry integer. The number of times to retry on error. 0 for no retrying.
#' @param sleep integer. Time in seconds to sleep between batches.
#' @param parallel logical. Use parallel::mclapply to execute your batches. Incompatible with retry.
#' @param ncores integer. Number of cores to use if parallel is set to true. Notice that it doesn't
#'   work on windows.
#' @return a batched version of the passed function.
#' @examples
#'   batched_identity <- batch(identity, "x", combination_strategy = c, size = 10)
#'   batched_identity(seq(100))
#'   # Does identity, but in batches of 10.
#' @export
batch <- function(
    batch_fn,
    keys,
    splitting_strategy = NULL,
    combination_strategy = batchman::combine,
    size = 50,
    trycatch = FALSE,
    batchman.verbose = isTRUE(interactive()),
    stop = FALSE,
    retry = 0,
    sleep = 0,
    ncores = parallel::detectCores(),
    parallel = FALSE
) {
    ## Parallellized code will behave oddly if some of the code stops for an error, so it's best not to do it.
    if(isTRUE(parallel) && isTRUE(trycatch)) {
      stop('Please choose speed or robustness. (parallel or retry. cannot have both)')
    }
    if (is.batched_fn(batch_fn)) return(batch_fn)
    if (missing(keys)) stop("Keys must be defined.")
    if (isTRUE(stop) || retry > 0) trycatch <- TRUE
    if (!is.numeric(retry) || retry %% 1 != 0 || retry < 0) {
      stop("Retry must be an positive integer.")
    }
    ## Batchman can store partial progress on runs if it stops unexpectedly.
    ## We should clear it on another run where partial progress is desired.
    if (isTRUE(trycatch)) partial_progress$clear()

    ## The goal is to swap the function with a batched version of itself.
    batched_fn <- function(...) {
      ## So we create a batched function.
      batched_fn <- make_batched_fn(decide_strategy(splitting_strategy))
      ## And then call it with the arguments that would have been passed
      ## to the non-batched function.
      batched_fn(...)
    }
    attr(batched_fn, "batched") <- TRUE
    class(batched_fn) <- c("batched_function", "function")


    ## The rest are just the helper functions.
    make_batched_fn <- function(splitting_strategy) {
      function(...) {
        ## The splitting strategy is how inputs are borken up into batches.
        ## Each call of the splitting strategy will return the next batch.
        ## So the first time it is called you get the first batch, the second
        ## time it is called you get the second batch, until you get a
        ## marker that there are no batches remaining.
        next_batch <- splitting_strategy(...)
        ## Once we have the next batch, we pass it into the loop.
        ## The loop is a giant while loop that processes all the batches.
        loop(next_batch)
      }
    }

    ## The splitting strategy will either be the default strategy or the
    ## custom strategy passed by the user.
    decide_strategy <- function(splitting_strategy) {
      if (is.null(splitting_strategy)) { default_strategy }
      else { splitting_strategy }
    }


    default_strategy <- function(...) {
      ## We use `match.call` to extract all the arguments that are being passed to
      ## the function we want to batch.
      args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
      ## We then look at the keys the user passed in saying they want to batch
      ## and the args that the function actually uses, and check that the keys
      ## are in the args.
      keys <- match_keys_with_args(args, keys)
      ## Sometimes the args being passed are function calls. If so, they will be
      ## evaluated during every batch, which is time intensive. To avoid this,
      ## we pre-evaluate them in advance, keeping the results in an in-memory cache.
      args <- cache_args_that_are_functions(args, keys)
      ## We now want to know the positions of the keys within the list of args.
      where_the_keys_at <- find_keys_within_args(args, keys)
      ## If there aren't any keys, return NULL
      if (length(where_the_keys_at) == 0) return(NULL)
      ## Use the indicies to find the first key to evaluate
      key_to_eval <- args[[where_the_keys_at[[1]]]]
      ## If the key is NULL, return NULL
      if (is.null(key_to_eval)) return(NULL)

      run_length <- calculate_run_length(key_to_eval)
      if (run_length > size && `verbose_set?`()) {
        cat("More than", size, "inputs detected.  Batching...\n")
      }

      ## Each call of the batch_calls function will return the next batch until
      ## there are no batches left.
      batch_calls(run_length, where_the_keys_at, args, size)
    }

    match_keys_with_args <- function(args, keys) {
      if (!identical(keys, "...")) keys <- keys[keys %in% names(args)]
      if (length(keys) == 0) stop("Bad keys - no batched key matches keys passed.")
      keys
    }

    cache_args_that_are_functions <- function(args, keys) {
      for (key in keys) {
        if (is.call(args[[key]]))
          args[[key]] <- eval(args[[key]], envir = parent.frame(find_in_stack(key)+1))
      }
      args
    }

    find_keys_within_args <- function(args, keys) {
      if(identical(keys, "...")) seq(2, length(args))
      else grep(paste0(keys, collapse="|"), names(args))
    }

    calculate_run_length <- function(key_to_eval) {
      eval(bquote(NROW(.(key_to_eval))),
        envir = parent.frame(find_in_stack(key_to_eval)))
    }

    batch_calls <- function(run_length, where_the_keys_at, args, size) {
      i <- 1
      second_arg <- quote(x[seq(y, z)])
      keys <- args[where_the_keys_at]

      make_batch_call <- function() {
        if (i > run_length) return(list("new_call" = done))
        for (j in where_the_keys_at) {
          second_arg[[2]] <- args[[j]]
          second_arg[[3]][[2]] <- i
          second_arg[[3]][[3]] <- min(i + size - 1, run_length)
          args[[j]] <- second_arg
        }
        i <<- i + size
        list("new_call" = args,
          "keys" = keys,
          "num_batches" = ceiling(run_length / size))
      }

      function(ncores) {
        lapply(1:ncores, function(core_idx) {
          make_batch_call()
        })
      }
    }

    loop <- function(next_batch) {
      if (is.null(next_batch)) return(NULL)
      ncores <- if (isTRUE(parallel)) ncores else 1
      batch_info <- next_batch(ncores)
      new_call <- lapply(batch_info, `[[`, 'new_call')
      run_env <- list2env(list(batch_fn = batch_fn))
      parent.env(run_env) <- parent.frame(find_in_stack(batch_info[[1]]$keys[[1]]))
      p <- if (`verbose_set?`()) progress_bar(ceiling(batch_info[[1]]$num_batches/ncores))
      apply_method <- if (isTRUE(parallel)) { parallel::mclapply } else { lapply }

      while(!is.done(new_call[[1]])) {
        temp_batches <- apply_method(new_call, function(newcall, ...) {
          if (is.done(newcall)) return(structure(NULL, emptyrun = TRUE))
          if (isTRUE(trycatch) && !isTRUE(parallel)) {
            iterated_try_catch(
              eval(newcall, envir = run_env),
              newcall,
              run_env,
              retry
            )
          } else { eval(newcall, envir = run_env) }
        }, mc.cores = ncores, mc.allow.recursive = FALSE, mc.preschedule = TRUE)
        if (`verbose_set?`()) { update_progress_bar(p) }
        if (sleep > 0) { Sys.sleep(sleep) }
        ## Parallel execution requires some special error handling
        errors <- vapply(temp_batches, function(x) is(x, 'try-error'), logical(1))
        if (any(errors)) {
          ## Looks like a batch has failed!
          ## Let's warn or stop
          if (isTRUE(stop)) {
            stop(attr(temp_batches[errors][[1]], 'condition'))
          } else {
            warning(as.character(attr(temp_batches[errors][[1]], 'condition')))
            temp_batches[errors] <- list(NULL)
          }
        }
        temp_batches  <- temp_batches[vapply(temp_batches, Negate(is.emptyrun), logical(1))]
        current_batch <- Reduce(combination_strategy, temp_batches)
        batches <- if (is.no_batches(batches)) current_batch
          else combination_strategy(batches, current_batch)

        if (isTRUE(trycatch)) partial_progress$set(batches)
        new_call <- lapply(next_batch(ncores), `[[`, 'new_call')
      }
      if (!is.no_batches(batches)) batches
    }


    find_in_stack <- function(key_to_eval) {
      if (!is(key_to_eval, "name")) return(3)
      stacks_to_search = c(3, 4)
      for (stack in stacks_to_search) {
        `exists?` <- exists(
          as.character(key_to_eval),
          envir = parent.frame(stack + 1),
          inherits = FALSE
        )
        if (`exists?`) return(stack)
      }
    }


    `verbose_set?` <- function() {
      # Verbose is true if it is enabled by the option OR
      # if it is not disabled by the option and is true in argument
      isTRUE(getOption("batchman.verbose")) || (
        !identical(getOption("batchman.verbose"), FALSE) &&
        isTRUE(batchman.verbose)
      )
    }


    iterated_try_catch <- function(expr, new_call, run_env, current_try) {
      tryCatch(
        eval(new_call, envir = run_env),
        error = function(e) {
          raise_error_or_warning(e, current_try)
          if (current_try > 0) {
            if (isTRUE(`verbose_set?`())) {
              cat(
                "Retrying for the",
                as.ordinal(retry - current_try + 1),
                "time.",
                "\n"
              )
            }
            iterated_try_catch(expr, new_call, run_env, current_try - 1)
          }
          else { NULL }
        }
      )
    }


    raise_error_or_warning <- function(e, retry) {
      if (isTRUE(stop) && retry == 0) {
        if (`verbose_set?`()) cat("\nERROR... HALTING.\n")
        if(exists("batches") && `verbose_set?`()) {
          cat("Partial progress saved to batchman::progress()\n")
        }
        stop(e$message)
      } else {
        if (grepl("Bad keys - no batched key", e$message)) stop(e$message)
        warning("Some of the data failed to process because: ", e$message)
      }
    }

    batched_fn
}
