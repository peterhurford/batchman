#' batch maps a function to a batched version of that function.
#'
#' @param batch_fn function. The method to batch over.
#' @param keys vector. The names of the keys within the function to batch.
#'   Can be "..." if one is batching a splat function with no keys.
#' @param splitting_strategy function. The strategy used to split up inputs.
#'   Leave NULL to use the versatile default splitting strategy.
#' @param combination_strategy function. The strategy used to recombine batches.
#'   Defaults to class-agnostic combination.
#' @param size numeric. The size of the packets. Default 50.
#' @param trycatch logical. Whether to wrap the function in a tryCatch block.
#'   Can be used to store and retrieve partial progress on an error.
#' @param batchman.verbose logical. Whether or not to announce progress by printing dots.
#' @param stop logical. Whether trycatch should stop if an error is raised.
#' @param retry integer. The number of times to retry on error. 0 for no retrying.
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
    sleep=0
) {

    make_body_fn <- function(splitting_strategy) {
      function(...) {
        next_batch <- splitting_strategy(...)
        loop(next_batch)
      }
    }


    loop <- function(next_batch) {
      if (is.null(next_batch)) return(NULL)
      batch_info <- next_batch()
      new_call <- batch_info$new_call
      run_env <- list2env(list(batch_fn = batch_fn))
      parent.env(run_env) <- parent.frame(find_in_stack(batch_info$keys[[1]]))
      p <- if (`verbose_set?`()) progress_bar(batch_info$num_batches)

      while (!is.done(new_call)) {
        if (`verbose_set?`()) { update_progress_bar(p) }

        batch <- if (isTRUE(trycatch)) {
          iterated_try_catch(
            eval(new_call, envir = run_env),
            new_call,
            run_env,
            retry
          )
        } else { eval(new_call, envir = run_env) }

        if (sleep > 0) { Sys.sleep(sleep) }

        batches <- if (is.no_batches(batches)) batch
          else combination_strategy(batches, batch)

        if (isTRUE(trycatch)) partial_progress$set(batches)
        new_call <- next_batch()$new_call
      }
      if (!is.no_batches(batches)) batches
    }


    find_in_stack <- function(what_to_eval) {
      if (!is(what_to_eval, "name")) return(3)
      stacks_to_search = c(3, 4)
      for (stack in stacks_to_search) {
        `exists?` <- exists(
          as.character(what_to_eval),
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
                batbelt::as.ordinal(retry - current_try + 1),
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


    decide_strategy <- function(splitting_strategy) {
      if (is.null(splitting_strategy)) { default_strategy }
      else { splitting_strategy }
    }


    default_strategy <- function(...) {
      args <- match.call(call = substitute(batch_fn(...)), definition = batch_fn)
      keys <- clean_keys(args, keys)
      if (length(keys) == 0) stop("Bad keys - no batched key matches keys passed.")
      args <- cache_functions(args, keys)
      where_the_inputs_at <- find_inputs(args, keys)
      if (length(where_the_inputs_at) == 0) return(NULL)
      what_to_eval <- args[[where_the_inputs_at[[1]]]]
      if (is.null(what_to_eval)) return(NULL)
      run_length <- calculate_run_length(what_to_eval)
      print_batching_message(run_length, size)
      generate_batch_maker(run_length, where_the_inputs_at, args, size)
    }


    clean_keys <- function(args, keys) {
      if (!identical(keys, "...")) keys <- keys[keys %in% names(args)]
      keys
    }


    cache_functions <- function(args, keys) {
      for (key in keys) {
        if (is.call(args[[key]]))
          args[[key]] <- eval(args[[key]], envir = parent.frame(find_in_stack(key)+1))
      }
      args
    }


    find_inputs <- function(args, keys) {
      if(identical(keys, "...")) seq(2, length(args))
      else grep(paste0(keys, collapse="|"), names(args))
    }


    calculate_run_length <- function(what_to_eval) {
      eval(
        bquote(NROW(.(what_to_eval))),
        envir = parent.frame(find_in_stack(what_to_eval))
      )
    }


    print_batching_message <- function(run_length, size) {
      if (run_length > size && `verbose_set?`()) {
        cat("More than", size, "inputs detected.  Batching...\n")
      }
    }


    generate_batch_maker <- function(run_length, where_the_inputs_at, args, size) {
      i <- 1
      second_arg <- quote(x[seq(y, z)])
      keys <- args[where_the_inputs_at]
      function() {
        if (i > run_length) return(list("new_call" = done))
        for (j in where_the_inputs_at) {
          second_arg[[2]] <- args[[j]]
          second_arg[[3]][[2]] <- i
          second_arg[[3]][[3]] <- min(i + size - 1, run_length)
          args[[j]] <- second_arg
        }
        i <<- i + size
        list(
          "new_call" = args,
          "keys" = keys,
          "num_batches" = ceiling(run_length / size)
        )
      }
    }


    if (is.batched_fn(batch_fn)) return(batch_fn)
    if (missing(keys)) stop("Keys must be defined.")
    if (isTRUE(stop) || retry > 0) trycatch <- TRUE
    if (!is.numeric(retry) || retry %% 1 != 0 || retry < 0) {
      stop("Retry must be an positive integer.")
    }

    if (isTRUE(trycatch)) partial_progress$clear()

    batched_fn <- function(...) {
      body_fn <- make_body_fn(decide_strategy(splitting_strategy))
      body_fn(...)
    }

    attr(batched_fn, "batched") <- TRUE
    class(batched_fn) <- c("batched_function", "function")
    batched_fn
}
