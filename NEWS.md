## v1.1.0

* Introduces the ability to set `options(batchman.verbose = TRUE)` to dynamically turn on verbosity.
* Added a test for `combine` with integers.
* Refactors the codebase for improved readibility.
* Fixed a bug in the progress bar.

## v1.0.0

* Integrated `robust_batch` into `batch`, using a `retry` parameter.  This change is NOT backwards compatible.

## v0.3.2
* `verbose` is now TRUE by default in interactive mode (where `interactive()` evaluates to TRUE), and FALSE by default in non-interactive mode.

## v0.3.1
* Switches `verbose` to `batchman.verbose` so as to not collide with `verbose` arguments within actual functions.
* Switched to [semantic versioning](http://www.semver.org).

## v0.3.0
* A new method, `robust_batch`, has been added that can attempt batching with automatic error-handling.  Call `robust_batch(batched_fn, ...)` where `batched_fn` is the function that you want to use after applying `batch` to it, and `...` are the arguments you wanted to pass to `batched_fn`.  The default amount of max retries is 3, but you can change this by passing `batchman.retries`.
* `stop = FALSE` now works as originally intended to keep going with batch processing even if an error occurs midstream.  Batches that result in an error simply return NA instead.

## v0.2.4
* Setting `options('batchman.verbose' = FALSE)` will now dynamically turn off batchman's printed output, even for batched functions after they are generated.  Note that there is still no way to dynamically turn on the verbosity if `verbose = FALSE` was originally set when the function was made.
* Initial prototypes for covr and travis added.

## v0.2.3
* You can now set `options('batchman.verbose' = FALSE)` to turn off batchman's print messages.  This is potentially useful for tests where digging into batchman to set `verbose = FALSE` is annoying.  Keep in mind that this still won't affect batched functions after they are generated.

## v0.2.2.1
* Hotfixes a bug with the progress bar.

## v0.2.2
* Incorporates the progress bar from dplyr <https://github.com/hadley/dplyr/blob/master/R/progress.R>

## v0.2.1
* The way of getting the pre-batched function has changed from looking at the `attr` of the batched function to using `get_before_fn(batched_fn)`, which instead relies on looking at the environment of `batched_fn` instead of the attributes.

* Batched functions now are of class `batch_function` (in addition to being of class `function`), and printing a batched function will show a list with both the `before_fn` (the function prior to batching) and the `after_fn` (the batched funtion).  This should make looking into the function more clear, while not sacrificing your ability to see what is going on under the hood.

