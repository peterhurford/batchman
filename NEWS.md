## v0.2.3
* You can now set `options('batchman.verbose' = FALSE)` to turn off batchman's print messages.  This is potentially useful for tests where digging into batchman to set `verbose = FALSE` is annoying.  Keep in mind that this still won't affect batched functions after they are generated.

## v0.2.2.1
* Hotfixes a bug with the progress bar.

## v0.2.2
* Incorporates the progress bar from dplyr <https://github.com/hadley/dplyr/blob/master/R/progress.R>

## v0.2.1
* The way of getting the pre-batched function has changed from looking at the `attr` of the batched function to using `get_before_fn(batched_fn)`, which instead relies on looking at the environment of `batched_fn` instead of the attributes.

* Batched functions now are of class `batch_function` (in addition to being of class `function`), and printing a batched function will show a list with both the `before_fn` (the function prior to batching) and the `after_fn` (the batched funtion).  This should make looking into the function more clear, while not sacrificing your ability to see what is going on under the hood.
