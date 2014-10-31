![Batchman](http://i.imgur.com/63jNVwY.png)

## Batchman
**Batchman** is a wrapper for R methods to run them in arbitrary batches.

Some methods crash when running on large inputs.  Batchman helps prevent those crashes, and helps increase speed where batching is useful.

If you find that running `computationally_intensive_method(dataframe_with_lots_of_rows)` is having trouble, you could instead run the method via Batchman, which will split up the inputs into smaller components, run them in individual batches, and then merge all those batches together.

To run batchman, you specify the function you want to batch, the key or keys that you want to batch by (or "..." if batching a splat function), a combination strategy for how to combine batches (e.g., `c` or `rbind`), and a batch size.

For example, to run `computationally_intensive_method` in batches of 50...

```R
batched_method <- batch(computationally_intensive_method, keys = 'df', combination_strategy = rbind, size = 50)
batched_method(dataframe_with_lots_of_rows)
```


## Installation

This package is not yet available from CRAN. To install the latest development builds directly from GitHub, run this instead:

```R
if (!require('devtools')) install.packages('devtools')
devtools::install_github('peterhurford', 'batchman')
```


## More Options

* Pass `trycatch = TRUE` to `batch` to run Batchman in a tryCatch block.  If this is done, batchman will stop upon an error, but store all the progress so far, which you can retrieve with `batchman::progress()`.

* Pass `stop = FALSE` to `batch` to have Batchman keep going when an error occurs, aborting any failed blocks.

* (If really inclined, you can pass a custom `splitting_strategy` to `batch`.  Not for the faint of heart.  The default splitting strategy should handle 99.9% of scenarios in which you use Batchman.)
