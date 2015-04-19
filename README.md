![Batchman](http://i.imgur.com/63jNVwY.png) 

## Batchman [![Build Status](https://travis-ci.org/peterhurford/batchman.svg?branch=master)](https://travis-ci.org/peterhurford/batchman?branch=master) [![Coverage Status](https://img.shields.io/coveralls/peterhurford/batchman.svg)](https://coveralls.io/r/peterhurford/batchman) ![Release Tag](https://img.shields.io/github/tag/peterhurford/batchman.svg)

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



## Using Batchman with Error-Prone Sources

Sometimes you might be batching functions that can be error-prone, like an API call.  When initializing the batching via the `batch` function, choose to pass `trycatch = TRUE` to `batch` to run Batchman in a `tryCatch` block.  If this is done, batchman will stop upon an error, but store all the progress so far, which you can retrieve with `batchman::progress()`.

You can pass both `trycatch = TRUE` and `stop = FALSE` to `batch`, and Batchman will keep going even when encountering an error.  Blocks that result in an error will be converted to `NA`.

-

Lastly, rather than having to constantly re-run and paste together the batch function when it errors, you can use `robust_batch`.  `robust_batch` will run Batchman all the way through, and then attempt to re-run on only the `NA`s (which result from errors).

To do this, call `robust_batch(batch_fn, ...)` where `batched_fn` is the function that you want to use after applying `batch` to it, and `...` are the arguments you wanted to pass to `batched_fn`.

The default amount of max retries is 3, but you can change th is by passing `batchman.retries`.



## Using Batchman with Big Data

Batchman could be a useful tool for handling big data in R.  Since batchman allows for a custom `combination_strategy`, you could do something other than combining the items in R memory (which may not be possible with the size of your data).  For example, you might make a method like:

```R
big_data_combine <- function(first_batch, second_batch) {
  store_in_database(second_batch)  
}
```

Which will work to store each batch generated (except the first, which you'd have to make an exception for).

You then can call batchman using `big_data_combine` as the `combination_strategy`.



## Combine Batches

If you have objects you want to combine, but don't know their class, you can combine them together with `combine` (as long as all items are the same class).  This allows for class-agnostic recombination, which is useful for batching, and is the default combination method for Batchman.

```R
> combine(1, 2, 3, 4)
[1] 1 2 3 4

> combine('hello', 'world')
[1] "helloworld"

> combine(c(1,2,3), c(4,5,6))
[1] 1 2 3 4 5 6

> combine(list('first' = 'a'), list('second' = 'b', 'third' = 'c'))
$first
[1] "a"

$second
[1] "b"

$third
[1] "c"

> combine(data.frame(x = 1:3, y = c('a','b','c')), data.frame(x = 11:13, y = c('x','y','z')))
   x y
   1  1 a
   2  2 b
   3  3 c
   4 11 x
   5 12 y
   6 13 z

> combine(matrix(1:6, nrow = 2, ncol = 3), matrix(11:16, nrow = 2, ncol = 3))
Still not good with matricies. Coming soon.
```

Bonus: If you have a list of things to combine, you can call `combine_by_list(list)` to combine them directly.



## Other Features

* You can now set `options('batchman.verbose' = FALSE)` to turn off batchman's print messages dynamically, even for functions that have already been generated.  This is potentially useful for tests where digging into batchman to set `bachman.verbose = FALSE` is annoying.
