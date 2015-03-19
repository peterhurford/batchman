![Batchman](http://i.imgur.com/63jNVwY.png) 

## Batchman [![Build Status](https://travis-ci.org/peterhurford/batchman.svg?branch=master)](https://travis-ci.org/peterhurford/batchman?branch=master) [![Coverage Status](https://coveralls.io/repos/peterhurford/batchman/badge.png)](https://coveralls.io/r/peterhurford/batchman)

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

* You can now set `options('batchman.verbose' = FALSE)` to turn off batchman's print messages dynamically, even for functions that have already been generated.  This is potentially useful for tests where digging into batchman to set `verbose = FALSE` is annoying.


## Using Batchman with Big Data

Batchman is also very useful as a tool for handling big data in R, as your `combination_strategy` can involve something other than actually combining the two items.  For example, you might make a method like:

```R
big_data_combine <- function(first_batch, second_batch) {
  store_in_database(second_batch)  
}
```

Which will work to store each batch generated (except the first, which you'd have to make an exception for).  This way you don't have to keep a very large (too large) object in R memory, and can work on it in parts.


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
