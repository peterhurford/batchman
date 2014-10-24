## Batchman

**Batchman** is a set of R methods to assist with batch processing, with the ability to run methods in batches and combine batches back together.

#### Run in Batches

If you find that running `computationally_intensive_method(dataframe_with_lots_of_rows)` is having trouble, you could instead run the method via Batchman, which will split up the inputs into smaller components, run them in individual batches, and then merge all those batches together.

For example, `batchman(computationally_intensive_method, dataframe_with_lots_of_rows, size = 50)` would run the `computationally_intensive_method` in batches of size 50.

Some methods crash when running on large inputs.  Batchman helps prevent those crashes, and helps increase speed where batching is useful.

#### Combine Batches

If you have objects you want to combine, but don't know their class, you can combine them together with `combine` (as long as all items are the same class).  This allows for class-agnostic recombination, which is useful for batching.

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


## Installation

This package is not yet available from CRAN. To install the latest development builds directly from GitHub, run this instead:

```R
if (!require('devtools')) install.packages('devtools')
devtools::install_github('peterhurford', 'batchman')
```
