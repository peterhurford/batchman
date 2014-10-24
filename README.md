## Batchman

**Batchman** is a wrapper for R methods to run them in arbitrary batches.

For example, if you find that running `computationally_intensive_method(dataframe_with_lots_of_rows)` is having trouble, you could instead run the method via Batchman, which will split up the inputs into smaller components, run them in individual batches, and then merge all those batches together.

For example, `batchman(computationally_intensive_method, dataframe_with_lots_of_rows, size = 50)` would run the `computationally_intensive_method` in batches of size 50.

Some methods crash when running on large inputs.  Batchman helps prevent those crashes, and helps increase speed where batching is useful.


## Installation

This package is not yet available from CRAN. To install the latest development builds directly from GitHub, run this instead:

```R
if (!require('devtools')) install.packages('devtools')
devtools::install_github('peterhurford', 'batchman')
```
