## v0.3.0
* `stop = FALSE` now works as originally intended to keep going with batch processing even if an error occurs midstream.  Batches that result in an error simply return NA instead.


## v0.2.1
* The way of getting the pre-batched function has changed from looking at the `attr` of the batched function to using `get_before_fn(batched_fn)`, which instead relies on looking at the environment of `batched_fn` instead of the attributes.

* Batched functions now are of class `batch_function` (in addition to being of class `function`), and printing a batched function will show a list with both the `before_fn` (the function prior to batching) and the `after_fn` (the batched funtion).  This should make looking into the function more clear, while not sacrificing your ability to see what is going on under the hood.
