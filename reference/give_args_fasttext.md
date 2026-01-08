# The Rcpp function which is used in the 'fasttext_interface' R function

The Rcpp function which is used in the 'fasttext_interface' R function

## Usage

``` r
give_args_fasttext(
  args,
  pth = "",
  MilliSecs = 100L,
  pth_in = "",
  queryWord = "",
  remove_previous_file = TRUE
)
```

## Arguments

- args:

  the arguments that will be passed to the function in form of a
  character vector

- pth:

  a character string specifying the path where the process-logs (or
  output in generally) should be saved

- MilliSecs:

  an integer specifying the delay in milliseconds when printing the
  results to the specified path_output

- pth_in:

  a character string specifying the path to the input data file

- queryWord:

  either an empty string or the queryword that should be passed to the
  function

- remove_previous_file:

  a boolean. If TRUE, in case that the path_output is not an empty
  string (""), then an existing file with the same output name will be
  removed

## Value

It does not return a value but only saves the results to a file
