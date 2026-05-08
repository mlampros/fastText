# Print Usage Information when the command equals to 'test'

Print Usage Information when the command equals to 'test'

## Usage

``` r
printTestUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printTestUsage' function in the R session

## Examples

``` r

library(fastText)

printTestUsage()
#> usage: fasttext test <model> <test-data> [<k>] [<th>]
#> 
#>   <model>      model filename
#>   <test-data>  test data filename (if -, read from stdin)
#>   <k>          (optional; 1 by default) predict top k labels
#>   <th>         (optional; 0.0 by default) probability threshold
#> 
```
