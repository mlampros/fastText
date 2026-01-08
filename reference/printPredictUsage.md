# Print Usage Information when the command equals to 'predict' or 'predict-prob'

Print Usage Information when the command equals to 'predict' or
'predict-prob'

## Usage

``` r
printPredictUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printPredictUsage' function in the R session

## Examples

``` r
library(fastText)

printPredictUsage()
#> usage: fasttext predict[-prob] <model> <test-data> [<k>] [<th>]
#> 
#>   <model>      model filename
#>   <test-data>  test data filename (if -, read from stdin)
#>   <k>          (optional; 1 by default) predict top k labels
#>   <th>         (optional; 0.0 by default) probability threshold
#> 
```
