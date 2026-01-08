# Print Usage Information when the command equals to 'nn'

Print Usage Information when the command equals to 'nn'

## Usage

``` r
printNNUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printNNUsage' function in the R session

## Examples

``` r
library(fastText)

printNNUsage()
#> usage: fasttext nn <model> <k>
#> 
#>   <model>      model filename
#>   <k>          (optional; 10 by default) predict top k labels
#> 
```
