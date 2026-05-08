# Print Usage Information when the command equals to 'test-label'

Print Usage Information when the command equals to 'test-label'

## Usage

``` r
printTestLabelUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printTestLabelUsage' function in the R session

## Examples

``` r

library(fastText)

printTestLabelUsage()
#> usage: fasttext test-label <model> <test-data> [<k>] [<th>]
#> 
#>   <model>      model filename
#>   <test-data>  test data filename
#>   <k>          (optional; 1 by default) predict top k labels
#>   <th>         (optional; 0.0 by default) probability threshold
#> 
```
