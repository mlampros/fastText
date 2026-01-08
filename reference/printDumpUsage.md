# Print Usage Information when the command equals to 'dump'

Print Usage Information when the command equals to 'dump'

## Usage

``` r
printDumpUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printDumpUsage' function in the R session

## Examples

``` r
library(fastText)

printDumpUsage()
#> usage: fasttext dump <model> <option>
#> 
#>   <model>      model filename
#>   <option>     option from args,dict,input,output
```
