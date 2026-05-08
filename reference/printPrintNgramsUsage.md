# Print Usage Information when the command equals to 'print-ngrams'

Print Usage Information when the command equals to 'print-ngrams'

## Usage

``` r
printPrintNgramsUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printPrintNgramsUsage' function in the R session

## Examples

``` r

library(fastText)

printPrintNgramsUsage()
#> usage: fasttext print-ngrams <model> <word>
#> 
#>   <model>      model filename
#>   <word>       word to print
#> 
```
