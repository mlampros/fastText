# Print Usage Information when the command equals to 'analogies'

Print Usage Information when the command equals to 'analogies'

## Usage

``` r
printAnalogiesUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printAnalogiesUsage' function in the R session

## Examples

``` r

library(fastText)

printAnalogiesUsage()
#> usage: fasttext analogies <model> <k>
#> 
#>   <model>      model filename
#>   <k>          (optional; 10 by default) predict top k labels
#> 
```
