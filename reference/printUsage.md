# Print Usage Information for all parameters

Print Usage Information for all parameters

## Usage

``` r
printUsage(verbose = TRUE)
```

## Arguments

- verbose:

  if TRUE then information will be printed in the console

## Value

It does not return a value but only prints the available parameters of
the 'printUsage' function in the R session

## Examples

``` r
library(fastText)

printUsage()
#> usage: fasttext <command> <args>
#> 
#> The commands supported by fasttext are:
#> 
#>   supervised              train a supervised classifier
#>   quantize                quantize a model to reduce the memory usage
#>   test                    evaluate a supervised classifier
#>   test-label              print labels with precision and recall scores
#>   predict                 predict most likely labels
#>   predict-prob            predict most likely labels with probabilities
#>   skipgram                train a skipgram model
#>   cbow                    train a cbow model
#>   print-word-vectors      print word vectors given a trained model
#>   print-sentence-vectors  print sentence vectors given a trained model
#>   print-ngrams            print ngrams given a trained model and word
#>   nn                      query for nearest neighbors
#>   analogies               query for analogies
#>   dump                    dump arguments,dictionary,input/output vectors
#> 
```
