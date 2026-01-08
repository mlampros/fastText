# Print the parameters for a specific command

Print the parameters for a specific command

## Usage

``` r
print_parameters(command = "supervised")
```

## Arguments

- command:

  a character string specifying the command for which the parameters
  should be printed in the R session. It should be one of "skipgram",
  "cbow", "supervised", "test", "test-label" or "quantize"

## Value

It does not return a value but only prints the available parameters in
the R session

## References

https://github.com/facebookresearch/fastText#full-documentation

https://github.com/facebookresearch/fastText/issues/341#issuecomment-339783130

## Examples

``` r
if (FALSE) { # \dontrun{

library(fastText)

print_parameters(command = 'supervised')
} # }
```
