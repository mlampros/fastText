# Language Identification using fastText

Language Identification using fastText

## Usage

``` r
language_identification(
  input_obj,
  pre_trained_language_model_path,
  k = 1,
  th = 0,
  threads = 1,
  verbose = FALSE
)
```

## Arguments

- input_obj:

  either a valid character string to a valid path where each line
  represents a different text extract or a vector of text extracts

- pre_trained_language_model_path:

  a valid character string to the pre-trained language identification
  model path, for more info see
  https://fasttext.cc/docs/en/language-identification.html

- k:

  predict top k labels (1 by default)

- th:

  probability threshold (0.0 by default)

- threads:

  an integer specifying the number of threads to run in parallel. This
  parameter applies only if k \> 1

- verbose:

  if TRUE then information will be printed out in the console

## Value

an object of class data.table which includes two or more columns with
the names 'iso_lang_N' and 'prob_N' where 'N' corresponds to 1 to 'k'
input parameter

## References

https://fasttext.cc/docs/en/language-identification.html
https://becominghuman.ai/a-handy-pre-trained-model-for-language-identification-cadd89db9db8

## Examples

``` r
library(fastText)

vec_txt = c("Incapaz de distinguir la luna y la cara de esta chica,
             Las estrellas se ponen nerviosas en el cielo",
             "Unable to tell apart the moon and this girl's face,
             Stars are flustered up in the sky.")

file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")

dtbl_out = language_identification(input_obj = vec_txt,
                                   pre_trained_language_model_path = file_pretrained,
                                   k = 3,
                                   th = 0.0,
                                   verbose = TRUE)
#> The 'fasttext' algorithm starts ...
#> Conversion of the predicted labels and probabilities for k = 3 and threads = 1 ... 
#> The predicted labels will be loaded from the temporary file ...
#> The temporary files will be removed ...
#> Elapsed time: 0 hours and 0 minutes and 0 seconds. 
dtbl_out
#>    iso_lang_1   prob_1 iso_lang_2     prob_2 iso_lang_3     prob_3
#>        <char>   <char>     <char>     <char>     <char>     <char>
#> 1:         es 0.910368         fr  0.0177581         pt  0.0164652
#> 2:         es 0.849259         ca  0.0497277         fr  0.0141624
#> 3:         en 0.927154         pt 0.00641008         ru 0.00501036
#> 4:         en   0.9014         de  0.0311772         ru 0.00573721
```
