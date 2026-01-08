# Plot the progress of loss, learning-rate and word-counts

Plot the progress of loss, learning-rate and word-counts

## Usage

``` r
plot_progress_logs(path_logs = "progress_data.txt", plot = FALSE)
```

## Arguments

- path_logs:

  a character string specifying a valid path to a file where the
  progress-logs are saved

- plot:

  a boolean specifying if the loss, learning-rate and word-counts should
  be plotted

## Value

an object of class data.frame that includes the progress logs with
columns 'progress', 'words_sec_thread', 'learning_rate' and 'loss'

## References

http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page\_(ggplot2)/

## Examples

``` r
if (FALSE) { # \dontrun{

library(fastText)

#-----------------------------------------------------------------
# the 'progress_data.txt' file corresponds to the 'path_output'
# parameter of the 'fasttext_interface()'. Therefore the user has
# to run first the 'fasttext_interface()' function to save the
# 'progress_data.txt' file to the desired folder.
#-----------------------------------------------------------------

res = plot_progress_logs(path = file.path(tempdir(), "progress_data.txt"),
                         plot = TRUE)

} # }
```
