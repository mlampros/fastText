# Multiple plot function

Multiple plot function

## Usage

``` r
multiplot(..., plotlist = NULL, cols = 1, layout = NULL)
```

## Arguments

- ...:

  ellipsis to pass ggplot objects

- plotlist:

  either NULL or a list of ggplot objects

- cols:

  Number of columns in layout

- layout:

  A matrix specifying the layout. If present, 'cols' is ignored

## Value

It does not return a value but only shows the ggplots in the R session

## Details

If the layout is something like matrix(c(1,2,3,3), nrow = 2, byrow =
TRUE), then plot 1 will go in the upper left, 2 will go in the upper
right, and 3 will go all the way across the bottom.

## References

http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page\_(ggplot2)/
