
## fastText 1.0.0

* **14-07-2019** : I fixed typos in vignette and modified the *plot_progress_logs()* function because it threw an error of the form : *line 1 did not have 11 elements* ( I added the *fill = TRUE* parameter to the *utils::read.table()* function to account for NA's as described in a [stackoverflow issue](https://stackoverflow.com/a/18161099/8302386) )
