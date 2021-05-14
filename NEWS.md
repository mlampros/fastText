
## fastText 1.0.0

* I've added the *CITATION* file in the 'inst' directory
* I've added the **language_identification()** function
* **20-04-2021** : I've added the pre-trained language identification model **lid.176.ftz** which can be downloaded from https://fasttext.cc/docs/en/language-identification.html In the same website exists also the **lid.176.bin** model which is bigger in size, faster and slightly more accurate.
* **14-07-2019** : I fixed typos in vignette and modified the *plot_progress_logs()* function because it threw an error of the form : *line 1 did not have 11 elements* ( I added the *fill = TRUE* parameter to the *utils::read.table()* function to account for NA's as described in a [stackoverflow issue](https://stackoverflow.com/a/18161099/8302386) )
