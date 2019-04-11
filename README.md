[![Travis-CI Build Status](https://travis-ci.org/mlampros/fastText.svg?branch=master)](https://travis-ci.org/mlampros/fastText)
[![codecov.io](https://codecov.io/github/mlampros/fastText/coverage.svg?branch=master)](https://codecov.io/github/mlampros/fastText?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/fastText?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/fastText/branch/master)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>


## fastText
<br>

The **fastText** R package is an interface to the [fastText](https://github.com/facebookresearch/fastText) library for efficient learning of word representations and sentence classification. More details on the functionality of fastText can be found in my [blog-post](http://mlampros.github.io/2019/04/11/fastText_updated_version/) and in the package documentation.

<br>

To install the package from Github you can **either** use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github('mlampros/fastText')


```
<br>

**or** directly download the fastText-zip file using the **Clone or download** button in the [repository page](https://github.com/mlampros/fastText), extract it locally (rename it to *fastText* if necessary and check that files such as DESCRIPTION, NAMESPACE etc. are present when you open the fastText folder) and then run,

<br>

```R

#-------------
# on a Unix OS
#-------------

setwd('/your_folder/fastText/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('/your_folder/')
system("R CMD build fastText")
system("R CMD INSTALL fastText_1.0.0.tar.gz")


#------------------
# on the Windows OS  
#------------------

setwd('C:/your_folder/fastText/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('C:/your_folder/')
system("R CMD build fastText")
system("R CMD INSTALL fastText_1.0.0.tar.gz")

```
<br>

Use the following link to report bugs/issues (for the R wrapper),
<br><br>

[https://github.com/mlampros/fastText/issues](https://github.com/mlampros/fastText/issues)


<br>