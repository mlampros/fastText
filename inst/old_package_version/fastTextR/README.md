
[![Travis-CI Build Status](https://travis-ci.org/mlampros/fastTextR.svg?branch=master)](https://travis-ci.org/mlampros/fastTextR)
[![codecov.io](https://codecov.io/github/mlampros/fastTextR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/fastTextR?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/fastTextR?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/fastTextR/branch/master)




## fastTextR
<br>

The fastTextR package is an R wrapper (only) for the *skipgram* and *cbow* functions of the [*fastText*](https://github.com/facebookresearch/fastText) library. *fastText* is a library for efficient learning of word representations and sentence classification. Since it uses C++11 features, it requires a compiler with good C++11 support. These include : (gcc-4.6.3 or newer) or (clang-3.3 or newer). More information about the fastText library can be found in [https://github.com/facebookresearch/fastText](https://github.com/facebookresearch/fastText). COPYRIGHTS, LICENSE and PATENTS files can be found in the *inst* folder of the R package.

A detailed example can be found in my [blog-post](http://mlampros.github.io/2017/01/05/textTinyR_package/) about text processing, in section 'word vectors'.

<br>

To install the package from Github you can **either** use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github('mlampros/fastTextR')


```
<br>

**or** directly download the fastTextR-zip file using the **Clone or download** button in the [repository page](https://github.com/mlampros/fastTextR), extract it locally (rename it to *fastTextR* if necessary and check that files such as DESCRIPTION, NAMESPACE etc. are present when you open the fastTextR folder) and then run,

<br>

```R

#-------------
# on a Unix OS
#-------------

setwd('/your_folder/fastTextR/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('/your_folder/')
system("R CMD build fastTextR")
system("R CMD INSTALL fastTextR_1.0.2.tar.gz")


#------------------
# on the Windows OS  
#------------------

setwd('C:/your_folder/fastTextR/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('C:/your_folder/')
system("R CMD build fastTextR")
system("R CMD INSTALL fastTextR_1.0.2.tar.gz")

```
<br>

Use the following link to report bugs/issues (for the R wrapper),
<br><br>

[https://github.com/mlampros/fastTextR/issues](https://github.com/mlampros/fastTextR/issues)


<br>

#### **Example usage**


<br>

```R


# example input data ---> 'dat.txt'



library(fastTextR)



#--------------------------
# skipgram or cbow methods
#--------------------------


res = skipgram_cbow(input_path = "/data_fasttext/dat.txt",

                    output_path = "/data_fasttext/model", 
                    
                    method = "skipgram", lr = 0.1, 
                    
                    lrUpdateRate = 100, dim = 100,
                    
                    ws = 5, epoch = 5, minCount = 1, 
                    
                    neg = 5, wordNgrams = 1, loss = "ns", 
                    
                    bucket = 2000000, minn = 0,
                    
                    maxn = 0, thread = 6, t = 0.0001, 
                    
                    verbose = 2)
                    
                    
                
#-------------------------------------------------------------
# prediction of unknown words for the skipgram and cbow models
#-------------------------------------------------------------


res = predict_unknown_words(skipgram_cbow_model_output = "/data_fasttext/model.bin",

                            unknown_words_path = "/data_fasttext/queries.txt",
                            
                            output_path = "/data_fasttext/NEW_VEC",
                            
                            verbose = TRUE)

```

<br>

More information about the parameters of each function can be found in the package documentation.


<br>
