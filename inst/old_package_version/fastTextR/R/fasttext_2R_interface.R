

#' wrapper for the skipgram and cbow methods of the fasttext text classifier
#'
#'
#' @param input_path a character string specifying the path to the train text file
#' @param output_path a character string specifying the path to the output-file
#' @param method a string specifying the method. One of \emph{skipgram}, \emph{cbow}
#' @param lr a float number specifying the learning rate [default is 0.1]
#' @param lrUpdateRate a number specifying the rate of updates for the learning rate [default is 100]
#' @param dim a number specifying the size of the word vectors [default is 100]
#' @param ws a number specifying the size of the context window [default is 5]
#' @param epoch a value specifying the number of epochs [default is 5]
#' @param minCount a number specifying the minimal number of word occurences [default is 1]
#' @param neg a value specifying the number of negatives sampled [default is 5]
#' @param wordNgrams a number specifying the max length of word ngram [default is 1]
#' @param loss a character string specifying the loss function. One of \emph{ns (negative sampling)}, \emph{hs (hierarchical softmax)}, \emph{softmax} [default is 'ns']
#' @param bucket a value specifying the number of buckets [default is 2000000]
#' @param minn a number specifying the min length of char ngram [default is 0]
#' @param maxn a number specifying the max length of char ngram [default is 0]
#' @param thread a value specifying the of threads [default is 6]
#' @param t a float number specifying the sampling threshold [default is 0.0001]
#' @param verbose a number (between 0 and 2) specifying the verbosity level [default is 2]
#' @return a character string specifying the location of the saved data and the number of the word vectors
#' @export
#' @details 
#' the function will save a model.bin and the word vectors to a pre-specified path (\emph{output_path})
#' @references
#' https://github.com/facebookresearch/fastText
#' 
#' https://arxiv.org/abs/1607.04606
#' 
#' https://arxiv.org/abs/1607.01759
#' @examples
#'
#' # library(fastTextR)
#' 
#' # res = skipgram_cbow(input_path = "/data_fasttext/out_test_file.txt", 
#' 
#' #                     output_path = "/data_fasttext/model", method = "skipgram")


skipgram_cbow = function(input_path = NULL, output_path = NULL, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 100,

                         ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                         
                         maxn = 0, thread = 6, t = 0.0001, verbose = 2) {

  try_err_files = inherits(tryCatch(normalizePath(input_path, mustWork = T), error = function(e) e), "error")
  if (!is.character(input_path) || try_err_files) stop("the input_path parameter should be a valid character string path")
  if (is.null(output_path)) stop("the output_path parameter should be a non-NULL valid character string path")
  if (.Platform$OS.type == 'unix') {
    first = strsplit(output_path, "/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "/")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (.Platform$OS.type == 'windows') {
    first = strsplit(output_path, "\\\\|/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "\\")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (!is.character(output_path) || try_err_files_out) stop("the output_path parameter should be a valid character string path")
  type_bin = strsplit(first[length(first)], '[.]')[[1]]
  if (length(type_bin) > 1) stop("the output_path parameter should be a valid path with no file extensions. Example path:  ../model")
  if (!method %in% c('skipgram', 'cbow')) stop("the method parameter should be either 'skipgram' or 'cbow'")
  params = c(lr, t, lrUpdateRate, dim, ws, epoch, minCount, neg, wordNgrams, bucket)
  params_nams = c('lr', 't', 'lrUpdateRate', 'dim', 'ws', 'epoch', 'minCount', 'neg', 'wordNgrams', 'bucket')
  flag_params = sum(params <= 0)
  if (flag_params > 0) {
    idx = which(params <= 0)[1]
    stop(paste0("the ", paste0(params_nams[idx], " parameter should be a number greater than 0.0")))}
  if (minn < 0) stop("the minn parameter should be greater than 0")
  if (maxn < 0) stop("the maxn parameter should be greater than 0")
  if (verbose < 0 || verbose > 2) stop("the verbose parameter should be a number between 0 and 2")
  if (thread < 1) stop("the thread parameter should be greater than 0")
  if (!loss %in% c('ns', 'hs', 'softmax')) stop("the loss parameter should be one of 'ns', 'hs', 'softmax'")

  if (verbose > 0) { start = Sys.time() }

  default_args = c("fasttext", method, "-input", as.character(input_path), "-output", as.character(output_path), "-lr", as.character(lr),

                   "-lrUpdateRate", as.character(lrUpdateRate), "-dim", as.character(dim), "-ws", as.character(ws), "-epoch", as.character(epoch),

                   "-minCount", as.character(minCount), "-neg", as.character(neg), "-wordNgrams",

                   as.character(wordNgrams), "-loss", loss, "-bucket", as.character(bucket), "-minn", as.character(minn), "-maxn",

                   as.character(maxn), "-thread", as.character(thread), "-t", as.character(t), "-verbose", as.character(verbose))

  convert_args_to_pointers(default_args, "", "")
  
  if (verbose > 0) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }

  return(structure(list(file_location = paste0("the output is saved in: ", output_path), num_vectors = dim), 
                   
                   class = 'fasttextR'))
}



#' predict unknown words for the skipgram and cbow models
#' 
#' 
#' @param skipgram_cbow_model_output the output.bin of the \emph{skipgram_cbow} function
#' @param unknown_words_path a character string specifying the path to the unknown words file
#' @param output_path a character string specifying the path to the output file
#' @param verbose either TRUE or FALSE. If TRUE information will be printed out
#' @return a character string specifying the location of the saved data
#' @export
#' @details 
#' the function will save to a pre-specified path (\emph{output_path}) the numeric vectors for the unknown words. In case of non-matches it returns zero-valued vectors
#' @references
#' https://github.com/facebookresearch/fastText
#' 
#' https://arxiv.org/abs/1607.04606
#' 
#' https://arxiv.org/abs/1607.01759
#' @examples
#'
#' # library(fastTextR)
#' 
#' # res = predict_unknown_words(skipgram_cbow_model_output = "/data_fasttext/model.bin", 
#' 
#' #                             unknown_words_path = "/data_fasttext/queries.txt", 
#' 
#' #                             output_path = "/data_fasttext/NEW_VEC")


predict_unknown_words = function(skipgram_cbow_model_output = NULL, unknown_words_path = NULL, output_path = NULL, verbose = FALSE) {
  
  try_err_files = inherits(tryCatch(normalizePath(skipgram_cbow_model_output, mustWork = T), error = function(e) e), "error")
  if (!is.character(skipgram_cbow_model_output) || try_err_files) stop("the skipgram_cbow_model_output parameter should be a valid character string path")
  try_err_unknown = inherits(tryCatch(normalizePath(unknown_words_path, mustWork = T), error = function(e) e), "error")
  if (!is.character(unknown_words_path) || try_err_unknown) stop("the unknown_words_path parameter should be a valid character string path")
  if (is.null(output_path)) stop("the output_path parameter should be a non-NULL character string path")
  if (!is.null(output_path)) {
    if (!is.character(output_path)) {
      stop("the output_path parameter should be a valid character string path")}}
  if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")
  
  if (verbose) { start = Sys.time() }
  
  default_args = c("fasttext", "predict_skipgram_cbow", skipgram_cbow_model_output)
  
  convert_args_to_pointers(default_args, unknown_words_path, output_path)
  
  if (verbose) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }
  
  return(paste0("the output is saved in: ", output_path))
}


