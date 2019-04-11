
if (.Platform$OS.type == "windows") {
  
  paste_delim = "\\"
}

if (.Platform$OS.type == "unix") {
  
  paste_delim = "/"
}


context('fasttext functions')


#--------------------------
# 'skipgram_cbow' function
#--------------------------


testthat::test_that("it returns an error if the input_path parameter is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data1", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                                   
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                                   
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the output_path parameter is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data1", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the method parameter is not one of c('skipgram', 'cbow')", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "unknown", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})


testthat::test_that("it returns an error if the learning rate parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.0, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the lrUpdateRate parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 0, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})


testthat::test_that("it returns an error if the dim parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 0,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})


testthat::test_that("it returns an error if the ws parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 0, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the epoch parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 0, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the minCount parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 0, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the neg parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 0, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the wordNgrams parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 0, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the loss parameter is not one of c('ns', 'hs', 'softmax')", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "unknown", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})


testthat::test_that("it returns an error if the bucket parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 0, minn = 0, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})




testthat::test_that("it returns an error if the minn parameter is less than 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = -1, 
                                        
                                        maxn = 0, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the maxn parameter is less than 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = -1, thread = 6, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the thread parameter is less than 1", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 0, t = 0.0001, verbose = 2)  )
})



testthat::test_that("it returns an error if the t parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 1, t = 0.0, verbose = 2)  )
})



testthat::test_that("it returns an error if the verbose parameter is less 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 1, t = 0.0001, verbose = -1)  )
})



testthat::test_that("it returns an error if the verbose parameter is greater than 2", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  testthat::expect_error( skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                                        
                                        ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                                        
                                        maxn = 0, thread = 1, t = 0.0001, verbose = 3)  )
})



testthat::test_that("it saves the output to a file if all parameters are valid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "model"), collapse = paste_delim)))
  
  res =  skipgram_cbow(input_path = path_in, output_path = path_out, method = "skipgram", lr = 0.1, lrUpdateRate = 100, dim = 10,
                       
                       ws = 5, epoch = 5, minCount = 1, neg = 5, wordNgrams = 1, loss = "ns", bucket = 2000000, minn = 0, 
                       
                       maxn = 0, thread = 1, t = 0.0001, verbose = 0)  
  
  testthat::expect_silent(res)
})




#---------------------------------
# 'predict_unknown_words' function
#---------------------------------


testthat::test_that("it returns an error if the skipgram_cbow_model_output parameter is not a valid character string path", {

  testthat::expect_error( predict_unknown_words(skipgram_cbow_model_output = NULL) )
})


testthat::test_that("it returns an error if the skipgram_cbow_model_output parameter is not a valid character string path", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "model.bin"), collapse = paste_delim)))
  
  testthat::expect_error( predict_unknown_words(skipgram_cbow_model_output = path_in, unknown_words_path = NULL) )
})


testthat::test_that("it returns an error if the unknown_words_path parameter is not a valid character string path", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "model.bin"), collapse = paste_delim)))
  
  path_unkn = paste0(getwd(), path.expand(paste(c("", "test_data", "queries.txt"), collapse = paste_delim)))
  
  testthat::expect_error( predict_unknown_words(skipgram_cbow_model_output = path_in, unknown_words_path = path_unkn, output_path = NULL) )
})


testthat::test_that("it returns an error if the output_path parameter is not a valid character string path", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "model.bin"), collapse = paste_delim)))
  
  path_unkn = paste0(getwd(), path.expand(paste(c("", "test_data", "queries.txt"), collapse = paste_delim)))
  
  testthat::expect_error( predict_unknown_words(skipgram_cbow_model_output = path_in, unknown_words_path = path_unkn, output_path = list()) )
})


testthat::test_that("it returns an error if the verbose parameter is not a boolean", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "model.bin"), collapse = paste_delim)))
  
  path_unkn = paste0(getwd(), path.expand(paste(c("", "test_data", "queries.txt"), collapse = paste_delim)))
  
  path_res_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "VECS.txt"), collapse = paste_delim)))
  
  testthat::expect_error( predict_unknown_words(skipgram_cbow_model_output = path_in, unknown_words_path = path_unkn, output_path = path_res_vecs, verbose = 'FALSE') )
})



testthat::test_that("it returns word vectors to the specified folder", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "model.bin"), collapse = paste_delim)))
  
  path_unkn = paste0(getwd(), path.expand(paste(c("", "test_data", "queries.txt"), collapse = paste_delim)))
  
  path_res_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "VECS"), collapse = paste_delim)))
  
  testthat::expect_silent( predict_unknown_words(skipgram_cbow_model_output = path_in, unknown_words_path = path_unkn, output_path = path_res_vecs, verbose = FALSE) )
})

