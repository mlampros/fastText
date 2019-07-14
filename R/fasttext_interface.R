
utils::globalVariables(c("progress", "loss", "learning_rate", "words_sec_thread"))           # to avoid the NOTE : no visible binding for global variables ....


#' Interface for the fasttext library
#'
#'
#' @param list_params a list of valid parameters
#' @param path_output a character string specifying the path where the process-logs (or output in generally) should be saved
#' @param path_input a character string specifying the path to the input data file
#' @param MilliSecs an integer specifying the delay in milliseconds when printing the results to the specified \emph{path_output}
#' @param remove_previous_file a boolean. If TRUE, in case that the \emph{path_output} is not an empty string (""), then an existing file with the same output name will be removed
#' @param print_process_time a boolean. If TRUE then the processing time of the function will be printed out in the R session
#' @details
#' This function allows the user to run the various methods included in the fasttext library from within R
#' @references
#' https://github.com/facebookresearch/fastText
#'
#' https://github.com/facebookresearch/fastText/blob/master/docs/supervised-tutorial.md
#' @export
#' @examples
#'
#' \donttest{
#'
#' library(fastText)
#'
#'
#' ####################################################################################
#' # If the user intends to run the following examples then he / she must replace     #
#' # the 'input', 'output', 'path_input', 'path_output', 'model' and 'test_data' file #
#' # paths depending on where the data are located or should be saved!                #
#' # ( 'tempdir()' is used here as an example folder )                                #
#' ####################################################################################
#'
#'
#' # ------------------------------------------------
#' # print information for the Usage of each function [ parameters ]
#' # ------------------------------------------------
#'
#' fastText::printUsage()
#' fastText::printTestUsage()
#' fastText::printTestLabelUsage()
#' fastText::printQuantizeUsage()
#' fastText::printPrintWordVectorsUsage()
#' fastText::printPrintSentenceVectorsUsage()
#' fastText::printPrintNgramsUsage()
#' fastText::printPredictUsage()
#' fastText::printNNUsage()
#' fastText::printDumpUsage()
#' fastText::printAnalogiesUsage()
#'
#' # -----------------------------------------------------------------------
#' # In case that the 'command' is one of 'cbow', 'skipgram' or 'supervised'
#' # -----------------------------------------------------------------------
#'
#' list_params = list(command = 'cbow',
#'                    lr = 0.1,
#'                    dim = 200,
#'                    input = file.path(tempdir(), "doc.txt"),
#'                    output = tempdir(),
#'                    verbose = 2,
#'                    thread = 1)
#'
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(),"model_logs.txt"),
#'                          MilliSecs = 100)
#'
#'
#' # ---------------------
#' # 'supervised' training
#' # ---------------------
#'
#' list_params = list(command = 'supervised',
#'                     lr = 0.1,
#'                     dim = 200,
#'                     input = file.path(tempdir(), "cooking.train"),
#'                     output = file.path(tempdir(), "model_cooking"),
#'                     verbose = 2,
#'                     thread = 1)
#'
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(), 'logs_supervise.txt'),
#'                          MilliSecs = 5)
#'
#' # ---------------------------------------
#' # In case that the 'command' is 'predict'
#' # ---------------------------------------
#'
#' list_params = list(command = 'predict',
#'                    model = file.path(tempdir(), 'model_cooking.bin'),
#'                    test_data = file.path(tempdir(), 'cooking.valid'),
#'                    k = 1,
#'                    th = 0.0)
#'
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(), 'predict_valid.txt'))
#'
#'
#' # ------------------------------------
#' # In case that the 'command' is 'test'  [ k = 5 , means that precision and recall are at 5 ]
#' # ------------------------------------
#'
#' list_params = list(command = 'test',
#'                    model = file.path(tempdir(), 'model_cooking.bin'),
#'                    test_data = file.path(tempdir(), 'cooking.valid'),
#'                    k = 5,
#'                    th = 0.0)
#'
#' res = fasttext_interface(list_params)   # It only prints 'Precision', 'Recall' to the R session
#'
#'
#' # ------------------------------------------
#' # In case that the 'command' is 'test-label'   [ k = 5 , means that precision and recall are at 5 ]
#' # ------------------------------------------
#'
#' list_params = list(command = 'test-label',
#'                    model = file.path(tempdir(), 'model_cooking.bin'),
#'                    test_data = file.path(tempdir(), 'cooking.valid'),
#'                    k = 5,
#'                    th = 0.0)
#'
#' res = fasttext_interface(list_params,              # prints also 'Precision', 'Recall' to R session
#'                          path_output = file.path(tempdir(), "test_valid.txt"))
#'
#' # -----------------
#' # quantize function  [ it will create an .ftz file ]
#' # -----------------
#'
#' # the quantize function is currenlty (01/02/2019) single-threaded
#' # https://github.com/facebookresearch/fastText/issues/353#issuecomment-342501742
#'
#' list_params = list(command = 'quantize',
#'                    input = file.path(tempdir(), 'model_cooking.bin'),
#'                    output = tempdir())
#'
#' res = fasttext_interface(list_params)
#'
#'
#' # ------------------
#' # print-word-vectors   [ each line of the 'queries.txt' must be a single word ]
#' # ------------------
#'
#' list_params = list(command = 'print-word-vectors',
#'                    model = file.path(tempdir(), 'model_cooking.bin'))
#'
#' res = fasttext_interface(list_params,
#'                          path_input = file.path(tempdir(), 'queries.txt'),
#'                          path_output = file.path(tempdir(), 'print_vecs_file.txt'))
#'
#'
#' # ----------------------
#' # print-sentence-vectors   [ See also the comments in the main.cc file about the input-file ]
#' # ----------------------
#'
#' list_params = list(command = 'print-sentence-vectors',
#'                    model = file.path(tempdir(), 'model_cooking.bin'))
#'
#' res = fasttext_interface(list_params,
#'                          path_input = file.path(tempdir(), 'text.txt'),
#'                          path_output = file.path(tempdir(), 'SENTENCE_VECs.txt'))
#'
#'
#' # ------------
#' # print-ngrams       [ print to console or to output-file ]
#' # ------------
#'
#' list_params = list(command = 'skipgram', lr = 0.1, dim = 200,
#'                    input = file.path(tempdir(), "doc.txt"),
#'                    output = tempdir(), verbose = 2, thread = 1,
#'                    minn = 2, maxn = 2)
#'
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(), "ngram_out.txt"),
#'                          MilliSecs = 5)
#'
#' list_params = list(command = 'print-ngrams',
#'                    model = file.path(tempdir(), 'ngram_out.bin'),
#'                    word = 'word')                           # print n-grams for specific word
#'
#' res = fasttext_interface(list_params, path_output = "")             # print output to console
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(), "NGRAMS.txt"))   # output to file
#'
#'
#' # -------------
#' # 'nn' function
#' # -------------
#'
#' list_params = list(command = 'nn',
#'                    model = file.path(tempdir(), 'model_cooking.bin'),
#'                    k = 20,
#'                    query_word = 'word')          # a 'query_word' is required
#'
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(), "nn_output.txt"))
#'
#'
#' # ---------
#' # analogies   [ in the output file each analogy-triplet-result is separated with a newline ]
#' # ---------
#'
#' list_params = list(command = 'analogies',
#'                    model = file.path(tempdir(), 'model_cooking.bin'),
#'                    k = 5)
#'
#' res = fasttext_interface(list_params,
#'                          path_input = file.path(tempdir(), 'analogy_queries.txt'),
#'                          path_output = file.path(tempdir(), 'analogies_output.txt'))
#'
#' # -------------
#' # dump function  [ the 'option' param should be one of 'args', 'dict', 'input' or 'output' ]
#' # -------------
#'
#' list_params = list(command = 'dump',
#'                    model = file.path(tempdir(), 'model_cooking.bin'),
#'                    option = 'args')
#'
#' res = fasttext_interface(list_params,
#'                          path_output = file.path(tempdir(), "DUMP.txt"))
#'
#' }

fasttext_interface = function(list_params,
                              path_output = "",
                              MilliSecs = 100,
                              path_input = "",
                              remove_previous_file = TRUE,
                              print_process_time = FALSE) {

  if (print_process_time) { start = Sys.time() }

  if (!'command' %in% names(list_params)) stop("The input 'list_params' argument should include the 'command' parameter!", call. = F)

  valid_methods = c("skipgram", "cbow", "supervised", "test", "test-label",
                    "quantize", "print-word-vectors", "print-sentence-vectors",
                    "print-ngrams", "nn", "analogies", "predict", "predict-prob", "dump")

  if (!list_params['command'] %in% valid_methods) stop('A valid "command" should be one of "skipgram", "cbow", "supervised", "test", "test-label",
                                                       "quantize", "print-word-vectors", "print-sentence-vectors", "print-ngrams", "nn", "analogies",
                                                       "predict", "predict-prob" or "dump" !', call. = F)
  if (!remove_previous_file) {
    if (path_output != "") {
      if (file.exists(path_output)) {
        warning("The specified 'path_output' file already exists! Data will be appended at the end of the file! You can either set the 'remove_previous_file' parameter to
                TRUE or remove any previous files before using this function, otherwise the new data will be accumulated increasing that way the size of the file!", call. = F)
      }
    }
  }

  default_arg = "fasttext"
  default_cmd = list_params[['command']]
  list_params['command'] = NULL

  if (default_cmd == 'predict' || default_cmd == 'predict-prob' || default_cmd == 'test' || default_cmd == 'test-label') {
    if (!all(names(list_params) %in% c('model', 'test_data', 'k', 'th'))) {
      stop("Valid parameters in case of the 'predict' command are : 'model', 'test_data', 'k' and 'th'", call. = F)
    }
    input_args = c(default_arg, default_cmd, as.vector(unlist(list_params)))
    give_args_fasttext(input_args, path_output, MilliSecs, "", "", remove_previous_file)
  }
  else if (default_cmd == 'print-word-vectors' || default_cmd == 'print-sentence-vectors') {
    if (!'model' %in% names(list_params)) {
      stop("The 'model' argument must be included in the list of parameters!", call. = F)
    }
    if (length(list_params) > 1) {
      stop("Only the 'command' and 'model' arguments should be included in the list in case of 'print-word-vectors' or 'print-sentence-vectors' command!", call. = F)
    }
    input_args = c(default_arg, default_cmd, list_params[['model']])
    give_args_fasttext(input_args, path_output, MilliSecs, path_input, "", remove_previous_file)
  }
  else if (default_cmd == 'print-ngrams') {
    if (!all(c('model', 'word') %in% names(list_params))) {
      stop("The 'model' and 'word' arguments must be included in the list of parameters!", call. = F)
    }
    if (length(list_params) > 2) {
      stop("Only the 'command', 'model' and 'word' arguments should be included in the list in case of the 'print-ngrams' command!", call. = F)
    }
    input_args = c(default_arg, default_cmd, list_params[['model']], list_params[['word']])
    give_args_fasttext(input_args, path_output, MilliSecs, path_input, "", remove_previous_file)
  }
  else if (default_cmd == 'nn') {
    if (!all(c('model', 'k', 'query_word') %in% names(list_params))) {
      stop("The 'model', 'k' and 'query_word' arguments must be included in the list of parameters!", call. = F)
    }
    if (length(list_params) > 3) {
      stop("Only the 'command', 'model', 'k' and 'query_word' arguments should be included in the list in case of the 'nn' command!", call. = F)
    }
    input_args = c(default_arg, default_cmd, list_params[['model']], list_params[['k']])
    give_args_fasttext(input_args, path_output, MilliSecs, path_input, list_params[['query_word']], remove_previous_file)
  }
  else if (default_cmd == 'analogies') {
    if (!all(c('model', 'k') %in% names(list_params))) {
      stop("The 'model' and 'k' arguments must be included in the list of parameters!", call. = F)
    }
    if (length(list_params) > 2) {
      stop("Only the 'command', 'model' and 'k' arguments should be included in the list in case of the 'analogies' command!", call. = F)
    }
    input_args = c(default_arg, default_cmd, list_params[['model']], list_params[['k']])
    give_args_fasttext(input_args, path_output, MilliSecs, path_input, "", remove_previous_file)
  }
  else if (default_cmd == 'dump') {
    if (!all(c('model', 'option') %in% names(list_params))) {
      stop("The 'model' and 'option' arguments must be included in the list of parameters!", call. = F)
    }
    if (length(list_params) > 2) {
      stop("Only the 'command', 'model' and 'option' arguments should be included in the list in case of the 'dump' command!", call. = F)
    }
    if (!list_params[['option']] %in% c('args', 'dict', 'input', 'output')) {
      stop("The 'option' argument must be one of 'args', 'dict', 'input' or 'output'", call. = F)
    }
    input_args = c(default_arg, default_cmd, list_params[['model']], list_params[['option']])
    give_args_fasttext(input_args, path_output, MilliSecs, path_input, "", remove_previous_file)
  }
  else {
    names_ = names(list_params)                     # get names of parameter-list
    values_ = as.vector(unlist(list_params))        # get values of parameter-list
    len_all = length(names_) + length(values_)      # length of both the 'names' and 'values'
    input_args = rep(NA, len_all + 2)               # + 2 due to the 'default_arg' and 'default_cmd'

    input_args[1] = default_arg
    input_args[2] = default_cmd

    count = 3                                       # begin from 3 because I've already added the 'default_arg' and 'default_cmd'
    for (i in 1:length(names_)) {

      input_args[count] = paste0('-', names_[i])
      count = count + 1

      input_args[count] = values_[i]
      count = count + 1
    }
    give_args_fasttext(input_args, path_output, MilliSecs, "", "", remove_previous_file)
  }

  if (print_process_time) {
    end = Sys.time()
    t = end - start
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }

  return(input_args)
}



#' Print the parameters for a specific command
#'
#'
#' @param command a character string specifying the command for which the parameters should be printed in the R session
#' @references
#' https://github.com/facebookresearch/fastText#full-documentation
#'
#' https://github.com/facebookresearch/fastText/issues/341#issuecomment-339783130
#' @export
#' @examples
#'
#' library(fastText)
#'
#' print_parameters(command = 'supervised')
#'

print_parameters = function(command = 'supervised') {

  try_c = tryCatch(give_args_fasttext(args = c('fasttext', command)), error = function(e) e)
  invisible()
}



#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @keywords internal
#' @importFrom grid grid.newpage
#' @importFrom grid grid.layout
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @references http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#' Plot the progress of loss, learning-rate and word-counts
#'
#'
#' @param path_logs a character string specifying a valid path to a file where the progress-logs are saved
#' @param plot a boolean specifying if the loss, learning-rate and word-counts should be plotted
#'
#' @references http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @importFrom utils read.table
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @export
#' @examples
#'
#' \donttest{
#'
#' library(fastText)
#'
#' #-----------------------------------------------------------------
#' # the 'progress_data.txt' file corresponds to the 'path_output'
#' # parameter of the 'fasttext_interface()'. Therefore the user has
#' # to run first the 'fasttext_interface()' function to save the
#' # 'progress_data.txt' file to the desired folder.
#' #-----------------------------------------------------------------
#'
#' res = plot_progress_logs(path = file.path(tempdir(), "progress_data.txt"),
#'                          plot = TRUE)
#'
#' }

plot_progress_logs = function(path_logs = "progress_data.txt",
                              plot = FALSE) {

  data = utils::read.table(path_logs, quote="\"", comment.char="", header = F, stringsAsFactors = F, fill = T)

  progress_ = as.vector(data$V2)
  progress_ = as.vector(sapply(progress_, function(x) as.numeric(strsplit(x, '%'))))
  words_sec_thread_ = as.vector(data$V4)
  lr_ = as.vector(data$V6)
  loss_ = as.vector(data$V8)

  df = data.frame(progress = progress_, words_sec_thread = words_sec_thread_, learning_rate = lr_, loss = loss_)
  dups = which(duplicated(df[, c(1,3,4)]))
  if (length(dups) > 0) {
    df = df[-dups, ]
  }

  if (plot) {

    # progress - loss
    p1 <- ggplot2::ggplot(df, ggplot2::aes(x=progress, y=loss)) +
      ggplot2::geom_line(colour='red') +
      ggplot2::ggtitle("progress of loss") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # progress - learning_rate
    p2 <- ggplot2::ggplot(df, ggplot2::aes(x=progress, y=learning_rate)) +
      ggplot2::geom_line(colour='blue') +
      ggplot2::ggtitle("progress of learning_rate") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # progress - words_sec_thread
    p3 <- ggplot2::ggplot(df, ggplot2::aes(x=progress, y=words_sec_thread)) +
      ggplot2::geom_line(colour='green') +
      ggplot2::ggtitle("progress of word_sec_thread") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    multiplot(p1, p2, p3, cols=2)
  }

  return(df)
}
