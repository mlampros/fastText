# include <Rcpp.h>
// [[Rcpp::depends("Rcpp")]]
// [[Rcpp::plugins(cpp11)]]


/**
 * Copyright (c) 2016-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */


//**************************************************************************************
// SEE the following threads for more information in case that I
// receive an "undefined symbol" error during compilation:
//
//      http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2017-August/009717.html
//      http://stackoverflow.com/questions/26032039/convert-vectorstring-into-char-c
//      https://stackoverflow.com/q/3024197   [ for the previous main() function ]
//
// SEE also my old version of the fastTextR package
//
//**************************************************************************************


#include <iomanip>
#include <iostream>
#include <queue>
#include <stdexcept>
#include "args.h"
#include "fasttext.h"
#include "string.h"
#include <fstream>


using namespace fasttext;


//' Print Usage Information for all parameters
//'
//' @return It does not return a value but only prints the available parameters of the 'printUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printUsage()
//'
// [[Rcpp::export]]
void printUsage() {
  Rcpp::Rcout
      << "usage: fasttext <command> <args>\n\n"
      << "The commands supported by fasttext are:\n\n"
      << "  supervised              train a supervised classifier\n"
      << "  quantize                quantize a model to reduce the memory usage\n"
      << "  test                    evaluate a supervised classifier\n"
      << "  test-label              print labels with precision and recall scores\n"
      << "  predict                 predict most likely labels\n"
      << "  predict-prob            predict most likely labels with probabilities\n"
      << "  skipgram                train a skipgram model\n"
      << "  cbow                    train a cbow model\n"
      << "  print-word-vectors      print word vectors given a trained model\n"
      << "  print-sentence-vectors  print sentence vectors given a trained model\n"
      << "  print-ngrams            print ngrams given a trained model and word\n"
      << "  nn                      query for nearest neighbors\n"
      << "  analogies               query for analogies\n"
      << "  dump                    dump arguments,dictionary,input/output vectors\n"
      << std::endl;
}


//' Print Usage Information when the command equals to 'quantize'
//'
//' @return It does not return a value but only prints the available parameters of the 'printQuantizeUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printQuantizeUsage()
//'
// [[Rcpp::export]]
void printQuantizeUsage() {
  Rcpp::Rcout << "usage: fasttext quantize <args>" << std::endl;
}


//' Print Usage Information when the command equals to 'test'
//'
//' @return It does not return a value but only prints the available parameters of the 'printTestUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printTestUsage()
//'
// [[Rcpp::export]]
void printTestUsage() {
  Rcpp::Rcout
      << "usage: fasttext test <model> <test-data> [<k>] [<th>]\n\n"
      << "  <model>      model filename\n"
      << "  <test-data>  test data filename (if -, read from stdin)\n"
      << "  <k>          (optional; 1 by default) predict top k labels\n"
      << "  <th>         (optional; 0.0 by default) probability threshold\n"
      << std::endl;
}


//' Print Usage Information when the command equals to 'predict' or 'predict-prob'
//'
//' @return It does not return a value but only prints the available parameters of the 'printPredictUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printPredictUsage()
//'
// [[Rcpp::export]]
void printPredictUsage() {
  Rcpp::Rcout
      << "usage: fasttext predict[-prob] <model> <test-data> [<k>] [<th>]\n\n"
      << "  <model>      model filename\n"
      << "  <test-data>  test data filename (if -, read from stdin)\n"
      << "  <k>          (optional; 1 by default) predict top k labels\n"
      << "  <th>         (optional; 0.0 by default) probability threshold\n"
      << std::endl;
}


//' Print Usage Information when the command equals to 'test-label'
//'
//' @return It does not return a value but only prints the available parameters of the 'printTestLabelUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printTestLabelUsage()
//'
// [[Rcpp::export]]
void printTestLabelUsage() {
  Rcpp::Rcout
      << "usage: fasttext test-label <model> <test-data> [<k>] [<th>]\n\n"
      << "  <model>      model filename\n"
      << "  <test-data>  test data filename\n"
      << "  <k>          (optional; 1 by default) predict top k labels\n"
      << "  <th>         (optional; 0.0 by default) probability threshold\n"
      << std::endl;
}


//' Print Usage Information when the command equals to 'print-word-vectors'
//'
//' @return It does not return a value but only prints the available parameters of the 'printPrintWordVectorsUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printPrintWordVectorsUsage()
//'
// [[Rcpp::export]]
void printPrintWordVectorsUsage() {
  Rcpp::Rcout << "usage: fasttext print-word-vectors <model>\n\n"
            << "  <model>      model filename\n"
            << std::endl;
}


//' Print Usage Information when the command equals to 'print-sentence-vectors'
//'
//' @return It does not return a value but only prints the available parameters of the 'printPrintSentenceVectorsUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printPrintSentenceVectorsUsage()
//'
// [[Rcpp::export]]
void printPrintSentenceVectorsUsage() {
  Rcpp::Rcout << "usage: fasttext print-sentence-vectors <model>\n\n"
            << "  <model>      model filename\n"
            << std::endl;
}


//' Print Usage Information when the command equals to 'print-ngrams'
//'
//' @return It does not return a value but only prints the available parameters of the 'printPrintNgramsUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printPrintNgramsUsage()
//'
// [[Rcpp::export]]
void printPrintNgramsUsage() {
  Rcpp::Rcout << "usage: fasttext print-ngrams <model> <word>\n\n"
            << "  <model>      model filename\n"
            << "  <word>       word to print\n"
            << std::endl;
}


//' Print Usage Information when the command equals to 'nn'
//'
//' @return It does not return a value but only prints the available parameters of the 'printNNUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printNNUsage()
//'
// [[Rcpp::export]]
void printNNUsage() {
  Rcpp::Rcout << "usage: fasttext nn <model> <k>\n\n"
            << "  <model>      model filename\n"
            << "  <k>          (optional; 10 by default) predict top k labels\n"
            << std::endl;
}


//' Print Usage Information when the command equals to 'analogies'
//'
//' @return It does not return a value but only prints the available parameters of the 'printAnalogiesUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printAnalogiesUsage()
//'
// [[Rcpp::export]]
void printAnalogiesUsage() {
  Rcpp::Rcout << "usage: fasttext analogies <model> <k>\n\n"
            << "  <model>      model filename\n"
            << "  <k>          (optional; 10 by default) predict top k labels\n"
            << std::endl;
}


//' Print Usage Information when the command equals to 'dump'
//'
//' @return It does not return a value but only prints the available parameters of the 'printDumpUsage' function in the R session
//' @export
//' @examples
//'
//' library(fastText)
//'
//' printDumpUsage()
//'
// [[Rcpp::export]]
void printDumpUsage() {
  Rcpp::Rcout << "usage: fasttext dump <model> <option>\n\n"
            << "  <model>      model filename\n"
            << "  <option>     option from args,dict,input,output" << std::endl;
}



// write data to file
//

void write_to_file(std::ofstream& out, std::string pth, bool remove_previous_file = true) {
  bool file_exists = false;
  if (FILE *file = fopen(pth.c_str(), "r")) {       // https://stackoverflow.com/a/46292862
    fclose(file);
    file_exists = true;
  }
  if (remove_previous_file) {
    if (file_exists) {
      if ( remove( pth.c_str() ) != 0 ) {
        Rcpp::Rcout << "Error deleting the output-file !" << std::endl;
      }
    }
  }
  out.open(pth, std::ios::app);                     // https://stackoverflow.com/a/6932446
}



void quantize(const std::vector<std::string>& args) {
  Args a = Args();
  if (args.size() < 3) {
    printQuantizeUsage();
    a.printHelp();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- quantize function");
  }
  a.parseArgs(args);
  FastText fasttext;
  //........................................ I modified it from the initial code in    https://github.com/facebookresearch/fastText/blob/master/src/main.cc#L108  to take the input and return an .ftz file
  // parseArgs checks if a->output is given.
  // fasttext.loadModel(a.output + ".bin");
  //........................................
  fasttext.loadModel(a.input);
  fasttext.quantize(a);
  fasttext.saveModel(a.output);
  // exit(0);
}



void test(const std::vector<std::string>& args, std::string pth, bool remove_previous_file = true) {
  bool perLabel = args[1] == "test-label";

  if (args.size() < 4 || args.size() > 6) {
    perLabel ? printTestLabelUsage() : printTestUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- test function");
  }

  const auto& model = args[2];
  const auto& input = args[3];
  int32_t k = args.size() > 4 ? std::stoi(args[4]) : 1;
  real threshold = args.size() > 5 ? std::stof(args[5]) : 0.0;

  FastText fasttext;
  fasttext.loadModel(model);

  Meter meter;

  if (input == "-") {
    fasttext.test(std::cin, k, threshold, meter);
  } else {
    std::ifstream ifs(input);
    if (!ifs.is_open()) {
      Rcpp::Rcout << "Test file cannot be opened!" << std::endl;
      Rcpp::stop("EXIT_FAILURE -- main.cc file -- test function");
    }
    fasttext.test(ifs, k, threshold, meter);
  }

  if (perLabel) {
    Rcpp::Rcout << std::fixed << std::setprecision(6);

    //------------------------------------------------------------------ 'writeMetric' function
    auto writeMetric = [](const std::string& name, double value) {
      Rcpp::Rcout << name << " : ";
      if (std::isfinite(value)) {
        Rcpp::Rcout << value;
      } else {
        Rcpp::Rcout << "--------";
      }
      Rcpp::Rcout << "  ";
    };
    //------------------------------------------------------------------

    std::ofstream out;
    write_to_file(out, pth, remove_previous_file);         // write data to file in case that the command is "test-label"

    std::shared_ptr<const Dictionary> dict = fasttext.getDictionary();

    if (pth != "") {

      for (int32_t labelId = 0; labelId < dict->nlabels(); labelId++) {

        std::streambuf *coutbuf = Rcpp::Rcout.rdbuf();                     // save old buf                      [ https://stackoverflow.com/a/10151286 ]
        Rcpp::Rcout.rdbuf(out.rdbuf());                                    // redirect Rcpp::Rcout to out! ( initially it was std::cout )

        //-------------------------------------------------------------- initial code
        writeMetric("F1-Score", meter.f1Score(labelId));
        writeMetric("Precision", meter.precision(labelId));
        writeMetric("Recall", meter.recall(labelId));
        Rcpp::Rcout << " " << dict->getLabel(labelId) << std::endl;
        //--------------------------------------------------------------

        Rcpp::Rcout.rdbuf(coutbuf);                                        //reset to standard output again
      }
    }
  }

  meter.writeGeneralMetrics(Rcpp::Rcout, k);    // applies to both 'test' and 'test-label' commands [ print the general metrics to the console ]

  // exit(0);
}



void printPredictions(
    const std::vector<std::pair<real, std::string>>& predictions,
    bool printProb,
    bool multiline,
    std::ofstream& out,
    std::string pth,
    bool newline = false) {

  if (pth != "") {

    std::stringstream stream;

    bool first = true;
    for (const auto& prediction : predictions) {
      if (!first && !multiline) {
        // std::cout << " ";
        stream << " ";
      }
      first = false;
      // std::cout << prediction.second;
      stream << prediction.second;
      if (printProb) {
        // std::cout << " " << prediction.first;
        stream << " " << prediction.first;
      }
      if (multiline) {
        // std::cout << std::endl;
        stream << "\n";
      }
    }
    if (!multiline) {
      // std::cout << std::endl;
      stream << "\n";
    }
    if (newline) {
      stream << "\n";
    }

    out << stream.str();
  }
  else {

    Rcpp::stop("The function returns only if the 'pth' parameter is specified!");
  }
}



void predict(const std::vector<std::string>& args, std::string pth = "", bool remove_previous_file = true) {

  if (pth == "") {
    Rcpp::stop("The user should specify the path_output file in case of the 'predict' function!");
  }

  if (args.size() < 4 || args.size() > 6) {
    printPredictUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- predict function");
  }
  int32_t k = 1;
  real threshold = 0.0;
  if (args.size() > 4) {
    k = std::stoi(args[4]);
    if (args.size() == 6) {
      threshold = std::stof(args[5]);
    }
  }

  bool printProb = args[1] == "predict-prob";
  FastText fasttext;
  fasttext.loadModel(std::string(args[2]));

  std::ifstream ifs;
  std::string infile(args[3]);
  bool inputIsStdIn = infile == "-";
  if (!inputIsStdIn) {
    ifs.open(infile);
    if (!inputIsStdIn && !ifs.is_open()) {
      Rcpp::Rcout << "Input file cannot be opened!" << std::endl;
      Rcpp::stop("EXIT_FAILURE -- main.cc file -- predict function");
    }
  }

  std::ofstream out;
  write_to_file(out, pth, remove_previous_file);         // write data to file

  std::istream& in = inputIsStdIn ? std::cin : ifs;
  std::vector<std::pair<real, std::string>> predictions;
  while (fasttext.predictLine(in, predictions, k, threshold)) {
    printPredictions(predictions, printProb, false, out, pth);
  }
  if (ifs.is_open()) {
    ifs.close();
  }
  // exit(0);
}



void printWordVectors(const std::vector<std::string> args, std::string pth_input, std::string pth_output, bool remove_previous_file = true) {

  if (pth_input == "" || pth_output == "") {
    Rcpp::stop("The user should specify the path_input and path_output files in case of the 'print-word-vectors' function!");
  }

  std::ifstream infile(pth_input);

  if (args.size() != 3) {
    printPrintWordVectorsUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- printWordVectors function");
  }
  FastText fasttext;
  fasttext.loadModel(std::string(args[2]));

  std::ofstream out;
  write_to_file(out, pth_output, remove_previous_file);         // write data to file

  std::string word;
  Vector vec(fasttext.getDimension());

  std::stringstream stream;

  while (infile >> word) {
    fasttext.getWordVector(vec, word);
    stream << word << " " << vec << "\n";
    // std::cout << word << " " << vec << std::endl;
  }

  out << stream.str();

  // exit(0);
}


//---------------------------------------------------------------------------------------------------
// NOTES on "printSentenceVectors":
//
//    .... Now, it is important to keep in mind that any sentence will end with a newline.
//         That means "one two" actually translates into the vectors for "one", "two" and EOS ....
//         [ EOS here means </s> , SEE also : const std::string Dictionary::EOS = "</s>";
//           In the "dictionary.cc" file ]
//
//    Therefore, the 'input.txt' file should consist of sentences of the following form:
//
//            How much does potato starch affect a cheese sauce recipe</s>
//            Dangerous pathogens capable of growing in acidic environments</s>
//            How do I cover up the white spots on my cast iron stove</s>
//            How do I cover up the white spots on my cast iron stove</s>
//            Michelin Three Star Restaurant but if the chef is not there</s>
//
//    The '\n' (newline character) might be needed too. However, for these examples it worked also
//    without it. Moreover, to find out if the function returns the correct results add the same
//    sentence twice (as I've done with the 3rd sentence) and observe if the function returns the same
//    numeric word vectors.
//
//    BE AWARE also that if at the end of the file a newline exists then the function will return an
//    additional word vector, THUS make sure that the input file does not include empty lines at the
//    end of the file.
//
//
// References :
//    https://github.com/facebookresearch/fastText/issues/323#issuecomment-353167113
//---------------------------------------------------------------------------------------------------

void printSentenceVectors(const std::vector<std::string> args, std::string pth_input, std::string pth_output, bool remove_previous_file = true) {

  if (pth_input == "" || pth_output == "") {
    Rcpp::stop("The user should specify the path_input and path_output files in case of the 'print-sentence-vectors' function!");
  }

  std::ifstream infile(pth_input);

  if (args.size() != 3) {
    printPrintSentenceVectorsUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- printSentenceVectors function");
  }
  FastText fasttext;
  fasttext.loadModel(std::string(args[2]));

  std::ofstream out;
  write_to_file(out, pth_output, remove_previous_file);         // write data to file

  std::stringstream stream;

  Vector svec(fasttext.getDimension());

  // while (std::cin.peek() != EOF) {
  while (infile.peek() != EOF) {                   // https://stackoverflow.com/a/6283787

    // fasttext.getSentenceVector(std::cin, svec);
    fasttext.getSentenceVector(infile, svec);      // here "infile" maps to "std::cin"
    // Don't print sentence
    // std::cout << svec << std::endl;
    stream << svec << "\n";
  }
  out << stream.str();

  // exit(0);
}



void printNgrams(const std::vector<std::string> args, std::string pth = "", bool remove_previous_file = true) {
  if (args.size() != 4) {
    printPrintNgramsUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- printNgrams function");
  }
  FastText fasttext;
  fasttext.loadModel(std::string(args[2]));

  std::ofstream out;
  if (pth != "") {
    write_to_file(out, pth, remove_previous_file);         // write data to file
  }

  std::string word(args[3]);
  std::vector<std::pair<std::string, Vector>> ngramVectors = fasttext.getNgramVectors(word);

  if (pth == "") {
    for (const auto& ngramVector : ngramVectors) {
      Rcpp::Rcout << ngramVector.first << " " << ngramVector.second << std::endl;
    }
  }
  else {
    std::stringstream stream;
    for (const auto& ngramVector : ngramVectors) {
      stream << ngramVector.first << " " << ngramVector.second << std::endl;
    }
    out << stream.str();
  }
  // exit(0);
}



void nn(const std::vector<std::string> args, std::string queryWord, std::string pth_output, bool remove_previous_file = true) {

  if (pth_output == "") {
    Rcpp::stop("The user should specify the path_output file in case of the 'nn' function!");
  }

  int32_t k;
  if (args.size() == 3) {
    k = 10;
  } else if (args.size() == 4) {
    k = std::stoi(args[3]);
  } else {
    printNNUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- nn function");
  }
  FastText fasttext;
  fasttext.loadModel(std::string(args[2]));
  // std::string prompt("Query word? ");
  // std::cout << prompt;

  std::ofstream out;
  write_to_file(out, pth_output, remove_previous_file);         // write data to file

  printPredictions(fasttext.getNN(queryWord, k), true, true, out, pth_output);

  // std::string queryWord;
  // while (std::cin >> queryWord) {
  //   printPredictions(fasttext.getNN(queryWord, k), true, true, out, pth_output);
  //   // std::cout << prompt;
  // }
  // exit(0);
}



void analogies(const std::vector<std::string> args, std::string pth_input, std::string pth_output, bool remove_previous_file = true) {

  if (pth_input == "" || pth_output == "") {
    Rcpp::stop("The user should specify the path_input and path_output files in case of the 'analogies' function!");
  }

  std::ifstream infile(pth_input);

  int32_t k;
  if (args.size() == 3) {
    k = 10;
  } else if (args.size() == 4) {
    k = std::stoi(args[3]);
  } else {
    printAnalogiesUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- analogies function");
  }
  if (k <= 0) {
    throw std::invalid_argument("k needs to be 1 or higher!");
  }

  std::ofstream out;
  write_to_file(out, pth_output, remove_previous_file);         // write data to file

  FastText fasttext;
  std::string model(args[2]);
  Rcpp::Rcout << "Loading model " << model << std::endl;
  fasttext.loadModel(model);

  // std::string prompt("Query triplet (A - B + C)? ");
  std::string wordA, wordB, wordC;
  // std::cout << prompt;
  while (infile >> wordA >> wordB >> wordC) {
  // while (true) {
  //   std::cin >> wordA;
  //   std::cin >> wordB;
  //   std::cin >> wordC;
    printPredictions(fasttext.getAnalogies(k, wordA, wordB, wordC), true, true, out, pth_output, true);      // use "newline = true" to separate the output-analogies for each triplet!

    // std::cout << prompt;
  }
  // exit(0);
}



void train(const std::vector<std::string> args, std::string pth = "", int MilliSecs = 100) {     // reduce the MilliSecs to increase the verbosity if the 'pth' parameter is not ""
  Args a = Args();
  a.parseArgs(args);
  FastText fasttext;
  std::string outputFileName(a.output + ".bin");
  std::ofstream ofs(outputFileName);
  if (!ofs.is_open()) {
    throw std::invalid_argument(
        outputFileName + " cannot be opened for saving.");
  }
  ofs.close();
  fasttext.train(a, pth, MilliSecs);
  fasttext.saveModel(outputFileName);
  fasttext.saveVectors(a.output + ".vec");
  if (a.saveOutput) {
    fasttext.saveOutput(a.output + ".output");
  }
}



void dump(const std::vector<std::string>& args, std::string pth, bool remove_previous_file = true) {

  if (pth == "") {
    Rcpp::stop("The user should specify a path file in case of the 'dump' function!");
  }

  if (args.size() < 4) {
    printDumpUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- dump function");
  }

  std::string modelPath = args[2];
  std::string option = args[3];

  std::ofstream out;
  write_to_file(out, pth, remove_previous_file);         // write data to file

  FastText fasttext;
  fasttext.loadModel(modelPath);

  std::streambuf *coutbuf = Rcpp::Rcout.rdbuf();                     // save old buf                      [ https://stackoverflow.com/a/10151286 ]
  Rcpp::Rcout.rdbuf(out.rdbuf());                                    // redirect Rcpp::Rcout to out! ( initially it was std::cout )

  if (option == "args") {
    fasttext.getArgs().dump(Rcpp::Rcout);
  } else if (option == "dict") {
    fasttext.getDictionary()->dump(Rcpp::Rcout);
  } else if (option == "input") {
    if (fasttext.isQuant()) {
      Rcpp::Rcout << "Not supported for quantized models." << std::endl;
    } else {
      fasttext.getInputMatrix()->dump(Rcpp::Rcout);
    }
  } else if (option == "output") {
    if (fasttext.isQuant()) {
      Rcpp::Rcout << "Not supported for quantized models." << std::endl;
    } else {
      fasttext.getOutputMatrix()->dump(Rcpp::Rcout);
    }
  } else {
    printDumpUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- dump function");
  }

  Rcpp::Rcout.rdbuf(coutbuf);
}


// int main(int argc, char** argv) {                                // initial function using 'argc', 'argv'
//   std::vector<std::string> args(argv, argv + argc);
//   if (args.size() < 2) {
//     printUsage();
//     Rcpp::stop("EXIT_FAILURE -- main.cc file -- main function");
//   }
//   std::string command(args[1]);
//   if (command == "skipgram" || command == "cbow" || command == "supervised") {
//     train(args);
//   } else if (command == "test" || command == "test-label") {
//     test(args);
//   } else if (command == "quantize") {
//     quantize(args);
//   } else if (command == "print-word-vectors") {
//     printWordVectors(args);
//   } else if (command == "print-sentence-vectors") {
//     printSentenceVectors(args);
//   } else if (command == "print-ngrams") {
//     printNgrams(args);
//   } else if (command == "nn") {
//     nn(args);
//   } else if (command == "analogies") {
//     analogies(args);
//   } else if (command == "predict" || command == "predict-prob") {
//     predict(args);
//   } else if (command == "dump") {
//     dump(args);
//   } else {
//     printUsage();
//     Rcpp::stop("EXIT_FAILURE -- main.cc file -- main function");
//   }
//   return 0;
// }


//' The Rcpp function which is used in the 'fasttext_interface' R function
//'
//' @param args the arguments that will be passed to the function in form of a character vector
//' @param pth a character string specifying the path where the process-logs (or output in generally) should be saved
//' @param MilliSecs an integer specifying the delay in milliseconds when printing the results to the specified path_output
//' @param pth_in a character string specifying the path to the input data file
//' @param queryWord either an empty string or the queryword that should be passed to the function
//' @param remove_previous_file a boolean. If TRUE, in case that the path_output is not an empty string (""), then an existing file with the same output name will be removed
//' @return It does not return a value but only saves the results to a file
//'
//' @keywords internal
//'
// [[Rcpp::export]]
void give_args_fasttext(std::vector<std::string> args,
                        std::string pth = "",
                        int MilliSecs = 100,
                        std::string pth_in = "",
                        std::string queryWord = "",
                        bool remove_previous_file = true) {

  if (args.size() < 2) {
    printUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- give_args_fasttext function");
  }
  std::string command(args[1]);
  if (command == "skipgram" || command == "cbow" || command == "supervised") {
    train(args, pth, MilliSecs);
  } else if (command == "test" || command == "test-label") {
    test(args, pth, remove_previous_file);
  } else if (command == "quantize") {
    quantize(args);
  } else if (command == "print-word-vectors") {
    printWordVectors(args, pth_in, pth, remove_previous_file);
  } else if (command == "print-sentence-vectors") {
    printSentenceVectors(args, pth_in, pth, remove_previous_file);
  } else if (command == "print-ngrams") {
    printNgrams(args, pth, remove_previous_file);
  } else if (command == "nn") {
    nn(args, queryWord, pth, remove_previous_file);
  } else if (command == "analogies") {
    analogies(args, pth_in, pth, remove_previous_file);
  } else if (command == "predict" || command == "predict-prob") {
    predict(args, pth, remove_previous_file);
  } else if (command == "dump") {
    dump(args, pth, remove_previous_file);
  } else {
    printUsage();
    Rcpp::stop("EXIT_FAILURE -- main.cc file -- give_args_fasttext function");
  }
}
