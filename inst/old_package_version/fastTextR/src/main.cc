# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]


/**
 * Copyright (c) 2016-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <iostream>
#include<fstream>
#include "fasttext.h"
#include "args.h"

using namespace fasttext;

void printUsage() {
  std::cout
  << "usage: fasttext <command> <args>\n\n"
  << "The commands supported by fasttext are:\n\n"
  << "  supervised          train a supervised classifier\n"
  << "  test                evaluate a supervised classifier\n"
  << "  predict             predict most likely labels\n"
  << "  predict-prob        predict most likely labels with probabilities\n"
  << "  skipgram            train a skipgram model\n"
  << "  cbow                train a cbow model\n"
  << "  print-vectors       print vectors given a trained model\n"
  << std::endl;
}

void printTestUsage() {
  std::cout
  << "usage: fasttext test <model> <test-data> [<k>]\n\n"
  << "  <model>      model filename\n"
  << "  <test-data>  test data filename (if -, read from stdin)\n"
  << "  <k>          (optional; 1 by default) predict top k labels\n"
  << std::endl;
}

void printPredictUsage() {
  std::cout
  << "usage: fasttext predict[-prob] <model> <test-data> [<k>]\n\n"
  << "  <model>      model filename\n"
  << "  <test-data>  test data filename (if -, read from stdin)\n"
  << "  <k>          (optional; 1 by default) predict top k labels\n"
  << std::endl;
}

void printPrintVectorsUsage() {
  std::cout
  << "usage: fasttext print-vectors <model>\n\n"
  << "  <model>      model filename\n"
  << std::endl;
}

void test(int argc, char** argv) {
  int32_t k;
  if (argc == 4) {
    k = 1;
  } else if (argc == 5) {
    k = atoi(argv[4]);
  } else {
    printTestUsage();
    exit(EXIT_FAILURE);
  }
  FastText fasttext;
  fasttext.loadModel(std::string(argv[2]));
  std::string infile(argv[3]);
  if (infile == "-") {
    fasttext.test(std::cin, k);
  } else {
    std::ifstream ifs(infile);
    if (!ifs.is_open()) {
      std::cerr << "Test file cannot be opened!" << std::endl;
      exit(EXIT_FAILURE);
    }
    fasttext.test(ifs, k);
    ifs.close();
  }
  exit(0);
}

void predict(int argc, char** argv) {
  int32_t k;
  if (argc == 4) {
    k = 1;
  } else if (argc == 5) {
    k = atoi(argv[4]);
  } else {
    printPredictUsage();
    exit(EXIT_FAILURE);
  }
  bool print_prob = std::string(argv[1]) == "predict-prob";
  FastText fasttext;
  fasttext.loadModel(std::string(argv[2]));
  
  std::string infile(argv[3]);
  if (infile == "-") {
    fasttext.predict(std::cin, k, print_prob);
  } else {
    std::ifstream ifs(infile);
    if (!ifs.is_open()) {
      std::cerr << "Input file cannot be opened!" << std::endl;
      exit(EXIT_FAILURE);
    }
    fasttext.predict(ifs, k, print_prob);
    ifs.close();
  }
  
  exit(0);
}

void printVectors(int argc, char** argv) {
  if (argc != 3) {
    printPrintVectorsUsage();
    exit(EXIT_FAILURE);
  }
  FastText fasttext;
  fasttext.loadModel(std::string(argv[2]));
  fasttext.printVectors();
  exit(0);
}

void train(int argc, char** argv) {
  std::shared_ptr<Args> a = std::make_shared<Args>();
  a->parseArgs(argc, argv);
  FastText fasttext;
  fasttext.train(a);
}


void SAVE_dict_vectors(char** argv, std::string INPUT, std::string OUTPUT) {
  
  FastText fasttext;
  
  fasttext.loadModel(std::string(argv[2]));
  
  fasttext.wordVectors(INPUT, OUTPUT);
}



// wrapper for the train(), test(), printVectors(), predict() functions
// conversion of string-arguments to pointers in c++ : http://stackoverflow.com/questions/26032039/convert-vectorstring-into-char-c
// for supervised, modify the std::cin in predict(), test()
//

// [[Rcpp::export]]
void convert_args_to_pointers(std::vector<std::string> string_commands, std::string INPUT, std::string OUTPUT) {         // 'string_commands' includes also the fasttext argument in index 0;
  
  int num_argc = string_commands.size();
  
  utils::initTables();
  
  if (num_argc < 2) {
    
    printUsage();
    
    exit(EXIT_FAILURE);
  }
  
  char** cstrings = new char*[string_commands.size()];
  
  for(size_t i = 0; i < string_commands.size(); ++i) {
    
    cstrings[i] = new char[string_commands[i].size() + 1];
    
    std::strcpy(cstrings[i], string_commands[i].c_str());
  }
  
  std::string command = string_commands[1];
  
  if (command == "skipgram" || command == "cbow" || command == "supervised") {
    
    train(num_argc, cstrings);}
  
  else if (command == "test") {
    
    test(num_argc, cstrings);} 
  
  else if (command == "print-vectors") {
    
    printVectors(num_argc, cstrings);} 
  
  else if (command == "predict" || command == "predict-prob" ) {
    
    predict(num_argc, cstrings);}
  
  else if (command == "predict_skipgram_cbow") {
    
    SAVE_dict_vectors(cstrings, INPUT, OUTPUT);}
  
  else {
    
    printUsage();
    
    exit(EXIT_FAILURE);
  }
  
  utils::freeTables();

  // clean up memory
  
  for(size_t i = 0; i < num_argc; ++i) {
    
    delete[] cstrings[i];
  }
  
  delete[] cstrings;
}

