#include "parser/scanner.h"
#include "parser/parser.h"
#include "ir/ir_generator.h"
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/IR/LegacyPassManager.h> 

using namespace std;

void runFile(const string &path);
void runPrompt();
void run(const string &source);

int main(int argc, char *argv[]) {
  if (argc > 2) {
    cerr << "Usage: clox [script]" << endl;
    exit(64);
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    runPrompt();
  }
  return 0;
}

void runFile(const string &path) {
  ifstream file(path);
  if (!file.is_open()) {
    cerr << "failed to open file: " << path << endl;
    exit(74);
  }

  string source((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
  run(source);
}

void runPrompt() {
  string line;
  cout << "> " << flush;

  while (getline(cin, line)) {
    run(line);
    cout << "> " << flush;
  }
}

void run(const string &source) {
  Scanner scanner(source);
  std::vector<Token> tokens;
  scanner.scan_tokens(tokens);
  if (scanner.has_error())
    return;
  /*
  for (auto &token : tokens) {
    std::cout << token.toString() << endl;
  }
  */
  Parser parser(tokens);
  if (parser.has_error())
    return;
  
  auto expr = parser.parse();
  IRGenerator generator;
  generator.generate_ir(*expr);
  if (generator.has_error())
    return;
  //generator.dump();
  
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
  auto target_triple = llvm::sys::getDefaultTargetTriple();
  //std::cout << "target Triple: " << target_triple << std::endl;
  generator.get_module().setTargetTriple(target_triple);
  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
  if (!target) {
    std::cerr << error << std::endl;
    return;
  }
  auto cpu = "generic";
  auto features = "";
  llvm::TargetOptions opt;
  auto rm = std::optional<llvm::Reloc::Model>();
  auto target_machine = target->createTargetMachine(
      target_triple, cpu, features, opt, rm);
  generator.get_module().setDataLayout(target_machine->createDataLayout());
  std::string obj_filename = "output.o";
  std::string filename = "output";
  std::error_code ec;
  llvm::raw_fd_ostream dest(obj_filename, ec, llvm::sys::fs::OF_None);
  if (ec) {
    std::cerr << "failed to open file: " << ec.message() << std::endl;
    return;
  }
  llvm::legacy::PassManager pass;
  auto file_type = llvm::CodeGenFileType::ObjectFile;
  if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
    std::cerr << "failed to emit object file" << std::endl;
    return;
  }
  pass.run(generator.get_module());
  dest.flush();

  std::string cmd = "clang " + obj_filename + " -o " + filename;
  if (system(cmd.c_str()) != 0) {
    std::cerr << "failed to compile" << std::endl;
  }
  std::string run_cmd = "./" + filename;
  if (system(run_cmd.c_str()) != 0) {
    std::cerr << "failed to run" << std::endl;
  }
}
