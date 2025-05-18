#include "ir/ir_generator.h"
#include "ast/expr.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <variant>

IRGenerator::IRGenerator()
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("llox", *context_)),
      builder_(std::make_unique<llvm::IRBuilder<>>(*context_)) {}

void IRGenerator::create_main_function() {
  llvm::FunctionType *main_type =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*context_), false);
  llvm::Function *main_fn = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, "main", module_.get());

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(*context_, "entry", main_fn);
  builder_->SetInsertPoint(bb);

  current_fn_ = main_fn;
  current_bb_ = bb;
}

bool IRGenerator::has_error() const { return has_error_; }

void IRGenerator::generate_ir(const Expr &ast_root) {
  create_main_function();

  const auto &val = ast_root.accept(*this);
  if (std::holds_alternative<std::monostate>(val)) {
    has_error_ = true;
    return;
  }
  llvm::Value *expr_value = std::get<llvm::Value *>(ast_root.accept(*this));
  create_print_call(expr_value);

  builder_->CreateRet(llvm::ConstantInt::get(
    llvm::Type::getInt32Ty(*context_), 0));
  llvm::verifyFunction(*current_fn_);
}

void IRGenerator::dump() const { module_->print(llvm::outs(), nullptr); }
llvm::Module &IRGenerator::get_module() const { return *module_; }

ExprResult IRGenerator::visit_binary_expr(const BinaryExpr &expr) {
  const auto &left_val = expr.get_left().accept(*this);
  const auto &right_val = expr.get_right().accept(*this);

  if (std::holds_alternative<std::monostate>(left_val) ||
      std::holds_alternative<std::monostate>(right_val)) {
    return std::monostate();
  }

  llvm::Value *L = std::get<llvm::Value *>(expr.get_left().accept(*this));
  llvm::Value *R = std::get<llvm::Value *>(expr.get_right().accept(*this));
  auto check_and_convert = [&](const std::string &op_name) -> bool {
    if (!L->getType()->isDoubleTy() || !R->getType()->isDoubleTy()) {
      std::cerr << op_name << "the left or right is not a number at line: "
                << expr.get_op().line << std::endl;
      return false;
    }
    return true;
  };

  llvm::Value *cmp_result = nullptr;
  switch (expr.get_op().type) {
  case TokenType::TOKEN_PLUS:
    if (!check_and_convert("plus")) {
      return std::monostate();
    }
    return builder_->CreateFAdd(L, R, "addtmp");
  case TokenType::TOKEN_MINUS:
    if (!check_and_convert("minus")) {
      return std::monostate();
    }
    return builder_->CreateFSub(L, R, "subtmp");
  case TokenType::TOKEN_STAR:
    if (!check_and_convert("mult")) {
      return std::monostate();
    }
    return builder_->CreateFMul(L, R, "multmp");
  case TokenType::TOKEN_SLASH:
    if (!check_and_convert("div")) {
      return std::monostate();
    }
    return builder_->CreateFDiv(L, R, "divtmp");
  case TokenType::TOKEN_GREATER:
    if (!check_and_convert("greater")) {
      return std::monostate();
    }
    return builder_->CreateFCmpUGT(L, R, "greater");
  case TokenType::TOKEN_GREATER_EQUAL:
    if (!check_and_convert("greater_equal")) {
      return std::monostate();
    }
    return builder_->CreateFCmpUGE(L, R, "greater_equal");
  case TokenType::TOKEN_LESS:
    if (!check_and_convert("less")) {
      return std::monostate();
    }
    return builder_->CreateFCmpULT(L, R, "less");
  case TokenType::TOKEN_LESS_EQUAL:
    if (!check_and_convert("less_equal")) {
      return std::monostate();
    }
    return builder_->CreateFCmpULE(L, R, "less_equal");
  case TokenType::TOKEN_EQUAL_EQUAL:
    if (L->getType() != R->getType()) {
      std::cerr << "type mismatch at line:" << expr.get_op().line << std::endl;
      return std::monostate();
    }
    if (L->getType()->isDoubleTy()) {
      cmp_result = builder_->CreateFCmpUEQ(L, R, "fcmp");
    } else if (L->getType()->isIntegerTy(1)) {
      cmp_result = builder_->CreateICmpEQ(L, R, "icmp");
    } else {
      std::cerr << "unknown type at line:" << expr.get_op().line << std::endl;
      return std::monostate();
    }
    return cmp_result;
  case TokenType::TOKEN_BANG_EQUAL:
    if (L->getType() != R->getType()) {
      std::cerr << "type mismatch at line:" << expr.get_op().line << std::endl;
      return std::monostate();
    }
    if (L->getType()->isDoubleTy()) {
      cmp_result = builder_->CreateFCmpUNE(L, R, "fcmp");
    } else if (L->getType()->isIntegerTy(1)) {
      cmp_result = builder_->CreateICmpNE(L, R, "icmp");
    } else {
      std::cerr << "unknown type at line:" << expr.get_op().line << std::endl;
      return std::monostate();
    }
    return cmp_result;
  default:
    return std::monostate();
  }
}

ExprResult IRGenerator::visit_grouping_expr(const GroupingExpr &expr) {
  return expr.get_expr().accept(*this);
}

ExprResult IRGenerator::visit_literal_expr(const LiteralExpr &expr) {
  auto &val = expr.get_value();
  if (std::holds_alternative<double>(expr.get_value())) {
    return llvm::ConstantFP::get(
        *context_, llvm::APFloat(std::get<double>(expr.get_value())));
  }
  if (std::holds_alternative<bool>(val)) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_),
                                  std::get<bool>(val));
  }
  if (std::holds_alternative<llvm::Value*>(val)) {
    
  }
  if (std::holds_alternative<std::string>(val)) {
    const auto &str = std::get<std::string>(val);
    llvm::Constant *strConstant =
        llvm::ConstantDataArray::getString(*context_, str, true);
    llvm::GlobalVariable *gvar = new llvm::GlobalVariable(
        *module_, strConstant->getType(), true,
        llvm::GlobalValue::PrivateLinkage, strConstant, ".str");
    llvm::Type *int8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(*context_), 0);
    return builder_->CreateBitCast(gvar, int8PtrTy);
  }
  std::cerr << "unknown literal type" << std::endl;
  return std::monostate();
}

ExprResult IRGenerator::visit_unary_expr(const UnaryExpr &expr) {
  const auto &rightVal = expr.get_right().accept(*this);
  if (std::holds_alternative<std::monostate>(rightVal)) {
    return std::monostate();
  }

  auto operand = std::get<llvm::Value *>(rightVal);
  switch (expr.get_op().type) {
  case TokenType::TOKEN_MINUS:
    if (!operand->getType()->isDoubleTy()) {
      std::cerr << "type error: operand must be a number at line "
                << expr.get_op().line << std::endl;
      return std::monostate();
    }
    return builder_->CreateFNeg(operand, "negtmp");
  case TokenType::TOKEN_BANG:
    if (!operand->getType()->isIntegerTy(1)) {
      std::cerr << "type error: operand must be a boolean at line "
                << expr.get_op().line << std::endl;
      return std::monostate();
    }
    return builder_->CreateNot(operand, "nottmp");
  default:
    return nullptr;
  }
}

void IRGenerator::create_print_call(llvm::Value *value) {
  if (!module_->getFunction("printf")) {
    llvm::Type *int8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(*context_), 0);
    llvm::FunctionType *printf_type =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(*context_),
                                {int8PtrTy},
                                true // 可变参数
        );
    llvm::Function::Create(printf_type, llvm::Function::ExternalLinkage,
                           "printf", module_.get());
  }

  llvm::Value *format_str = nullptr;
  if (value->getType()->isDoubleTy()) {
    format_str = builder_->CreateGlobalString("%f\n");
    builder_->CreateCall(module_->getFunction("printf"), {format_str, value});
  } 
  else if (value->getType()->isIntegerTy(1)) { 
    format_str = builder_->CreateGlobalString("%d\n");
    llvm::Value *extended = builder_->CreateZExt(
        value, llvm::Type::getInt32Ty(*context_), "bool_ext");
    builder_->CreateCall(module_->getFunction("printf"), {format_str, extended});
  }
  else if (value->getType()->isPointerTy()) { 
    format_str = builder_->CreateGlobalString("%s\n");
    builder_->CreateCall(module_->getFunction("printf"), {format_str, value});
  }
  else {
    std::cerr << "unknown type" << std::endl;
    has_error_ = true;
  }
}