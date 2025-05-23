#include "ir/ir_generator.h"
#include "ast/expr.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <variant>

#ifdef __APPLE__
#define STDERR_SYMBOL "__stderrp"
#else
#define STDERR_SYMBOL "stderr"
#endif

llvm::StructType *getLLVMType(llvm::LLVMContext &ctx) {
  return llvm::StructType::get(
      ctx, {llvm::Type::getInt8Ty(ctx),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0)});
}

IRGenerator::IRGenerator()
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("llox", *context_)),
      builder_(std::make_unique<llvm::IRBuilder<>>(*context_)) {
  auto target_triple = llvm::sys::getDefaultTargetTriple();
  // std::cout << "target Triple: " << target_triple << std::endl;
  module_->setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

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

  const auto &expr_result = ast_root.accept(*this);
  if (std::holds_alternative<std::monostate>(expr_result)) {
    has_error_ = true;
    return;
  }
  llvm::Value *expr_value = std::get<llvm::Value *>(expr_result);
  create_print_call(expr_value);
  builder_->CreateRet(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0));

  std::string function_errors;
  llvm::raw_string_ostream f_rso(function_errors);
  if (llvm::verifyFunction(*current_fn_, &f_rso)) {
    std::cerr << "invalid function detected:" << f_rso.str() << std::endl;
    has_error_ = true;
    return;
  }

  std::string module_errors;
  llvm::raw_string_ostream rso(module_errors);
  if (llvm::verifyModule(*module_, &rso)) {
    std::cerr << "invalid module detected:" << rso.str() << std::endl;
    has_error_ = true;
    return;
  }
}

void IRGenerator::dump() const { module_->print(llvm::outs(), nullptr); }
llvm::Module &IRGenerator::get_module() const { return *module_; }

ExprResult IRGenerator::visit_binary_expr(const BinaryExpr &expr) {
  const auto &left_ret = expr.get_left().accept(*this);
  const auto &right_ret = expr.get_right().accept(*this);

  if (std::holds_alternative<std::monostate>(left_ret) ||
      std::holds_alternative<std::monostate>(right_ret)) {
    return std::monostate();
  }

  llvm::Value *L = std::get<llvm::Value *>(left_ret);
  llvm::Value *R = std::get<llvm::Value *>(right_ret);
  llvm::StructType *lox_type = getLLVMType(*context_);

  std ::cout << "BinaryExpr: " << std::endl;

  // Fast path
  if (auto *left_global = llvm::dyn_cast<llvm::GlobalVariable>(L)) {
    if (auto *right_global = llvm::dyn_cast<llvm::GlobalVariable>(R)) {
      if (left_global->getName().starts_with("const.loxval") &&
          right_global->getName().starts_with("const.loxval")) {

        auto *left_struct =
            llvm::cast<llvm::ConstantStruct>(left_global->getInitializer());
        auto *right_struct =
            llvm::cast<llvm::ConstantStruct>(right_global->getInitializer());

        auto *left_type =
            llvm::cast<llvm::ConstantInt>(left_struct->getOperand(0));
        auto *right_type =
            llvm::cast<llvm::ConstantInt>(right_struct->getOperand(0));

        value_type l_type = static_cast<value_type>(left_type->getZExtValue());
        value_type r_type = static_cast<value_type>(right_type->getZExtValue());

        if (l_type == r_type) {
          if (l_type == value_type::NUMBER) {
            auto *left_val =
                llvm::cast<llvm::GlobalVariable>(left_struct->getOperand(1));
            auto *right_val =
                llvm::cast<llvm::GlobalVariable>(right_struct->getOperand(1));

            double l_num =
                llvm::cast<llvm::ConstantFP>(left_val->getInitializer())
                    ->getValueAPF()
                    .convertToDouble();
            double r_num =
                llvm::cast<llvm::ConstantFP>(right_val->getInitializer())
                    ->getValueAPF()
                    .convertToDouble();

            std::variant<std::monostate, double, bool> result;
            switch (expr.get_op().type) {
            case TokenType::TOKEN_PLUS:
              result = l_num + r_num;
              break;
            case TokenType::TOKEN_MINUS:
              result = l_num - r_num;
              break;
            case TokenType::TOKEN_STAR:
              result = l_num * r_num;
              break;
            case TokenType::TOKEN_SLASH:
              result = l_num / r_num;
              break;
            case TokenType::TOKEN_GREATER:
              result = l_num > r_num;
              break;
            case TokenType::TOKEN_GREATER_EQUAL:
              result = l_num >= r_num;
              break;
            case TokenType::TOKEN_LESS:
              result = l_num < r_num;
              break;
            case TokenType::TOKEN_LESS_EQUAL:
              result = l_num <= r_num;
              break;
            case TokenType::TOKEN_EQUAL_EQUAL:
              result = l_num == r_num;
              break;
            case TokenType::TOKEN_BANG_EQUAL:
              result = l_num != r_num;
              break;
            default:
              result = std::monostate();
              break;
            }
            if (std::holds_alternative<double>(result)) {
              llvm::Constant *num_const = llvm::ConstantFP::get(
                  llvm::Type::getDoubleTy(*context_), std::get<double>(result));
              return create_literal_global(num_const, value_type::NUMBER,
                                           "num");
            } else {
              llvm::Constant *bool_const = llvm::ConstantInt::get(
                  llvm::Type::getInt1Ty(*context_), std::get<bool>(result));
              return create_literal_global(bool_const, value_type::BOOLEAN,
                                           "bool");
            }
          } else if (l_type == value_type::STRING) {
            auto *left_val =
                llvm::cast<llvm::GlobalVariable>(left_struct->getOperand(1));
            auto *right_val =
                llvm::cast<llvm::GlobalVariable>(right_struct->getOperand(1));
            llvm::StringRef left_str =
                llvm::cast<llvm::ConstantDataArray>(left_val->getInitializer())
                    ->getAsString();
            llvm::StringRef right_str =
                llvm::cast<llvm::ConstantDataArray>(right_val->getInitializer())
                    ->getAsString();

            std::string left_s(left_str.str().data(), left_str.size() - 1);
            std::string right_s(right_str.str().data(), right_str.size() - 1);
            std::variant<std::monostate, std::string, bool> result;

            switch (expr.get_op().type) {
            case TokenType::TOKEN_PLUS: {
              result = left_s + right_s;
              break;
            }
            case TokenType::TOKEN_GREATER:
              result = left_s > right_s;
              break;
            case TokenType::TOKEN_GREATER_EQUAL:
              result = left_s >= right_s;
              break;
            case TokenType::TOKEN_LESS:
              result = left_s < right_s;
              break;
            case TokenType::TOKEN_LESS_EQUAL:
              result = left_s <= right_s;
              break;
            case TokenType::TOKEN_EQUAL_EQUAL:
              result = left_s == right_s;
              break;
            case TokenType::TOKEN_BANG_EQUAL:
              result = left_s != right_s;
              break;
            default:
              result = std::monostate();
              break;
            }
            if (std::holds_alternative<std::string>(result)) {
              llvm::Constant *str_const = llvm::ConstantDataArray::getString(
                  *context_, std::get<std::string>(result), true);
              return create_literal_global(str_const, value_type::STRING,
                                           "str");
            } else if (std::holds_alternative<bool>(result)) {
              llvm::Constant *bool_const = llvm::ConstantInt::get(
                  llvm::Type::getInt1Ty(*context_), std::get<bool>(result));
              return create_literal_global(bool_const, value_type::BOOLEAN,
                                           "bool");
            }
          } else if (l_type == value_type::BOOLEAN) {
            auto *left_val =
                llvm::cast<llvm::GlobalVariable>(left_struct->getOperand(1));
            auto *right_val =
                llvm::cast<llvm::GlobalVariable>(right_struct->getOperand(1));
            bool left_bool =
                llvm::cast<llvm::ConstantInt>(left_val->getInitializer())
                    ->getValue()
                    .getBoolValue();
            bool right_bool =
                llvm::cast<llvm::ConstantInt>(right_val->getInitializer())
                    ->getValue()
                    .getBoolValue();

            std::variant<std::monostate, bool> result;
            switch (expr.get_op().type) {
            case TokenType::TOKEN_EQUAL_EQUAL:
              result = left_bool == right_bool;
              break;
            case TokenType::TOKEN_BANG_EQUAL:
              result = left_bool != right_bool;
              break;
            default:
              result = std::monostate();
              break;
            }
            if (std::holds_alternative<bool>(result)) {
              llvm::Constant *bool_const = llvm::ConstantInt::get(
                  llvm::Type::getInt1Ty(*context_), std::get<bool>(result));
              return create_literal_global(bool_const, value_type::BOOLEAN,
                                           "bool");
            }
          } else if (l_type == value_type::NIL) {
            std::variant<std::monostate, bool> result;
            switch (expr.get_op().type) {
            case TokenType::TOKEN_EQUAL_EQUAL:
              result = true;
              break;
            case TokenType::TOKEN_BANG_EQUAL:
              result = false;
              break;
            default:
              result = std::monostate();
              break;
            }
            if (std::holds_alternative<bool>(result)) {
              llvm::Constant *bool_const = llvm::ConstantInt::get(
                  llvm::Type::getInt1Ty(*context_), std::get<bool>(result));
              return create_literal_global(bool_const, value_type::BOOLEAN,
                                           "bool");
            }
          }
        }
      }
    }
  }

  // Slow path: 生成运行时检查
  auto *current_block = builder_->GetInsertBlock();
  auto *func = current_block->getParent();

  auto *check_block = llvm::BasicBlock::Create(*context_, "bin_check", func);
  auto *error_block = llvm::BasicBlock::Create(*context_, "bin_error", func);
  auto *compute_block =
      llvm::BasicBlock::Create(*context_, "bin_compute", func);
  llvm::BasicBlock *real_merge =
      llvm::BasicBlock::Create(*context_, "bin_merge", func);

  builder_->CreateBr(check_block);

  // type check
  builder_->SetInsertPoint(check_block);
  llvm::Value *left_type_ptr = builder_->CreateStructGEP(lox_type, L, 0);
  llvm::Value *left_type =
      builder_->CreateLoad(builder_->getInt8Ty(), left_type_ptr);
  llvm::Value *right_type_ptr = builder_->CreateStructGEP(lox_type, R, 0);
  llvm::Value *right_type =
      builder_->CreateLoad(builder_->getInt8Ty(), right_type_ptr);

  // 在类型检查失败的分支设置错误信息
  llvm::Value *type_ok =
      builder_->CreateICmpEQ(left_type, right_type, "type_cmp");
  auto *type_error_block =
      llvm::BasicBlock::Create(*context_, "type_error", func);
  builder_->CreateCondBr(type_ok, compute_block, type_error_block);
  builder_->SetInsertPoint(type_error_block);
  std::string type_err_msg =
      "Type mismatch between " +
      value_type_to_string(static_cast<value_type>(left_type->getZExtValue())) +
      " and " +
      value_type_to_string(static_cast<value_type>(right_type->getZExtValue()));
  prepare_error_info(type_err_msg, expr.get_op().line);
  builder_->CreateBr(error_block);

  builder_->SetInsertPoint(compute_block);
  llvm::Value *left_val_ptr = builder_->CreateStructGEP(lox_type, L, 1);
  left_val_ptr = builder_->CreateLoad(getInt8PtrTy(), left_val_ptr);
  llvm::Value *right_val_ptr = builder_->CreateStructGEP(lox_type, R, 1);
  right_val_ptr = builder_->CreateLoad(getInt8PtrTy(), right_val_ptr);

  auto *num_tag = builder_->getInt8(static_cast<uint8_t>(value_type::NUMBER));
  auto *str_tag = builder_->getInt8(static_cast<uint8_t>(value_type::STRING));
  auto *bool_tag = builder_->getInt8(static_cast<uint8_t>(value_type::BOOLEAN));
  auto *nil_tag = builder_->getInt8(static_cast<uint8_t>(value_type::NIL));

  auto *num_block = llvm::BasicBlock::Create(*context_, "num_bin", func);
  auto *str_block = llvm::BasicBlock::Create(*context_, "str_bin", func);
  auto *bool_block = llvm::BasicBlock::Create(*context_, "bool_bin", func);
  auto *nil_block = llvm::BasicBlock::Create(*context_, "nil_bin", func);
  auto *end_block = llvm::BasicBlock::Create(*context_, "cmp_end", func);

  // 创建类型不支持的错误处理块
  auto *unsupported_error_block =
      llvm::BasicBlock::Create(*context_, "unsupported_error", func);
  builder_->SetInsertPoint(unsupported_error_block);
  std::string unsupported_err_msg =
      "Unsupported type: " +
      value_type_to_string(static_cast<value_type>(left_type->getZExtValue()));
  prepare_error_info(unsupported_err_msg, expr.get_op().line);
  builder_->CreateBr(error_block);

  auto *type_switch =
      builder_->CreateSwitch(left_type, unsupported_error_block, 4);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(num_tag), num_block);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(str_tag), str_block);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(bool_tag), bool_block);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(nil_tag), nil_block);

  llvm::Value *num_result = nullptr;
  bool is_num_valid = false;
  llvm::Value *str_result = nullptr;
  bool is_str_valid = false;
  llvm::Value *bool_result = nullptr;
  bool is_bool_valid = false;
  llvm::Value *nil_result = nullptr;
  bool is_nil_valid = false;

  builder_->SetInsertPoint(num_block);
  {
    llvm::Type *double_ptr_ty =
        llvm::PointerType::get(builder_->getDoubleTy(), 0);
    llvm::Value *left_dbl = builder_->CreateLoad(
        builder_->getDoubleTy(),
        builder_->CreateBitCast(left_val_ptr, double_ptr_ty));
    llvm::Value *right_dbl = builder_->CreateLoad(
        builder_->getDoubleTy(),
        builder_->CreateBitCast(right_val_ptr, double_ptr_ty));

    llvm::Value *cmp_result = nullptr;
    switch (expr.get_op().type) {
    case TokenType::TOKEN_PLUS:
      num_result = builder_->CreateFAdd(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_MINUS:
      num_result = builder_->CreateFSub(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_STAR:
      num_result = builder_->CreateFMul(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_SLASH:
      num_result = builder_->CreateFDiv(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_GREATER:
      num_result = builder_->CreateFCmpUGT(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_GREATER_EQUAL:
      num_result = builder_->CreateFCmpUGE(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_LESS:
      num_result = builder_->CreateFCmpULT(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_LESS_EQUAL:
      num_result = builder_->CreateFCmpULE(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_EQUAL_EQUAL:
      cmp_result = builder_->CreateFCmpUEQ(left_dbl, right_dbl);
      break;
    case TokenType::TOKEN_BANG_EQUAL:
      cmp_result = builder_->CreateFCmpUNE(left_dbl, right_dbl);
      break;
    default:
      break;
    }
    if (num_result != nullptr) {
      llvm::AllocaInst *num_mem = builder_->CreateAlloca(
          builder_->getDoubleTy(), nullptr, "num_result");
      builder_->CreateStore(num_result, num_mem);
      num_result = builder_->CreateBitCast(num_mem, getInt8PtrTy());
      is_num_valid = true;
    } else if (cmp_result != nullptr) {
      llvm::AllocaInst *bool_mem = builder_->CreateAlloca(
          builder_->getInt1Ty(), nullptr, "num_comp_result");
      builder_->CreateStore(cmp_result, bool_mem);
      num_result = builder_->CreateBitCast(bool_mem, getInt8PtrTy());
      is_num_valid = true;
    } else {
      num_result = llvm::ConstantPointerNull::get(getInt8PtrTy());
    }
    builder_->CreateBr(end_block);
  }

  builder_->SetInsertPoint(bool_block);
  {
    llvm::Type *bool_ptr_ty = llvm::PointerType::get(builder_->getInt1Ty(), 0);
    llvm::Value *left_bool_ptr =
        builder_->CreateBitCast(left_val_ptr, bool_ptr_ty);
    llvm::Value *right_bool_ptr =
        builder_->CreateBitCast(right_val_ptr, bool_ptr_ty);
    llvm::Value *left_bool =
        builder_->CreateLoad(builder_->getInt1Ty(), left_bool_ptr);
    llvm::Value *right_bool =
        builder_->CreateLoad(builder_->getInt1Ty(), right_bool_ptr);

    switch (expr.get_op().type) {
    case TokenType::TOKEN_EQUAL_EQUAL:
      bool_result = builder_->CreateICmpEQ(left_bool, right_bool);
      break;
    case TokenType::TOKEN_BANG_EQUAL:
      bool_result = builder_->CreateICmpNE(left_bool, right_bool);
      break;
    default:
      break;
    }
    if (bool_result != nullptr) {
      llvm::AllocaInst *bool_mem =
          builder_->CreateAlloca(builder_->getInt1Ty(), nullptr, "bool_result");
      builder_->CreateStore(bool_result, bool_mem);
      bool_result = builder_->CreateBitCast(bool_mem, getInt8PtrTy());
      is_bool_valid = true;
    } else {
      bool_result = llvm::ConstantPointerNull::get(getInt8PtrTy());
    }
    builder_->CreateBr(end_block);
  }

  // nil比较
  builder_->SetInsertPoint(nil_block);
  {
    switch (expr.get_op().type) {
    case TokenType::TOKEN_EQUAL_EQUAL:
      nil_result = builder_->getInt1(true);
      break;
    case TokenType::TOKEN_BANG_EQUAL:
      nil_result = builder_->getInt1(false);
      break;
    default:
      break;
    }
    if (nil_result != nullptr) {
      llvm::AllocaInst *nil_mem =
          builder_->CreateAlloca(builder_->getInt1Ty(), nullptr, "nil_result");
      builder_->CreateStore(nil_result, nil_mem);
      nil_result = builder_->CreateBitCast(nil_mem, getInt8PtrTy());
      is_nil_valid = true;
    } else {
      nil_result = llvm::ConstantPointerNull::get(getInt8PtrTy());
    }
    builder_->CreateBr(end_block);
  }

  // str compare
  builder_->SetInsertPoint(str_block);
  {
    llvm::Value *buffer = nullptr;
    llvm::Function *strcmp_fn = llvm::cast<llvm::Function>(
        module_
            ->getOrInsertFunction("strcmp",
                                  llvm::FunctionType::get(
                                      builder_->getInt32Ty(),
                                      {getInt8PtrTy(), getInt8PtrTy()}, false))
            .getCallee());
    llvm::Value *str1 = builder_->CreateLoad(getInt8PtrTy(), left_val_ptr);
    llvm::Value *str2 = builder_->CreateLoad(getInt8PtrTy(), right_val_ptr);
    llvm::Value *comp_result = nullptr;
    switch (expr.get_op().type) {
    case TokenType::TOKEN_PLUS: {
      llvm::Function *strlen_fn = llvm::cast<llvm::Function>(
          module_
              ->getOrInsertFunction(
                  "strlen", llvm::FunctionType::get(builder_->getInt64Ty(),
                                                    {getInt8PtrTy()}, false))
              .getCallee());

      llvm::Value *len1 = builder_->CreateCall(strlen_fn, {str1});
      llvm::Value *len2 = builder_->CreateCall(strlen_fn, {str2});
      llvm::Value *total_len = builder_->CreateAdd(len1, len2);

      // 2. malloc
      llvm::Function *malloc_fn = llvm::cast<llvm::Function>(
          module_
              ->getOrInsertFunction(
                  "malloc",
                  llvm::FunctionType::get(getInt8PtrTy(),
                                          {builder_->getInt64Ty()}, false))
              .getCallee());

      buffer = builder_->CreateCall(
          malloc_fn,
          {builder_->CreateAdd(
              total_len, llvm::ConstantInt::get(builder_->getInt64Ty(), 1))});

      // 3. strcpy
      llvm::Function *strcpy_fn = llvm::cast<llvm::Function>(
          module_
              ->getOrInsertFunction(
                  "strcpy",
                  llvm::FunctionType::get(
                      getInt8PtrTy(), {getInt8PtrTy(), getInt8PtrTy()}, false))
              .getCallee());

      llvm::Function *strcat_fn = llvm::cast<llvm::Function>(
          module_
              ->getOrInsertFunction(
                  "strcat",
                  llvm::FunctionType::get(
                      getInt8PtrTy(), {getInt8PtrTy(), getInt8PtrTy()}, false))
              .getCallee());

      builder_->CreateCall(strcpy_fn, {buffer, str1});
      builder_->CreateCall(strcat_fn, {buffer, str2});

      str_result = buffer;
      break;
    }
    case TokenType::TOKEN_GREATER:
      comp_result = builder_->CreateICmpSGT(
          builder_->CreateCall(strcmp_fn, {str1, str2}), builder_->getInt32(0));
      break;
    case TokenType::TOKEN_GREATER_EQUAL:
      comp_result = builder_->CreateICmpSGE(
          builder_->CreateCall(strcmp_fn, {str1, str2}), builder_->getInt32(0));
      break;
    case TokenType::TOKEN_LESS:
      comp_result = builder_->CreateICmpSLT(
          builder_->CreateCall(strcmp_fn, {str1, str2}), builder_->getInt32(0));
      break;
    case TokenType::TOKEN_LESS_EQUAL:
      comp_result = builder_->CreateICmpSLE(
          builder_->CreateCall(strcmp_fn, {str1, str2}), builder_->getInt32(0));
      break;
    case TokenType::TOKEN_EQUAL_EQUAL:
      comp_result = builder_->CreateICmpEQ(
          builder_->CreateCall(strcmp_fn, {str1, str2}), builder_->getInt32(0));
      break;
    default:
      break;
    }
    if (str_result != nullptr) {
      llvm::AllocaInst *str_mem =
          builder_->CreateAlloca(getInt8PtrTy(), nullptr, "str_result");
      builder_->CreateStore(str_result, str_mem);
      str_result = builder_->CreateBitCast(str_mem, getInt8PtrTy());
      is_str_valid = true;
    } else if (comp_result != nullptr) {
      llvm::AllocaInst *comp_mem =
          builder_->CreateAlloca(builder_->getInt1Ty(), nullptr, "nil_result");
      builder_->CreateStore(comp_result, comp_mem);
      str_result = builder_->CreateBitCast(comp_mem, getInt8PtrTy());
      is_str_valid = true;
    } else {
      str_result = llvm::ConstantPointerNull::get(getInt8PtrTy());
    }
    builder_->CreateBr(end_block);
  }

  auto *invalid_result_block =
      llvm::BasicBlock::Create(*context_, "invalid_result", func);
  builder_->SetInsertPoint(invalid_result_block);
  std::string invalid_err_msg = "Invalid operation result";
  prepare_error_info(invalid_err_msg, expr.get_op().line);
  builder_->CreateBr(error_block);

  builder_->SetInsertPoint(end_block);
  llvm::PHINode *has_valid_phi = builder_->CreatePHI(builder_->getInt1Ty(), 4);
  struct BlockInfo {
    llvm::BasicBlock *bb;
    llvm::Value *type_tag;
    llvm::Value *value;
    bool is_valid;
  };
  std::vector<BlockInfo> block_infos = {
      {num_block, builder_->getInt8(static_cast<uint8_t>(value_type::NUMBER)),
       num_result, is_num_valid},
      {str_block, builder_->getInt8(static_cast<uint8_t>(value_type::STRING)),
       str_result, is_str_valid},
      {bool_block, builder_->getInt8(static_cast<uint8_t>(value_type::BOOLEAN)),
       bool_result, is_bool_valid},
      {nil_block, builder_->getInt8(static_cast<uint8_t>(value_type::NIL)),
       nil_result, is_nil_valid}};

  llvm::PHINode *result_type_phi =
      builder_->CreatePHI(builder_->getInt8Ty(), 4);
  llvm::PHINode *result_value_phi = builder_->CreatePHI(getInt8PtrTy(), 4);

  for (const auto &info : block_infos) {
    result_type_phi->addIncoming(info.type_tag, info.bb);
    result_value_phi->addIncoming(info.value, info.bb);
    has_valid_phi->addIncoming(
        llvm::ConstantInt::get(builder_->getInt1Ty(), info.is_valid), info.bb);
  }
  std::cout << is_bool_valid << std::endl;
  std::cout << is_num_valid << std::endl;
  std::cout << is_str_valid << std::endl;
  std::cout << is_nil_valid << std::endl;

  llvm::Value *wrapped_result =
      builder_->CreateAlloca(lox_type, nullptr, "final_result");
  llvm::Value *type_ptr =
      builder_->CreateStructGEP(lox_type, wrapped_result, 0);
  builder_->CreateStore(result_type_phi, type_ptr);
  llvm::Value *value_ptr =
      builder_->CreateStructGEP(lox_type, wrapped_result, 1);
  builder_->CreateStore(result_value_phi, value_ptr);
  builder_->CreateCondBr(has_valid_phi, real_merge, invalid_result_block);

  builder_->SetInsertPoint(real_merge);
  llvm::PHINode *phi = builder_->CreatePHI(
      llvm::PointerType::get(getLLVMType(*context_), 0), 1, "binary_result");
  phi->addIncoming(wrapped_result, end_block);

  return phi;
}

ExprResult IRGenerator::visit_grouping_expr(const GroupingExpr &expr) {
  std::cout << "visit_grouping_expr" << std::endl;
  return expr.get_expr().accept(*this);
}

llvm::GlobalVariable *
IRGenerator::create_literal_global(llvm::Constant *value, value_type type,
                                   const std::string &prefix) {
  std::cout << "create_literal_global" << std::endl;

  llvm::GlobalVariable *value_global = new llvm::GlobalVariable(
      *module_, value->getType(), true, llvm::GlobalValue::InternalLinkage,
      value, "const." + prefix);

  llvm::Value *value_ptr =
      builder_->CreateBitCast(value_global, getInt8PtrTy());

  // create global struct
  llvm::StructType *lox_type = getLLVMType(*context_);
  return new llvm::GlobalVariable(
      *module_, lox_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantStruct::get(
          lox_type,
          llvm::ArrayRef<llvm::Constant *>(
              {llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                                      static_cast<uint8_t>(type)),
               llvm::cast<llvm::Constant>(value_ptr)})),
      "const.loxval");
}

ExprResult IRGenerator::visit_literal_expr(const LiteralExpr &expr) {
  std::cout << "visit_literal_expr" << std::endl;
  auto &lox_val = expr.get_value();
  if (std::holds_alternative<std::monostate>(lox_val)) {
    return std::monostate();
  }
  const auto &val = std::get<LoxValue>(lox_val);
  switch (val.type_tag) {
  case value_type::NUMBER: {
    llvm::Constant *num_const =
        llvm::ConstantFP::get(*context_, llvm::APFloat(val.number));
    return create_literal_global(num_const, value_type::NUMBER, "num");
  }
  case value_type::BOOLEAN: {
    llvm::Constant *bool_const =
        llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), val.boolean);
    return create_literal_global(bool_const, value_type::BOOLEAN, "bool");
  }
  case value_type::STRING: {
    const std::string &str = *val.str_data;
    llvm::Constant *str_const =
        llvm::ConstantDataArray::getString(*context_, str, true);
    return create_literal_global(str_const, value_type::STRING, "str");
  }
  case value_type::NIL: {
    llvm::Constant *nil_const = llvm::ConstantPointerNull::get(getInt8PtrTy());
    return create_literal_global(nil_const, value_type::NIL, "nil");
  }
  }
  std::cerr << "unknown type for literal" << std::endl;
  return std::monostate();
}

ExprResult IRGenerator::visit_unary_expr(const UnaryExpr &expr) {
  std::cout << "visit_unary_expr" << std::endl;
  const auto &right_ret = expr.get_right().accept(*this);
  if (std::holds_alternative<std::monostate>(right_ret)) {
    return std::monostate();
  }

  llvm::Value *operand = std::get<llvm::Value *>(right_ret);
  llvm::StructType *lox_type = getLLVMType(*context_);

  // Fast path:
  if (auto *global_var = llvm::dyn_cast<llvm::GlobalVariable>(operand)) {
    if (global_var->getName().starts_with("const.loxval")) {
      llvm::Constant *init_val = global_var->getInitializer();
      auto *const_struct = llvm::cast<llvm::ConstantStruct>(init_val);
      auto *type_tag =
          llvm::cast<llvm::ConstantInt>(const_struct->getOperand(0));
      auto *value_ptr = llvm::cast<llvm::Constant>(const_struct->getOperand(1));

      value_type val_type = static_cast<value_type>(type_tag->getZExtValue());

      switch (expr.get_op().type) {
      case TokenType::TOKEN_MINUS: {
        if (val_type != value_type::NUMBER)
          break;
        auto *num_global = llvm::cast<llvm::GlobalVariable>(value_ptr);
        double num_val =
            llvm::cast<llvm::ConstantFP>(num_global->getInitializer())
                ->getValueAPF()
                .convertToDouble();

        llvm::Constant *neg_const =
            llvm::ConstantFP::get(*context_, llvm::APFloat(-num_val));
        return create_literal_global(neg_const, value_type::NUMBER, "num");
      }
      case TokenType::TOKEN_BANG: {
        if (val_type == value_type::BOOLEAN) {
          auto *bool_global = llvm::cast<llvm::GlobalVariable>(value_ptr);
          bool bool_val =
              llvm::cast<llvm::ConstantInt>(bool_global->getInitializer())
                  ->getZExtValue();

          llvm::Constant *not_const = llvm::ConstantInt::get(
              llvm::Type::getInt1Ty(*context_), !bool_val);
          return create_literal_global(not_const, value_type::BOOLEAN, "bool");
        } else if (val_type == value_type::NIL) {
          return create_literal_global(
              llvm::ConstantInt::getTrue(llvm::Type::getInt1Ty(*context_)),
              value_type::BOOLEAN, "bool");
        } else {
          return create_literal_global(
              llvm::ConstantInt::getFalse(llvm::Type::getInt1Ty(*context_)),
              value_type::BOOLEAN, "bool");
        }
        break;
      }
      default:
        break;
      }
    }
  }

  // Slow path: generate IR
  auto *current_block = builder_->GetInsertBlock();
  auto *func = current_block->getParent();
  auto *check_block = llvm::BasicBlock::Create(*context_, "unary_check", func);
  auto *error_block = llvm::BasicBlock::Create(*context_, "unary_error", func);
  auto *compute_block =
      llvm::BasicBlock::Create(*context_, "unary_compute", func);
  auto *merge_block = llvm::BasicBlock::Create(*context_, "unary_merge", func);
  builder_->CreateBr(check_block);

  // 1. check type
  builder_->SetInsertPoint(check_block);
  llvm::Value *type_ptr = builder_->CreateStructGEP(lox_type, operand, 0);
  llvm::Value *type_tag =
      builder_->CreateLoad(llvm::Type::getInt8Ty(*context_), type_ptr);

  llvm::ConstantInt *expected_type = nullptr;
  if (expr.get_op().type == TokenType::TOKEN_MINUS) {
    expected_type =
        llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                               static_cast<uint8_t>(value_type::NUMBER));
  } else { // TOKEN_BANG
    expected_type =
        llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                               static_cast<uint8_t>(value_type::BOOLEAN));
  }

  llvm::Value *type_ok =
      builder_->CreateICmpEQ(type_tag, expected_type, "type_check");
  builder_->CreateCondBr(type_ok, compute_block, error_block);

  create_error_block(error_block, "operand type is not expected",
                     expr.get_op().line);

  // 3. compute block
  builder_->SetInsertPoint(compute_block);
  llvm::Value *val_ptr = builder_->CreateStructGEP(lox_type, operand, 1);
  llvm::Value *raw_val = builder_->CreateLoad(getInt8PtrTy(), val_ptr);

  llvm::Value *result = nullptr;
  value_type result_type = value_type::NIL;

  switch (expr.get_op().type) {
  case TokenType::TOKEN_MINUS: {
    llvm::Type *double_ptr_ty =
        llvm::PointerType::get(llvm::Type::getDoubleTy(*context_), 0);
    llvm::Value *double_ptr = builder_->CreateBitCast(raw_val, double_ptr_ty);
    llvm::Value *num =
        builder_->CreateLoad(llvm::Type::getDoubleTy(*context_), double_ptr);
    result = builder_->CreateFNeg(num, "neg_tmp");
    result_type = value_type::NUMBER;
    break;
  }
  case TokenType::TOKEN_BANG: {
    llvm::Type *bool_ptr_ty =
        llvm::PointerType::get(llvm::Type::getInt1Ty(*context_), 0);
    llvm::Value *bool_ptr = builder_->CreateBitCast(raw_val, bool_ptr_ty);
    llvm::Value *bval =
        builder_->CreateLoad(llvm::Type::getInt1Ty(*context_), bool_ptr);
    result = builder_->CreateNot(bval, "not_tmp");
    result_type = value_type::BOOLEAN;
    break;
  }
  default:
    builder_->CreateUnreachable();
  }

  llvm::Value *wrapped_result = wrap_llvm_lox_value(result, result_type);
  builder_->CreateBr(merge_block);

  // 4. merge block
  builder_->SetInsertPoint(merge_block);
  llvm::PHINode *phi = builder_->CreatePHI(
      llvm::PointerType::get(getLLVMType(*context_), 0), 1, "unary_result");
  phi->addIncoming(wrapped_result, compute_block);

  return phi;
}

void IRGenerator::create_print_call(llvm::Value *value) {
  std::cout << "create_print_call" << std::endl;
  llvm::StructType *lox_type = getLLVMType(*context_);
  auto *current_block = builder_->GetInsertBlock();
  auto *func = current_block->getParent();

  if (auto *global_var = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
    if (global_var->getName().starts_with("const.loxval")) {
      llvm::Constant *init_val = global_var->getInitializer();
      if (auto *const_struct = llvm::dyn_cast<llvm::ConstantStruct>(init_val)) {
        auto *type_tag =
            llvm::cast<llvm::ConstantInt>(const_struct->getOperand(0));
        auto *value_ptr =
            llvm::cast<llvm::Constant>(const_struct->getOperand(1));

        auto *merge_block =
            llvm::BasicBlock::Create(*context_, "print_exit", func);
        auto *err_block =
            llvm::BasicBlock::Create(*context_, "print_err", func);

        switch (static_cast<value_type>(type_tag->getZExtValue())) {
        case value_type::NIL: {
          llvm::Value *fmt = builder_->CreateGlobalString("nil\n");
          builder_->CreateCall(get_printf_function(), {fmt});
          break;
        }
        case value_type::BOOLEAN: {
          auto *bool_global = llvm::cast<llvm::GlobalVariable>(value_ptr);
          bool bool_val =
              llvm::cast<llvm::ConstantInt>(bool_global->getInitializer())
                  ->getZExtValue();

          llvm::Value *fmt = builder_->CreateGlobalString("%s\n");
          llvm::Value *str =
              builder_->CreateGlobalString(bool_val ? "true" : "false");
          builder_->CreateCall(get_printf_function(), {fmt, str});
          break;
        }
        case value_type::NUMBER: {
          auto *num_global = llvm::cast<llvm::GlobalVariable>(value_ptr);
          double num_val =
              llvm::cast<llvm::ConstantFP>(num_global->getInitializer())
                  ->getValueAPF()
                  .convertToDouble();
          llvm::Value *fmt = builder_->CreateGlobalString("%g\n");
          llvm::Value *num =
              llvm::ConstantFP::get(*context_, llvm::APFloat(num_val));
          builder_->CreateCall(get_printf_function(), {fmt, num});
          break;
        }
        case value_type::STRING: {
          llvm::Value *str = builder_->CreateBitCast(value_ptr, getInt8PtrTy());
          llvm::Value *fmt = builder_->CreateGlobalString("%s\n");
          builder_->CreateCall(get_printf_function(), {fmt, str});
          break;
        }
        default:
          builder_->CreateBr(err_block);
        }
        builder_->CreateBr(merge_block);
        builder_->SetInsertPoint(err_block);
        {
          llvm::Value *err_fmt =
              builder_->CreateGlobalString("Unknown type to print\n");
          builder_->CreateCall(get_printf_function(), {err_fmt});
          builder_->CreateBr(merge_block);
        }
        builder_->SetInsertPoint(merge_block);
        return;
      }
    }
  }

  // Slow path: generate IR
  llvm::Value *type_tag_ptr = builder_->CreateStructGEP(lox_type, value, 0);
  llvm::Value *type_tag =
      builder_->CreateLoad(llvm::Type::getInt8Ty(*context_), type_tag_ptr);

  llvm::Value *val_ptr = builder_->CreateStructGEP(lox_type, value, 1);
  llvm::Value *raw_value = builder_->CreateLoad(getInt8PtrTy(), val_ptr);

  auto *merge_block = llvm::BasicBlock::Create(*context_, "print_exit", func);

  auto *nil_block = llvm::BasicBlock::Create(*context_, "print_nil", func);
  auto *bool_block = llvm::BasicBlock::Create(*context_, "print_bool", func);
  auto *num_block = llvm::BasicBlock::Create(*context_, "print_num", func);
  auto *str_block = llvm::BasicBlock::Create(*context_, "print_str", func);
  auto *err_block = llvm::BasicBlock::Create(*context_, "print_err", func);

  auto *nil_tag = llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                                         static_cast<uint8_t>(value_type::NIL));
  auto *bool_tag =
      llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                             static_cast<uint8_t>(value_type::BOOLEAN));
  auto *num_tag =
      llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                             static_cast<uint8_t>(value_type::NUMBER));
  auto *str_tag =
      llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_),
                             static_cast<uint8_t>(value_type::STRING));

  auto *type_switch = builder_->CreateSwitch(type_tag, err_block, 4);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(nil_tag), nil_block);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(bool_tag), bool_block);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(num_tag), num_block);
  type_switch->addCase(llvm::cast<llvm::ConstantInt>(str_tag), str_block);

  builder_->SetInsertPoint(nil_block);
  {
    llvm::Value *fmt = builder_->CreateGlobalString("nil\n");
    builder_->CreateCall(get_printf_function(), {fmt});
    builder_->CreateBr(merge_block);
  }

  builder_->SetInsertPoint(bool_block);
  {
    llvm::Type *bool_ptr_ty =
        llvm::PointerType::get(llvm::Type::getInt1Ty(*context_), 0);
    llvm::Value *bool_ptr = builder_->CreateBitCast(raw_value, bool_ptr_ty);
    llvm::Value *bval =
        builder_->CreateLoad(llvm::Type::getInt1Ty(*context_), bool_ptr);

    llvm::Value *fmt = builder_->CreateGlobalString("%s\n");
    llvm::Value *str =
        builder_->CreateSelect(bval, builder_->CreateGlobalString("true"),
                               builder_->CreateGlobalString("false"));
    builder_->CreateCall(get_printf_function(), {fmt, str});
    builder_->CreateBr(merge_block);
  }

  builder_->SetInsertPoint(num_block);
  {
    llvm::Type *double_ptr_ty =
        llvm::PointerType::get(llvm::Type::getDoubleTy(*context_), 0);
    llvm::Value *double_ptr = builder_->CreateBitCast(raw_value, double_ptr_ty);
    llvm::Value *num =
        builder_->CreateLoad(llvm::Type::getDoubleTy(*context_), double_ptr);

    llvm::Value *fmt = builder_->CreateGlobalString("%g\n");
    builder_->CreateCall(get_printf_function(), {fmt, num});
    builder_->CreateBr(merge_block);
  }

  builder_->SetInsertPoint(str_block);
  {
    llvm::Value *str = builder_->CreateLoad(getInt8PtrTy(), raw_value);
    llvm::Value *fmt = builder_->CreateGlobalString("%s\n");
    builder_->CreateCall(get_printf_function(), {fmt, str});
    builder_->CreateBr(merge_block);
  }

  builder_->SetInsertPoint(err_block);
  {
    llvm::Value *err_fmt =
        builder_->CreateGlobalString("Unknown type to print\n");
    builder_->CreateCall(get_printf_function(), {err_fmt});
    builder_->CreateBr(merge_block);
  }

  builder_->SetInsertPoint(merge_block);
}

llvm::Function *IRGenerator::get_printf_function() {
  if (!module_->getFunction("printf")) {
    llvm::FunctionType *printf_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_), {getInt8PtrTy()}, true);
    return llvm::Function::Create(printf_type, llvm::Function::ExternalLinkage,
                                  "printf", module_.get());
  }
  return module_->getFunction("printf");
}

llvm::Value *IRGenerator::wrap_llvm_lox_value(llvm::Value *value,
                                              value_type type) {
  llvm::StructType *lox_type = getLLVMType(*context_);
  llvm::Value *lox_value =
      builder_->CreateAlloca(lox_type, nullptr, "lox_value");

  llvm::Value *type_tag = builder_->getInt8(static_cast<uint8_t>(type));
  llvm::Value *type_ptr = builder_->CreateStructGEP(lox_type, lox_value, 0);
  builder_->CreateStore(type_tag, type_ptr);

  llvm::Value *value_ptr = builder_->CreateStructGEP(lox_type, lox_value, 1);

  switch (type) {
  case value_type::NUMBER: {
    llvm::Value *num_mem = builder_->CreateAlloca(
        llvm::Type::getDoubleTy(*context_), nullptr, "num_mem");
    builder_->CreateStore(value, num_mem);
    builder_->CreateStore(builder_->CreateBitCast(num_mem, getInt8PtrTy()),
                          value_ptr);
    break;
  }
  case value_type::BOOLEAN: {
    llvm::Value *bool_mem = builder_->CreateAlloca(
        llvm::Type::getInt1Ty(*context_), nullptr, "bool_mem");
    builder_->CreateStore(value, bool_mem);
    builder_->CreateStore(builder_->CreateBitCast(bool_mem, getInt8PtrTy()),
                          value_ptr);
    break;
  }
  case value_type::STRING:
  case value_type::NIL: {
    builder_->CreateStore(builder_->CreatePointerCast(value, getInt8PtrTy()),
                          value_ptr);
    break;
  }
  default:
    std::cerr << "unknown value type:" << static_cast<int>(type) << std::endl;
    has_error_ = true;
  }

  return lox_value;
}

void IRGenerator::prepare_error_info(const std::string &msg, int line) {
  // 创建动态错误信息
  llvm::Value *err_global = builder_->CreateGlobalString(msg);
  // 存储到全局slot
  builder_->CreateStore(err_global, error_info_slot_);
}

void IRGenerator::init_error_block(llvm::BasicBlock *error_block) {
  builder_->SetInsertPoint(error_block);

  llvm::Value *err_msg_ptr =
      builder_->CreateLoad(builder_->getInt8PtrTy(), error_info_slot_);
  llvm::Value *fmt_str = builder_->CreateGlobalString("error: %s\n");

  llvm::FunctionType *fprintf_type = llvm::FunctionType::get(
      builder_->getInt32Ty(), {getInt8PtrTy(), getInt8PtrTy()}, true);
  llvm::FunctionCallee fprintf_fn =
      module_->getOrInsertFunction("fprintf", fprintf_type);

  llvm::PointerType *file_ptr_ty =
      llvm::PointerType::get(llvm::StructType::create(*context_, "FILE"), 0);
  llvm::GlobalVariable *stderr_var =
      llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
          STDERR_SYMBOL, llvm::PointerType::get(file_ptr_ty, 0)));
  llvm::Value *stderr_addr =
      builder_->CreateLoad(llvm::PointerType::get(file_ptr_ty, 0), stderr_var);
  llvm::Value *stderr_ptr =
      builder_->CreateBitCast(stderr_addr, getInt8PtrTy());

  builder_->CreateCall(fprintf_fn, {stderr_ptr, fmt_str, err_msg_ptr});

  llvm::FunctionType *exit_type = llvm::FunctionType::get(
      builder_->getVoidTy(), {builder_->getInt32Ty()}, false);
  llvm::FunctionCallee exit_fn =
      module_->getOrInsertFunction("exit", exit_type);
  builder_->CreateCall(exit_fn,
                       {llvm::ConstantInt::get(builder_->getInt32Ty(), 1)});

  builder_->CreateUnreachable();
}
