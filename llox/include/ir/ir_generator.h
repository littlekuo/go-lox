#ifndef IR_GENERATOR_H
#define IR_GENERATOR_H

#include "ast/expr.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

class IRGenerator : public ExprVisitor {
private:
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;

  llvm::Function *current_fn_ = nullptr;
  llvm::BasicBlock *current_bb_ = nullptr;
  bool has_error_{false};
  void create_print_call(llvm::Value* value);

public:
  IRGenerator();
  
  void create_main_function();
  void generate_ir(const Expr& ast_root);
  void dump() const;
  llvm::Module& get_module() const;
  ExprResult visit_binary_expr(const BinaryExpr &expr) override;
  ExprResult visit_grouping_expr(const GroupingExpr &expr) override;
  ExprResult visit_literal_expr(const LiteralExpr &expr) override;
  ExprResult visit_unary_expr(const UnaryExpr &expr) override;
  bool has_error() const;
};

#endif