#ifndef _NODE_H_
#define _NODE_H_

#include <memory>
#include <list>
#include <iostream>

#include "codegen.hpp"

/// possible binary expression operators:
// plus, minus, multiplication, division
typedef enum opEnum {
    PLUS, MINUS, MUL, DIV
} BinOp;
/// possible comparison operators:
// non-equal, less-than, less-equal, equal, greater-equal,
// greater-than
typedef enum compEnum {
    NE, LT, LE, EQ, GE, GT
} CompOp;

/// base class of all AST nodes;
// all nodes can be used to generate code via
// method CodeGenerator::codegen()
class Node: public CodeGenerator {
public:
    virtual ~Node() {}
    virtual void printName() const{
      if(nullptr==this){return;}
      std::cout<<typeid(*this).name()<<std::endl;
    }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};


// some syntactic sugar for creating AST nodes
// type of pointer to a node (or subclass)
#define PNODE(type) type*
// Rashed Al-Lahaseh - 00821573
// Create a new AST node (of type Node or subclass)
#define NEW_PNODE(type,...) (new type(__VA_ARGS__))
#define DEL_NODE(n) delete(n)
// type of pointer to Node (or subclass)
typedef PNODE(Node) PNode;

// some shorthands
class NStatement;
class NExpression;
class NVariableDeclaration;
typedef std::list<PNODE(const NStatement)> NStatementList;
typedef std::list<PNODE(const NExpression)> NExpressionList;
typedef std::list<PNODE(const NVariableDeclaration)> NVariableList;

// Here follow the nodes corresponding to different non-terminals

class NExpression: public Node {
public:
    virtual ~NExpression() {}
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

class NStatement : public Node {
public:
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
    // Rashed Al-Lahaseh - 00821573
    // Define a function to check void type by checking returned expression if NULL
    // Here by default definition should return 'false'
    virtual bool isVoid() const { return false; }
};

class NInteger : public NExpression {
    long long value;
public:
    virtual ~NInteger(){};
    NInteger(long long value) : value(value) { }
    long long getValue() const { return this->value; }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"value: "<<value<<std::endl;
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

class NIdentifier : public NExpression {
public:
    const std::string name;
    NIdentifier(const std::string& name) : name(name) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"name: "<<name<<std::endl;
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

// Rashed Al-Lahaseh - 00821573
// Define method call type
class NMethodCall : public NExpression {
public:
    PNODE(const NIdentifier) id;
    NExpressionList arguments;
    virtual ~NMethodCall(){
        for(auto elt:arguments){
            DEL_NODE(elt);
        }
    }
    NMethodCall(PNODE(const NIdentifier) id, const NExpressionList& arguments) :
        id(id), arguments(arguments) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout << "id: " << id << std::endl;
      std::cout<<"arguments: "<<std::endl;
      std::cout<<'['<<std::endl;
      for(auto elt:arguments){
          elt->print();
      }
      std::cout<<']'<<std::endl;
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

class NBinaryExpression: public NExpression {
public:
    int op;
    PNODE(const NExpression) lhs;
    PNODE(const NExpression) rhs;
    NBinaryExpression(PNODE(const NExpression) lhs, int op,
            PNODE(const NExpression) rhs) :
        op(op), lhs(lhs), rhs(rhs) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"op: "<<op<<std::endl;
      std::cout<<'['<<std::endl;
      std::cout<<"LEFT: "<<std::endl;
        lhs->print();
      std::cout<<"RIGHT: "<<std::endl;
        rhs->print();
      std::cout<<']'<<std::endl;
      std::cout<<']'<<std::endl;
    }
};

class NBinaryOperator: public NBinaryExpression {
public:
    NBinaryOperator(PNODE(const NExpression) lhs, int op,
            PNODE(const NExpression) rhs) :
            NBinaryExpression(lhs, op, rhs) {
    }
    virtual llvm::Value* codeGen(CodeGenContext &context) const;
};

class NComparisonOperator: public NBinaryExpression {
public:
    NComparisonOperator(PNODE(const NExpression) lhs, int op,
            PNODE(const NExpression) rhs) :
            NBinaryExpression(lhs, op, rhs) {
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

class NAssignment : public NExpression {
public:
    PNODE(const NIdentifier) lhs;
    PNODE(const NExpression) rhs;
    NAssignment(PNODE(const NIdentifier) lhs, PNODE(const NExpression) rhs) :
        lhs(lhs), rhs(rhs) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<'['<<std::endl;
      std::cout<<"LEFT: "<<std::endl;
        lhs->print();
      std::cout<<"RIGHT: "<<std::endl;
        rhs->print();
      std::cout<<']'<<std::endl;
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};


class NBlock : public NExpression {
public:
    NStatementList statements;
    NBlock() { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"statements: "<<std::endl;
      std::cout<<'['<<std::endl;
      for(auto elt:statements){
            elt->print();
      }
      std::cout<<']'<<std::endl;
      std::cout<<']'<<std::endl;
    }
    // Rashed Al-Lahaseh - 00821573
    // Define func to search for statements defined if the func is void type
    virtual bool foundVoid() const {
      bool availability = false;
      for(auto elt:statements) { if ((*elt).isVoid()) { availability = true; } }
      return availability;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

class NExpressionStatement : public NStatement {
public:
    PNODE(const NExpression) expression;
    NExpressionStatement(PNODE(const NExpression) expr) :
        expression(expr) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"expression: "<<std::endl;
        expression->print();
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(
            CodeGenContext& context) const;
};

class NReturnStatement : public NStatement {
public:
    PNODE(const NExpression) expression;
    NReturnStatement(PNODE(const NExpression) expr) :
        expression(expr) { }
    virtual void print() const{
      std::cout<<'['<<std::endl;
      printName();
        std::cout << "expression: " << std::endl;
        expression->print();
      std::cout<<']'<<std::endl;
    }
    // Rashed Al-Lahaseh - 00821573
    // Define a function to check void type by checking returned expression if NULL
    virtual bool isVoid() const {
      return expression == nullptr;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

// Rashed Al-Lahaseh - 00821573
// Add 'type' PNODE
class NVariableDeclaration : public NStatement {
public:
    PNODE(const NIdentifier) type;
    PNODE(const NIdentifier) id;
    PNODE(const NExpression) assignmentExpr;
    NVariableDeclaration(PNODE(const NIdentifier) type, PNODE(const NIdentifier) id) :
        type(type), id(id) { assignmentExpr = nullptr; }
    NVariableDeclaration(PNODE(const NIdentifier) type, PNODE(const NIdentifier) id,
            PNODE(const NExpression) assignmentExpr) :
        type(type), id(id), assignmentExpr(assignmentExpr) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"type: "<<std::endl;
      type->print();
      std::cout<<"id: "<<std::endl;
      id->print();
      if(nullptr!=assignmentExpr){
          std::cout<<"assignment:"<<std::endl;
          assignmentExpr->print();
      }
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

// Rashed Al-Lahaseh - 00821573
// Add 'type' PNODE
class NExternDeclaration : public NStatement {
public:
    PNODE(const NIdentifier) type;
    PNODE(const NIdentifier) id;
    NVariableList arguments;
    NExternDeclaration(PNODE(const NIdentifier) type,
            PNODE(const NIdentifier) id,
            const NVariableList &arguments) :
        type(type), id(id), arguments(arguments) {}
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"type: "<<std::endl;
      type->print();
      std::cout<<"id: "<<std::endl;
      id->print();
      std::cout<<'['<<std::endl;
      std::cout<<"arguments:"<<std::endl;
      for(auto elt:arguments){
          elt->print();
      }
      std::cout<<']'<<std::endl;
      std::cout<<']'<<std::endl;
    }
   virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

// Rashed Al-Lahaseh - 00821573
// Add 'type' PNODE
class NFunctionDeclaration : public NStatement {
public:
    PNODE(const NIdentifier) type;
    PNODE(const NIdentifier) id;
    NVariableList arguments;
    PNODE(const NBlock) block;
    NFunctionDeclaration(PNODE(const NIdentifier) type,
            PNODE(const NIdentifier) id,
            const NVariableList &arguments,
            PNODE(const NBlock) block) :
        type(type), id(id), arguments(arguments), block(block) { }
    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"type: "<<std::endl;
      type->print();
      std::cout<<"id: "<<std::endl;
      id->print();
      std::cout<<'['<<std::endl;
      std::cout<<"arguments:"<<std::endl;
      for(auto elt:arguments){
          elt->print();
      }
      std::cout<<']'<<std::endl;
      std::cout<<'['<<std::endl;
      std::cout<<"block:"<<std::endl;
      block->print();
      std::cout<<']'<<std::endl;
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

/// If statement, represented by a condition, an if-branch, and an else-branch
class NIfStatement: public NStatement {
public:
    PNODE(const NExpression) condition;
    PNODE(const NBlock) ifBranch;
    PNODE(const NBlock) elseBranch;

    NIfStatement(PNODE(const NExpression) newCond,
            PNODE(const NBlock) newIf,
            PNODE(const NBlock) newElse = nullptr):
    condition(newCond),ifBranch(newIf),elseBranch(newElse){};

    virtual void print() const{
      if(nullptr==this){return;}
      std::cout<<'['<<std::endl;
      printName();
      std::cout<<"condition: "<<std::endl;
        condition->print();
      std::cout<<"then: "<<std::endl;
        ifBranch->print();
      std::cout<<"else: "<<std::endl;
        elseBranch->print();
      std::cout<<']'<<std::endl;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context) const;
};

extern NBlock programBlock; /* the top level root node of our final AST */

#endif
