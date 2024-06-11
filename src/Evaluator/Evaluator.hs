module Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.State
import qualified Evaluator.Domain.Context as Context
import Evaluator.Domain.Error
import Evaluator.Domain.Monads
import Evaluator.Domain.Value 
import Evaluator.Utils.Utils
import Syntax.AbsTortex
import Syntax.Utils (getOperation)

instance Evaluator Program where
  eval (PProgram inits) = do
    mapM_ eval inits
    eval $ EApp (Ident "main") []

instance Evaluator Init where
  eval (IFnDef name arguments _ block) = do
    ctx <- get
    let functionType = VFun arguments block (Context.environment ctx)
    modify $ Context.insertValue name functionType
    return Dummy
 
  eval (IInit name _ expression) = do
    exprVal <- eval expression
    modify $ Context.insertValue name exprVal
    return Dummy

instance Evaluator Block where
  eval (SBlock statements) = do
    mapM_ evalIfNotReturned statements
    return Dummy
    where
      evalIfNotReturned stmt = do
        ctx <- get
        if Context.returnFlag ctx then return Dummy else eval stmt

instance Evaluator Stmt where
  eval SEmpty = return VVoid
 
  eval (SBStmt block) = evalRollbackEnv $ eval block
 
  eval (SInit si) = eval si
 
  eval (SAss name expression) = do
    expressionVal <- eval expression
    modify $ Context.updateValue name expressionVal
    return Dummy
 
  eval (SIncr name) = do
    evalIntStmt (1 +) name
 
  eval (SDecr name) = do
    evalIntStmt (\x -> x - 1) name
 
  eval (SRet expression) = do
    expressionVal <- eval expression
    modify $ Context.setReturned expressionVal
    return Dummy

  eval SRetVoid = do
    modify $ Context.setReturned VVoid
    return Dummy
 
  eval (SCond expression trueBlock) = do
    expressionVal <- eval expression
    evalRollbackEnv $ if isTrue expressionVal then eval trueBlock else return Dummy
 
  eval (SCondElse expression trueBlock falseBlock) = do
    expressionVal <- eval expression
    evalRollbackEnv $ if isTrue expressionVal then eval trueBlock else eval falseBlock
 
  eval while@(SWhile expression block) = do
    expressionVal <- eval expression
    evalRollbackEnv $ if isTrue expressionVal then eval block >> eval while else return Dummy
 
  eval (SExp expression) = eval expression

instance Evaluator Expr where
  eval (EVar name) = gets $ Context.getValue name
 
  eval (ELitInt value) = return $ VInt value
 
  eval ELitTrue = return $ VBool True
 
  eval ELitFalse = return $ VBool False
 
  eval (EString value) = return $ VString value
 
  eval (ENeg expression) = do
    expressionVal <- eval expression
    return $ mapVInt ((-1) *) expressionVal
 
  eval (ENot expression) = do
    expressionVal <- eval expression
    return $ mapVBool not expressionVal
 
  eval (EMul e1 Mod e2) = evalIntExpr mod e1 e2
 
  eval (EMul e1 Times e2) = evalIntExpr (*) e1 e2
 
  eval (EMul e1 Div e2) = do
    val2 <- eval e2
    if isIntEqual val2 0 then throwError DivideByZeroError else evalIntExpr quot e1 e2
 
  eval (EAdd e1 Plus e2) = evalIntExpr (+) e1 e2
 
  eval (EAdd e1 Minus e2) = evalIntExpr (-) e1 e2
 
  eval (ERel e1 op e2) = evalIntBoolExpr (getOperation op) e1 e2
 
  eval (EAnd e1 e2) = evalBoolExpr (&&) e1 e2
 
  eval (EOr e1 e2) = evalBoolExpr (||) e1 e2

  eval (ELambda arguments _ block) = gets (VFun arguments block . Context.environment)

  eval (EApp name expressions) = evalWithBuiltinCheck name expressions $ do 
    ctx <- get
    argumentVals <- mapM eval expressions
    argumentLocs <- mapM getArgumentLocation expressions

    let env = Context.environment ctx
    let function = Context.getValue name ctx
    let functionEnv = getFunctionEnvironment function
    let functionArgs = getFunctionArgs function
    let functionBlock = getFunctionBlock function

    modify $ Context.insertEnvironment functionEnv
    modify $ Context.insertValue name function
    modify $ Context.setReturnFlag False

    mapM_ insertArgsToCtx (zip3 functionArgs argumentVals argumentLocs)

    _ <- eval functionBlock
    
    newCtx <- get
    let returnValue = Context.getReturnValue newCtx

    modify $ Context.setReturnFlag False
    modify $ Context.insertEnvironment env
    return returnValue
