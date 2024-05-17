module Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.State
import Evaluator.Domain.Context
import Evaluator.Domain.Environment
import Evaluator.Domain.Error
import Evaluator.Domain.Monads
import Evaluator.Utils.Utils
import Syntax.AbsTortex
import Syntax.Utils (getOperation)

runEvaluate :: Program -> IO (Either EvaluationError Value)
runEvaluate program = runExceptT $ evalStateT (eval program) emptyContext

instance Evaluator Program where
  eval (PProgram inits) = do
    mapM_ eval inits
    eval $ EApp (Ident "main") []

instance Evaluator Init where
  eval (IFnDef name arguments _ block) = do
    ctx <- get
    let functionType = VFun arguments block (environment ctx)
    modify $ insertValue name functionType
    pure Dummy
 
  eval (IInit name _ expression) = do
    exprVal <- eval expression
    modify $ insertValue name exprVal
    pure Dummy

instance Evaluator Block where
  eval (SBlock statements) =
    do
      mapM_ eval statements
      pure Dummy
      `catchError` handleFlowControlError
    where
      handleFlowControlError :: EvaluationError -> EvaluatorT
      handleFlowControlError (ReturnCalled value) = pure value
      handleFlowControlError other = throwError other

instance Evaluator Stmt where
  eval SEmpty = pure VVoid
 
  eval (SBStmt block) = eval block
 
  eval (SInit si) = keepEnvAndEval $ eval si
 
  eval (SAss name expression) = do
    expressionVal <- eval expression
    modify $ updateValue name expressionVal
    return Dummy
 
  eval (SIncr name) = do
    evalIntStmt (1 +) name
 
  eval (SDecr name) = do
    evalIntStmt (\x -> x - 1) name
 
  eval (SRet expression) = do
    expressionVal <- eval expression
    throwError (ReturnCalled expressionVal)
 
  eval SRetVoid = do
    throwError (ReturnCalled VVoid)
 
  eval (SCond expression trueBlock) = do
    expressionVal <- eval expression
    keepEnvAndEval $ if isTrue expressionVal then eval trueBlock else return Dummy
 
  eval (SCondElse expression trueBlock falseBlock) = do
    expressionVal <- eval expression
    keepEnvAndEval $ if isTrue expressionVal then eval trueBlock else eval falseBlock
 
  eval while@(SWhile expression block) = do
    expressionVal <- eval expression
    keepEnvAndEval $
      if isTrue expressionVal then eval block >> eval while else return Dummy
 
  eval (SExp expression) = eval expression

instance Evaluator Expr where
  eval (EVar name) = gets $ getValue name
 
  eval (ELitInt value) = pure $ VInt value
 
  eval ELitTrue = pure $ VBool True
 
  eval ELitFalse = pure $ VBool False
 
  eval (EString value) = pure $ VString value
 
  eval (ENeg expression) = do
    expressionVal <- eval expression
    pure $ mapVInt ((-1) *) expressionVal
 
  eval (ENot expression) = do
    expressionVal <- eval expression
    pure $ mapVBool not expressionVal
 
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

  eval (ELambda arguments _ block) = gets (VFun arguments block . environment)

  eval (EApp name expressions) = do 
    ctx <- get
    argumentVals <- mapM eval expressions
    argumentLocs <- mapM getArgumentLocation expressions

    let env = environment ctx
    let function = getValue name ctx
    let functionEnv = getFunctionEnvironment function
    let functionArgs = getFunctionArgs function
    let functionBlock = getFunctionBlock function

    modify $ insertEnvironment functionEnv
    modify $ insertValue name function
    modify putReturnValue

    mapM_ insertArgsToCtx (zip3 functionArgs argumentVals argumentLocs)

    returnValue <- eval functionBlock

    modify $ insertEnvironment env
    pure returnValue
