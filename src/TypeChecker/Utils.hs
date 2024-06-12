module TypeChecker.Utils
  ( assertUniqueInits,
    assertValidArguments,
    assertExpressionType,
    assertVariableType,
    assertTypesOrThrow,
    assertOrThrow,
    checkUniqueArguments,
    assertValidLambdaBody,
    getExistingSymbolOrThrow,
    assertExpressionTypesOrThrow,
    assertTypesListOrThrow,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Error

assertVariableType :: Type -> Ident -> TypeCheckerT ()
assertVariableType expectedType name = do
  env <- get
  case lookupIdent name env of
    Just variableType -> assertTypesOrThrow expectedType variableType (InvalidTypeError expectedType variableType)
    Nothing -> throwError $ UnknownIdentifierError name

assertValidArguments :: [Arg] -> TypeCheckerT ()
assertValidArguments args = either throwError return $ checkUniqueArguments args

checkUniqueArguments :: [Arg] -> Either TypeError ()
checkUniqueArguments = checkUniqueIdents . fmap getIdent
  where
    getIdent (PArg name _) = name
    getIdent (PArgVar name _) = name

assertUniqueInits :: [Init] -> TypeCheckerT ()
assertUniqueInits inits = either throwError return $ checkUniqueInits inits

checkUniqueInits :: [Init] -> Either TypeError ()
checkUniqueInits = checkUniqueIdents . fmap getIdent
  where
    getIdent (IFnDef name _ _ _) = name
    getIdent (IInit name _ _) = name

checkUniqueIdents :: [Ident] -> Either TypeError ()
checkUniqueIdents = fmap (const ()) . foldl (\acc x -> acc >>= tryInsert x) (Right Set.empty)
  where
    tryInsert e set =
      if Set.member e set then Left (DuplicatedNameError e) else Right (Set.insert e set)

assertTypesOrThrow :: Eq a => a -> a -> TypeError -> TypeCheckerT ()
assertTypesOrThrow expected actual = assertOrThrow ((==) expected actual)

assertOrThrow :: Bool -> TypeError -> TypeCheckerT ()
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e

assertValidLambdaBody :: TypeChecker a => Type -> a -> TypeCheckerT ()
assertValidLambdaBody expectedType body = do
  _ <- checkType (Just expectedType) body
  env <- get
  assertOrThrow (returnStatementOccuredFlag env) MissingReturnStatementError

getExistingSymbolOrThrow :: Ident -> TypeError -> TypeCheckerT Type
getExistingSymbolOrThrow name err = do
  symbolType <- gets (lookupIdent name)
  maybe (throwError err) return symbolType

assertExpressionType :: TypeReader a => Type -> a -> TypeCheckerT ()
assertExpressionType expectedType expr = do
  actualType <- readType expr
  assertOrThrow (expectedType == actualType) (WrongExpressionType expectedType actualType)

assertExpressionTypesOrThrow :: TypeReader a => Type -> a -> a -> TypeCheckerT ()
assertExpressionTypesOrThrow expectedType e1 e2 = do
  assertExpressionType expectedType e1
  assertExpressionType expectedType e2

assertTypesListOrThrow :: TypeReader a => [Type] -> [a] -> TypeCheckerT ()
assertTypesListOrThrow expectedTypes expressions
  | length expectedTypes /= length expressions = throwError MissingArgumentError
  | otherwise = forM_ (zip expectedTypes expressions) (uncurry assertExpressionType)
