{-# LANGUAGE FlexibleContexts #-}

module TypeChecker.Utils.TypeChecker
  ( assertUniqueInitsOrThrow,
    assertValidArgumentsOrThrow,
    assertExpressionTypeOrThrow,
    assertVariableTypeOrThrow,
    assertTypesOrThrow,
    assertOrThrow,
    checkUniqueArguments,
    assertValidLambdaBodyOrThrow,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Error
import qualified TypeChecker.Utils.TypeReader as TR

assertVariableTypeOrThrow :: Type -> Ident -> TypeCheckerT
assertVariableTypeOrThrow expectedType name = do
  env <- get
  case lookupIdent name env of
    Just variableType -> assertTypesOrThrow expectedType variableType (InvalidTypeError expectedType variableType)
    Nothing -> throwError $ UnknownIdentifierError name

assertExpressionTypeOrThrow :: TypeReader a => Type -> a -> TypeCheckerT
assertExpressionTypeOrThrow expectedType expr = do
  _ <- TR.assertTypeOrThrow expectedType expr
  return ()

assertValidArgumentsOrThrow :: [Arg] -> TypeCheckerT
assertValidArgumentsOrThrow args = either throwError return $ checkUniqueArguments args

checkUniqueArguments :: [Arg] -> Either TypeError ()
checkUniqueArguments = checkUniqueIdents . fmap getIdent
  where
    getIdent (PArg name _) = name
    getIdent (PArgVar name _) = name

assertUniqueInitsOrThrow :: [Init] -> TypeCheckerT
assertUniqueInitsOrThrow inits = either throwError return $ checkUniqueInits inits

checkUniqueInits :: [Init] -> Either TypeError ()
checkUniqueInits = checkUniqueIdents . fmap getIdent
  where
    getIdent (IFnDef name _ _ _) = name
    getIdent (IInit name _ _) = name

checkUniqueIdents :: [Ident] -> Either TypeError ()
checkUniqueIdents = fmap (const ()) . foldl (\acc x -> acc >>= tryInsert x) (Right Set.empty)
  where
    tryInsert e set =
      if Set.member e set then Left DuplicatedNameError else Right (Set.insert e set)

assertTypesOrThrow :: Eq a => a -> a -> TypeError -> TypeCheckerT
assertTypesOrThrow expected actual = assertOrThrow ((==) expected actual)

assertOrThrow :: Bool -> TypeError -> TypeCheckerT
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e

assertValidLambdaBodyOrThrow :: TypeChecker a => Type -> a -> TypeCheckerT' ()
assertValidLambdaBodyOrThrow expectedType body = do
  _ <- checkType (Just expectedType) body
  env <- get
  assertOrThrow (returnStatementOccuredFlag env) MissingReturnStatementError
