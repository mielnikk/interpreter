{-# LANGUAGE FlexibleInstances #-}

module TypeChecker.Utils
  ( assertUniqueInits,
    assertValidArguments,
    assertExpressionType,
    assertVariableType,
    assertTypesOrThrow,
    assertOrThrow,
    getExistingSymbolOrThrow,
    assertExpressionsType,
    assertExpressionListTypes,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Domain.RawType
import TypeChecker.Error

assertVariableType :: RawType -> Ident -> BNFC'Position -> TypeCheckerT ()
assertVariableType expectedType name position = do
  env <- get
  case lookupIdent name env of
    Just variableType ->
      assertTypesOrThrow expectedType variableType (InvalidTypeError position expectedType variableType)
    Nothing -> throwError $ UnknownIdentifierError position name

assertValidArguments :: [Arg] -> TypeCheckerT ()
assertValidArguments args = either throwError return $ checkUniqueArguments args

checkUniqueArguments :: [Arg] -> Either TypeError ()
checkUniqueArguments = checkUniqueIdents . fmap getIdent
  where
    getIdent (PArg position name _) = (name, position)
    getIdent (PArgVar position name _) = (name, position)

assertUniqueInits :: [Init] -> TypeCheckerT ()
assertUniqueInits inits = either throwError return $ checkUniqueInits inits

checkUniqueInits :: [Init] -> Either TypeError ()
checkUniqueInits = checkUniqueIdents . fmap getIdent
  where
    getIdent (IFnDef position name _ _ _) = (name, position)
    getIdent (IInit position name _ _) = (name, position)

checkUniqueIdents :: [(Ident, BNFC'Position)] -> Either TypeError ()
checkUniqueIdents = fmap (const ()) . foldl (\acc (name, position) -> acc >>= tryInsert name position) (Right Set.empty)
  where
    tryInsert name' position' set =
      if Set.member name' set then Left (DuplicatedNameError position' name') else Right (Set.insert name' set)

assertTypesOrThrow :: Eq a => a -> a -> TypeError -> TypeCheckerT ()
assertTypesOrThrow expected actual = assertOrThrow ((==) expected actual)

assertOrThrow :: Bool -> TypeError -> TypeCheckerT ()
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e

getExistingSymbolOrThrow :: Ident -> TypeError -> TypeCheckerT RawType
getExistingSymbolOrThrow name err = do
  symbolType <- gets (lookupIdent name)
  maybe (throwError err) return symbolType

assertExpressionType :: TypeReader a => RawType -> a -> TypeCheckerT ()
assertExpressionType expectedType expr = do
  actualType <- readType expr
  let pos = hasPosition expr
  assertOrThrow (expectedType == actualType) (WrongExpressionType pos expectedType actualType)

assertExpressionsType :: TypeReader a => RawType -> a -> a -> TypeCheckerT ()
assertExpressionsType expectedType e1 e2 = do
  assertExpressionType expectedType e1
  assertExpressionType expectedType e2

assertExpressionListTypes :: TypeReader a => [RawType] -> [a] -> TypeCheckerT ()
assertExpressionListTypes expectedTypes expressions = forM_ (zip expectedTypes expressions) (uncurry assertExpressionType)
