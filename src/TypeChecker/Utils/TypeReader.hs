module TypeChecker.Utils.TypeReader where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Error

assertTypeOrThrow :: TypeReader a => Type -> a -> EmptyTypeReaderT'
assertTypeOrThrow expectedType expr = do
  actualType <- readType expr
  assertOrThrow (expectedType == actualType) MismatchedTypesError

assertTypesOrThrow :: TypeReader a => Type -> a -> a -> EmptyTypeReaderT'
assertTypesOrThrow expectedType e1 e2 = do
  assertTypeOrThrow expectedType e1
  assertTypeOrThrow expectedType e2

assertTypesListOrThrow :: TypeReader a => [Type] -> [a] -> EmptyTypeReaderT'
assertTypesListOrThrow expectedTypes expressions
  | length expectedTypes /= length expressions = throwError MissingArgumentError
  | otherwise = forM_ (zip expectedTypes expressions) (uncurry assertTypeOrThrow)

assertOrThrow :: Bool -> TypeError -> EmptyTypeReaderT'
assertOrThrow True _ = return ()
assertOrThrow False e = throwError e

getExistingSymbolOrThrow :: Ident -> TypeError -> TypeReaderT
getExistingSymbolOrThrow name err = do
  symbolType <- asks (lookupIdent name)
  maybe (throwError err) return symbolType

assertValidArgumentsOrThrow :: [Arg] -> EmptyTypeReaderT'
assertValidArgumentsOrThrow args = either throwError return $ checkUniqueArguments args

checkUniqueArguments :: [Arg] -> Either TypeError ()
checkUniqueArguments = checkUniqueIdents . fmap getIdent
  where
    getIdent (PArg name _) = name
    getIdent (PArgVar name _) = name

checkUniqueIdents :: [Ident] -> Either TypeError ()
checkUniqueIdents = fmap (const ()) . foldl (\acc x -> acc >>= tryInsert x) (Right Set.empty)
  where
    tryInsert e set =
      if Set.member e set then Left DuplicatedNameError else Right (Set.insert e set)
