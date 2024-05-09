module TypeChecker.Utils.TypeChecker
  ( assertUniqueInitsOrThrow,
    assertValidArgumentsOrThrow,
    assertTypeOrThrow,
  )
where

import Control.Monad.Except
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Monads
import qualified Typechecker.Utils.TypeReader as TR
import TypeChecker.Error

assertTypeOrThrow :: TypeReader a => Environment -> Type -> a -> TypeCheckerT
assertTypeOrThrow env expectedType expr = do 
  let typeCheckResult = assertType env expectedType expr 
  

assertType :: TypeReader a => Environment -> Type -> a -> Either TypeError ()
assertType expectedType expr env = 
  runExcept $ runReader (TR.assertTypeOrThrow expectedType expr) env

assertValidArgumentsOrThrow :: [Arg] -> TypeCheckerT
assertValidArgumentsOrThrow args = either throwError return $ checkUniqueArguments args

checkUniqueArguments :: [Arg] -> Either TypeError ()
checkUniqueArguments args = checkUniqueIdents . fmap getIdent
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
checkUniqueIdents = returnResult . foldl (\acc x -> acc >>= tryInsert x) (Right Set.empty)
  where
    tryInsert e set =
      if Set.member e set then Left DuplicatedNameError else Right (Set.insert e set)
    returnResult (Right _) = Right ()
    returnResult e = e

assertTypesOrThrow :: Eq a => a -> a -> TypeError -> TypeCheckerT
assertTypesOrThrow expected actual = assertOrThrow ((==) expected actual)

assertOrThrow :: Bool -> TypeError -> TypeCheckerT
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e
