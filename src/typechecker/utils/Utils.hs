module TypeChecker.Utils.Utils
  ( assertUniqueInitsOrThrow,
  )
where

import Control.Monad.Except
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Error
import TypeChecker.Utils.Monads

assertUniqueInitsOrThrow :: [Init] -> TypeCheckerT
assertUniqueInitsOrThrow inits = case checkUniqueInits inits of
  Left e -> throwError e
  Right _ -> return ()

checkUniqueInits :: [Init] -> Either TypeError (Set Ident)
checkUniqueInits = foldl (\acc x -> acc >>= tryInsert x) (Right Set.empty) . fmap getIdent
  where
    getIdent (IFnDef name _ _ _) = name
    getIdent (IInit name _ _) = name
    tryInsert e set =
      if Set.member e set then Left DuplicatedNameError else Right (Set.insert e set)

assertTypesOrThrow :: Eq a => a -> a -> TypeError -> TypeCheckerT
assertTypesOrThrow expected actual = assertOrThrow ((==) expected actual)

assertOrThrow :: Bool -> TypeError -> TypeCheckerT
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e
