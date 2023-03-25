-- | Unification-based rewriting.
--
-- The rewriting engine unifies the left-hand side of the rewrite rule with the
-- given input regex.  If unification succeeds, then it outputs the right-hand
-- side of the rewrite rule.  Otherwise, it proceeds recursively.

module Rewriting (
  HoleVar(..),
  Rule(..),
  applyRule,
  rewrite,
  unify
  ) where

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Void
import Syntax

-- | Variables that are used in holes in the rewrite rule.
newtype HoleVar = HoleVar Int deriving (Eq, Ord)

instance Show HoleVar where
  show (HoleVar n) = "$" ++ show n

-- | Substitutions are just mapping of holes to S-Exps, i.e. the partial
-- unification map.  We never map a hole to another hole, so this representation
-- is sufficient.
type Subst = Map HoleVar (SExp Void)

-- | Try matching (unifying) given input and left-hand side.  This is simpler
-- than full unification because the input does not have any holes.
unify :: Subst -> SExp Void -> SExp HoleVar -> Maybe Subst
unify σ (Atom s) (Atom t)
  | s == t    = Just σ
  | otherwise = Nothing
unify σ (App s) (App t)
  | length s /= length t = Nothing
  | otherwise = foldl unify' (Just σ) (zip s t)
  where unify' σO p = σO >>= \σ' -> uncurry (unify σ') p
unify σ sexp (Hole h) = case σ !? h of
  Nothing -> Just $ M.insert h sexp σ
  Just sexp' | sexp == sexp' -> Just σ
             | otherwise     -> Nothing
unify _ _ _ = Nothing

-- | Rewrite the right-hand side of a rewrite rule based on the given
-- substitution.  This *deliberately fails* if there are some variables that are
-- not unified.
rewrite :: Subst -> SExp HoleVar -> Maybe (SExp Void)
rewrite _ (Atom s) = Just $ Atom s
rewrite σ (App sexps) = App <$> mapM (rewrite σ) sexps
rewrite σ (Hole h) = σ !? h

-- | A rewrite rule is just a pair of s-expressions with holes.
data Rule = Rule (SExp HoleVar) (SExp HoleVar)
  deriving Eq

instance Show Rule where
  show (Rule s1 s2) = show s1 ++ "\n->\n" ++ show s2

-- | Apply a substitution rule, if it fails, apply it recursively.
applyRule :: Rule -> SExp Void -> SExp Void
applyRule rule@(Rule lhs rhs) sexp = fromMaybe recurse applyAtTopLevel
  where
    applyAtTopLevel = unify M.empty sexp lhs >>= flip rewrite rhs
    recurse = case sexp of
      Atom _ -> sexp
      Hole _ -> undefined -- this should be unreachable
      App sexps -> App $ fmap (applyRule rule) sexps
