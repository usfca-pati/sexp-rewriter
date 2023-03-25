-- | Abstract syntax for S-expressions with holes for rewriting.

module Syntax (
  SExp(..)
  ) where

-- | S-expressions with holes.
data SExp a = Atom String
            | App [SExp a]
            | Hole a
            deriving (Eq, Ord)

instance Show a => Show (SExp a) where
  show (Atom s) = s
  show (Hole a) = show a
  show (App []) = "()"
  show (App (s1 : sRest)) = (showParen True $
    foldl (\x y s -> x (" " ++ (y s))) (shows s1) (map shows sRest)) ""
