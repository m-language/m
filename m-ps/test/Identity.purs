module Test.Identity where

import Prelude

import Data.Foldable (fold)
import Data.List (List(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, oneOf, resize)
import Tree (Tree(..))

fnIdentity :: Tree -> Tree
fnIdentity tree = ApplyTree (ApplyTree (ApplyTree (SymbolTree "fn") (SymbolTree "x")) (SymbolTree "x")) tree

fmIdentity :: Tree -> Tree
fmIdentity tree = ApplyTree (ApplyTree (ApplyTree (SymbolTree "fm") (SymbolTree "x")) (SymbolTree "x")) tree

fn0Identity :: Tree -> Tree
fn0Identity tree = ApplyTree (ApplyTree (SymbolTree "fn") (ListTree Nil)) tree

fm0Identity :: Tree -> Tree
fm0Identity tree = ApplyTree (ApplyTree (SymbolTree "fm") (ListTree Nil)) tree

blockIdentity :: Tree -> Tree
blockIdentity tree = ApplyTree (ApplyTree (SymbolTree "block") (ListTree Nil)) tree

defIdentity :: Tree -> Tree
defIdentity tree = ApplyTree (ApplyTree (ApplyTree (SymbolTree "def") (SymbolTree "x")) tree) (SymbolTree "x")

newtype Identity = Identity (Tree -> Tree)

derive instance identityNewtype :: Newtype Identity _

instance identitySemigroup :: Semigroup Identity where
  append (Identity f) (Identity g) = Identity $ f >>> g

instance identityMonoid :: Monoid Identity where
  mempty = Identity \x -> x

identityGen :: Gen Identity
identityGen = oneOf $ (pure $ Identity \x -> x) :| 
    map (pure <<< Identity) 
      [ fnIdentity
      , fmIdentity
      , fn0Identity
      , fm0Identity
      , blockIdentity
      , defIdentity ]

instance arbitraryIdentity :: Arbitrary Identity where
  arbitrary = resize 100 $ fold <$> arrayOf identityGen
