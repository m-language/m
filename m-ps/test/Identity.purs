module Test.Identity where

import Prelude

import Data.Foldable (fold, foldl)
import Data.List (List(..), (:), fromFoldable, singleton)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, oneOf, resize)
import Tree (Tree(..))

applyIdentity :: Tree -> Tree -> Tree
applyIdentity f a = ApplyTree (f : a : Nil)

curryIdentity :: Tree -> Tree
curryIdentity (ApplyTree (car : cdr)) = foldl (\f x -> ApplyTree (f : x : Nil)) (curryIdentity car) cdr
curryIdentity tree = ApplyTree $ singleton tree

fnIdentity :: Tree -> Tree
fnIdentity tree = ApplyTree $ fromFoldable [SymbolTree "fn", SymbolTree "x", SymbolTree "x", tree]

fmIdentity :: Tree -> Tree
fmIdentity tree = ApplyTree $ fromFoldable [SymbolTree "fm", SymbolTree "x", SymbolTree "x", tree]

blockIdentity :: Tree -> Tree
blockIdentity tree = ApplyTree $ fromFoldable [SymbolTree "block", ApplyTree Nil, tree]

defIdentity :: Tree -> Tree
defIdentity tree = ApplyTree $ fromFoldable [SymbolTree "def", SymbolTree "x", tree, SymbolTree "x"]

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
      , blockIdentity
      , defIdentity
      , curryIdentity ]

instance arbitraryIdentity :: Arbitrary Identity where
  arbitrary = resize 100 $ fold <$> arrayOf identityGen
