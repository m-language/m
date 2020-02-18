module Tree where

data Tree
    = SymbolTree String
    | IntTree Integer
    | CharTree Char
    | StringTree String
    | ApplyTree [Tree]
    deriving (Eq)

instance Show Tree where
    show (SymbolTree name  ) = name
    show (IntTree    int   ) = show int
    show (CharTree   char  ) = show char
    show (StringTree string) = show string
    show (ApplyTree  args  ) = "(" ++ unwords (map show args) ++ ")"
