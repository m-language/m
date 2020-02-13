module Tree where

data Tree
    = Symbol String
    | CharTree Char
    | StringTree String
    | IntTree Integer
    | Apply [Tree]
    deriving (Eq)

instance Show Tree where
    show (Symbol     name  ) = name
    show (CharTree   char  ) = show char
    show (StringTree string) = show string
    show (IntTree    int   ) = show int
    show (Apply      args  ) = "(" ++ unwords (map show args) ++ ")"
