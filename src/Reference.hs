module Reference ( Reference (Reference, unparsed)
                 , Feature (..)
                 ) where

data Reference = Reference { comment :: String
                           , author :: String
                           , year :: Integer
                           , title :: String
                           , features :: [Feature]
                           , unparsed :: String
                           } deriving (Eq)

data Feature = BookTitle String
             | Editor String
             | Page Integer
             | PageRange Integer Integer
             | Volume Integer
             deriving (Eq)

instance Show Reference where
  show ref = "@comment{ " ++ comment ref ++ " }\n"
           ++ "@article{\n"
           ++ "\tauthor = " ++ show (author ref) ++ ",\n"
           ++ "\ttitle = "  ++ show (title ref)  ++ ",\n"
           ++ "\tyear = "   ++ show (year ref)   ++ ",\n"
           ++ concatMap show (features ref)
           ++ "}"


instance Show Feature where
  show (Editor editor)   = "\teditor = \""    ++ editor      ++ "\",\n"
  show (BookTitle title) = "\tbooktitle = \"" ++ title       ++ "\",\n"
  show (Page page)       = "\tpage = \""      ++ show page   ++ "\",\n"
  show (PageRange n m)   = "\tpages = \""     ++ show n      ++ "--"    ++ show m ++ "\",\n"
  show (Volume volume)   = "\tvolume = \""    ++ show volume ++ "\",\n"

