-- |Parsing a naming convention.
--
-- Decode a pathname specified by the user, and deduce a naming
-- convention from it.

module Data.Weave.Naming.Parse (
    parseNaming
) where

import System.FilePath

import Data.Weave.Naming.Internal

-- |Parse a filename into a SimpleNaming convention.  The filename
-- will be broken down into a prefix, a preferred extension, and
-- whether it is compressed or not.  Currently, the only recognized
-- extension is '.dat' or '.dat.gz'.
parseNaming :: String -> Either String SimpleNaming
parseNaming name =
    let (isComp, name2) =
            let (a, b) = splitExtension name in
            if b == ".gz" then (True, a) else (False, name) in
    let (a, b) = splitExtension name2 in
    if b == ".dat" then
        Right $ SimpleNaming a "dat" isComp
    else
        Left $ "Unknown file: should be name.dat.gz or name.dat"
