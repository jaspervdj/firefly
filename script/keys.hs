--------------------------------------------------------------------------------
-- | Run this on SDL/SDL_keysym.h
module Main where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Data.Char           (isAlpha, isSpace, toLower)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (mapMaybe)


--------------------------------------------------------------------------------
-- | Parses something of the form: "SDLK_XXX = 323,"
parseKey :: String -> Maybe (String, Int)
parseKey str = case break (== '=') str of
    (_ , "")                         -> Nothing
    (name, rest)
        | "SDLK_" `isPrefixOf` name' -> Just (name', val)
        | otherwise                  -> Nothing
      where
        name' = trim name
        trim  = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        val   = read $ trim $ takeWhile (/= ',') $ drop 1 rest


--------------------------------------------------------------------------------
sanitizeKeyName :: String -> String
sanitizeKeyName str = ensureSane $ map toLower $
    filter (/= '_') $
    drop (length "SDLK_") str
  where
    ensureSane ""   = str
    ensureSane (x : xs)
        | isAlpha x = x : xs
        | otherwise = 'k' : x : xs


--------------------------------------------------------------------------------
makeExportList :: [String] -> [String]
makeExportList []       = error "Herp"
makeExportList (n : ns) =
    ["    ( " ++ sanitizeKeyName n] ++
    map (\x -> "    , " ++ sanitizeKeyName x) ns ++
    ["    ) where"]


--------------------------------------------------------------------------------
makeDefinition :: String -> Int -> [String]
makeDefinition name val =
    [ name' ++ " :: Key"
    , name' ++ " = Key " ++ show val
    , "{-# INLINE " ++ name' ++ " #-}"
    , ""
    ]
  where
    name' = sanitizeKeyName name


--------------------------------------------------------------------------------
main :: IO ()
main = do
    keys <- mapMaybe parseKey . lines <$> getContents

    -- Print export list
    putStrLn $ unlines $ makeExportList $ map fst keys

    -- Print definitions
    putStrLn $ unlines $ concat $ map (uncurry makeDefinition) keys
