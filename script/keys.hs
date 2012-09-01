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
parseKey :: String -> Maybe String
parseKey str = case break (== '=') str of
    (_ , "")                         -> Nothing
    (name, _)
        | "SDLK_" `isPrefixOf` name' -> Just name'
        | otherwise                  -> Nothing
      where
        name' = trim name
        trim  = reverse . dropWhile isSpace . reverse . dropWhile isSpace


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
makeDefinition :: String -> [String]
makeDefinition name =
    [ "#{enum Key, Key, " ++ name' ++ " = " ++ name ++ "}"
    , "{-# INLINE " ++ name' ++ " #-}"
    ]
  where
    name' = sanitizeKeyName name


--------------------------------------------------------------------------------
main :: IO ()
main = do
    keys <- mapMaybe parseKey . lines <$> getContents

    -- Print export list
    putStrLn $ unlines $ makeExportList keys

    -- Print definitions
    putStrLn $ unlines $ concat $ map makeDefinition keys
