{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.JSON.Tagless where

import Control.Applicative
import Text.Parser.Token
import Text.Parser.Char
import Text.Parser.Combinators

import Data.List (intersperse)

----------------------------------------------------------------------------

class JSON a where
  jsNum :: Double -> a
  jsString :: String -> a
  jsBool :: Bool -> a
  jsNull :: a
  jsArray :: [a] -> a
  jsDic :: [(String, a)] -> a

----------------------------------------------------------------------------

newtype JSRenderer = JSRenderer { runJSRenderer :: String }
instance JSON JSRenderer where
  jsNum = JSRenderer . show
  jsString = JSRenderer . show
  jsBool x = JSRenderer $ if x then "true" else "false"
  jsNull = JSRenderer "null"
  jsArray xs = JSRenderer $ "[" ++ (concat $ intersperse "," $ map runJSRenderer xs) ++ "]"
  jsDic xs = JSRenderer $ "{" ++ (concat $ intersperse "," $ [ show k ++ " : " ++ runJSRenderer v | (k, v) <- xs ]) ++ "}"

instance Show JSRenderer where
  show = runJSRenderer

----------------------------------------------------------------------------

newtype JSStore = JSStore { runJSStore :: forall r.
                                          (Double -> r)
                                          -> (String -> r)
                                          -> (Bool -> r)
                                          -> r
                                          -> ([JSStore] -> r)
                                          -> ([(String, JSStore)] -> r)
                                          -> r }
instance JSON JSStore where
  jsNum x = JSStore $ \f _ _ _ _ _ -> f x
  jsString x = JSStore $ \_ f _ _ _ _ -> f x
  jsBool x = JSStore $ \_ _ f _ _ _ -> f x
  jsNull = JSStore $ \_ _ _ x _ _ -> x
  jsArray xs = JSStore $ \_ _ _ _ f _ -> f xs
  jsDic xs = JSStore $ \_ _ _ _ _ f -> f xs

isNull :: JSStore -> Bool
isNull x = runJSStore x g g g True g g where g = const False

toNum :: JSStore -> Maybe Double
toNum x = runJSStore x Just g g Nothing g g where g = const Nothing
toString :: JSStore -> Maybe String
toString x = runJSStore x g Just g Nothing g g where g = const Nothing
toBool :: JSStore -> Maybe Bool
toBool x = runJSStore x g g Just Nothing g g where g = const Nothing
toArray :: JSStore -> Maybe [JSStore]
toArray x = runJSStore x g g g Nothing Just g where g = const Nothing
toDic :: JSStore -> Maybe [(String, JSStore)]
toDic x = runJSStore x g g g Nothing g Just where g = const Nothing

toRenderer :: JSStore -> JSRenderer
toRenderer x = runJSStore x jsNum jsString jsBool jsNull
                  (jsArray . map toRenderer)
                  (jsDic . map (fmap toRenderer))

----------------------------------------------------------------------------

jsonParser :: forall n js. (TokenParsing n, JSON js) => n js
jsonParser = spaces *> valueParser where
  valueParser :: (TokenParsing n, JSON js) => n js
  valueParser =     jsDic <$> (between (symbolic '{') (symbolic '}') $
                        commaSep $ ((,) <$> stringLiteral <* symbolic ':' <*> valueParser) )
                <|> jsArray <$> (between (symbolic '[') (symbolic ']') $ commaSep valueParser)
                <|> jsNum <$> (either fromIntegral id <$> integerOrDouble)
                <|> jsString <$> stringLiteral
                <|> symbol "true" *> pure (jsBool True)
                <|> symbol "false" *> pure (jsBool False)
                <|> symbol "null" *> pure jsNull

