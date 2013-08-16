{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, (<>))
import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Hakyll

main :: IO ()
main = hakyll $ do
  match "static/*" $ do
    route   idRoute
    compile copyFileCompiler
    
  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = 
            field "posts" (\_ -> postList recentFirst) `mappend`
            postCtx
            
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
      
  match "about.html" $ do
    route   idRoute
    compile copyFileCompiler
    
  match "resume.html" $ do
    route idRoute
    compile copyFileCompiler
    
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
        
  match "templates/*" $ compile templateCompiler
  
  create ["blog.html"] $ do 
    route idRoute
    compile $ do
      let blogCtx =
            field "posts" (\_ -> postList recentFirst) `mappend`
            defaultContext
            
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" blogCtx
        >>= loadAndApplyTemplate "templates/default.html" blogCtx
        >>= relativizeUrls
        
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl postCtx posts
  return list
  
-- teaserSeparator :: String
-- teaserSeparator = "<!--more-->"
  
-- teaserField :: String           -- ^ Key to use
--             -> Snapshot         -- ^ Snapshot to load
--             -> Context String   -- ^ Resulting context
-- teaserField key snapshot = field key $ \item ->
--     (needlePrefix teaserSeparator . itemBody) <$>
--     loadSnapshot (itemIdentifier item) snapshot
    
-- needlePrefix :: String -> String -> String
-- needlePrefix needle haystack = go haystack
--   where
--     go [] = []
--     go xss@(x:xs) | needle `isPrefixOf` xss = []
--                   | otherwise = x : go xs