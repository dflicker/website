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
            constField "IndexCurrent" "class=\"current\"" `mappend`
            constField "BlogCurrent" "" `mappend`
            defaultContext
            
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
            postCtx
            
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" blogCtx
        >>= loadAndApplyTemplate "templates/default.html" blogCtx
        >>= relativizeUrls
        
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    constField "BlogCurrent" "class=\"current\"" `mappend`
    constField "IndexCurrent" "" `mappend`
    defaultContext

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl postCtx posts
  return list