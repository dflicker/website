{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, (<>))
import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Hakyll
import Data.Maybe (fromMaybe)

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
    compile $ do 
      pandocCompiler
       >>= saveSnapshot "content"
       >>= loadAndApplyTemplate "templates/post.html" postCtx
       >>= loadAndApplyTemplate "templates/blog-base.html" postCtx
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
        >>= loadAndApplyTemplate "templates/blog-base.html" blogCtx
        >>= relativizeUrls
        
  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend`
                    bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< (return . getTeaserContents) =<< loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration feedCtx posts
      
-- Context for pages
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    constField "BlogCurrent" "class=\"current\"" `mappend`
    constField "IndexCurrent" "" `mappend`
    defaultContext

-- Makes list of posts with base template applied
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl postCtx posts
  return list

-- Configuration for atom feed
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "David Flicker: latest posts"
    , feedDescription = "Posts on robotics, PCBs, microcontrollers and more"
    , feedAuthorName  = "David Flicker"
    , feedAuthorEmail = "dtflicker@gmail.com"
    , feedRoot        = "http://www.davidflicker.com"
    }
    
-- Gets the teaser from a list of posts
getTeaserContents :: [Item String] -> [Item String]
getTeaserContents = 
  map $ \x -> itemSetBody (fromMaybe (itemBody x) $ needlePrefix teaserSeparator $ itemBody x) x
  
teaserSeparator :: String
teaserSeparator = "<!--more-->"