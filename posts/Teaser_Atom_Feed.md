---
published: 2013-09-01
title: Generating an Atom feed using Hakyll and teasers
---

I use [Hakyll](http://jaspervdj.be/hakyll/index.html) to generate this site, and I take 
advantage of the built-in teaser functionality to make the list of blog posts and their 
intro blurbs on the front page. Today I wanted to put those teasers on my Atom feed, 
also generated with Hakyll. <!--more-->

I followed the [teaser tutorial](http://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html)
and the [Atom feed tutorial](http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html)
to get the basic structures done. Now, I needed a way to transform the saved snapshot 
to just the teaser portion. Here is the original flow for generating the Atom XML file:

~~~~~{.haskell}
posts <- fmap (take 10) . recentFirst =<< 
            loadAllSnapshots "posts/*" "content"
~~~~~

First, I load all the snapshots (pages after they have been transformed from markdown to 
html), sort them based on published date, and take the last 10 posts. To just post the 
teaser and not the entire article, I defined a new function:

~~~~~{.haskell}
getTeaserContents :: [Item String] -> [Item String]
getTeaserContents = 
  map $ \x -> itemSetBody (fromMaybe (itemBody x) $ 
    needlePrefix teaserSeparator $ itemBody x) x
~~~~~

This lambda function takes an Item String, gets the text inside and  selects all the text 
before the teaser separator using needlePrefix, a Hakyll function. Since needlePrefix 
returns Maybe String, if the teaser separator isn't found, I just use the entire post. 
Finally, I set the body of the Item String to the new String. I then use map to apply 
this lambda to each Item String.

The final change is to plug this transform in the post processing flow:

~~~~~{.haskell}
    posts <- fmap (take 10) . recentFirst =<< (return . getTeaserContents) =<< 
      loadAllSnapshots "posts/*" "content"
~~~~~

The return is needed to lift the [Item String] to Compiler [Item String].

I'm still feeling my way around Haskell and Hakyll, so if someone knows a better way to do this let me know.

