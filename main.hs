{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), (&&&), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat, mappend)

import Hakyll

recipeCompiler :: Compiler Resource (Page String)
recipeCompiler = readPageCompiler >>> addDefaultFields

myCompiler :: Compiler () (Page String)
myCompiler = undefined

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
     
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
            
    -- Render posts
    group "index" $ do
        match "posts/*.html" $ do
            --route $ idRoute
            compile $ recipeCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")      
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> relativizeUrlsCompiler
                
        match "posts/*.markdown" $ do
            compile $ pageCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")      
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> relativizeUrlsCompiler      
            
    group "seperate" $ do
         match "posts/*.html"  $ do
            route $ (setExtension ".html") -- composeRoutes (setExtension ".html") (gsubRoute "posts/" (\_ -> "recipes/"))
            compile $ recipeCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")  
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
                >>> applyTemplateCompiler "templates/default.html"                                
                >>> relativizeUrlsCompiler
                
         match "posts/*.markdown" $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")      
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
                >>> applyTemplateCompiler "templates/default.html"                                
                >>> relativizeUrlsCompiler
                
                    
    -- -- Index
    --     match "index.html" $ route idRoute
    --     create "index.html" $ constA mempty
    --         >>> arr (setField "title" "Home")
    --         >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
    --         >>> requireAllA (inGroup (Just "index")) (id *** arr (take 3 . recentFirst) >>> allPosts)
    --         >>> applyTemplateCompiler "templates/index.html"
    --         >>> applyTemplateCompiler "templates/default.html"                  
    --         >>> relativizeUrlsCompiler
        
    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA (inGroup (Just "index")) (id *** arr (recentFirst) >>> allPosts)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"                  
        >>> relativizeUrlsCompiler 
        
    -- About
    match "about.html" $ route idRoute
    create "about.html" $ constA mempty
        >>> arr (setField "title" "About")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> applyTemplateCompiler "templates/about.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Recipes List
    match "postlist.html" $ route idRoute
    create "postlist.html" $ constA mempty
        >>> arr (setField "title" "All Posts")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA (inGroup (Just "seperate")) (id *** arr (recentFirst) >>> addPostList)
        >>> applyTemplateCompiler "templates/postlist.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
        
    -- Tags
    create "tags" $
        requireAll (inGroup (Just "index")) (\_ ps -> readTags ps :: Tags String)
        
    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*"
            >>> mapCompiler (arr $ copyBodyToField "description")
            >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler


renderTagCloud' :: Compiler (Tags String) String
renderTagCloud' = renderTagCloud tagIdentifier 100 120

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

--  Get all post in detail
allPosts :: Compiler (Page String, [Page String]) (Page String)
allPosts = setFieldA "posts" $
    arr (recentFirst)
       -- >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$postlist@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "postlist" $
    arr (recentFirst)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> allPosts
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "SimpleBlog RSS feed."
    , feedDescription = "A simple demo of an RSS feed created with Hakyll."
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "http://example.com"
    }
    