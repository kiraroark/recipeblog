{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), arr, (>>^))
import Data.Monoid (mempty, mconcat)

import Hakyll

import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath ((</>))
import Debug.Trace

import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.String.Utils
import Data.Maybe

import RecipeParser

-- Number of posts in a page.
articlesPerIndexPage :: Int
articlesPerIndexPage = 2

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
    group "index" $
        match "posts/*.txt" $
            compile $ recipeCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")      
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> relativizeUrlsCompiler
                
    group "seperate" $
         match "posts/*.txt" $ do
            route (setExtension ".html")
            compile $ recipeCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")  
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
                >>> applyTemplateCompiler "templates/default.html"                                
                >>> relativizeUrlsCompiler
                
    -- Index
    match "index*.html" $ route $ customRoute (\i -> "pages" </> trace ("routing: " ++ show i) (toFilePath i) ) 
    metaCompile $ requireAll_ (inGroup (Just "index"))
        >>> arr (chunk articlesPerIndexPage)
        >>^ makeIndexPages
        
    -- About
    match "about.html" $ route idRoute
    create "about.html" $ constA mempty
        >>> arr (setField "title" "About")
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
        >>> applyTemplateCompiler "templates/about.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Recipes List
    match "postlist.html" $ route idRoute
    create "postlist.html" $ constA mempty
        >>> arr (setField "title" "All Posts")
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
        >>> requireAllA (inGroup (Just "seperate")) addPostList
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
        
    -- Read templates
    match "templates/*" $ compile templateCompiler
    
renderTagCloud' :: Compiler (Tags String) String
renderTagCloud' = renderTagCloud tagIdentifier 100 120

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

-- Given a list of posts, generates all post in detail and
-- add it to the current page under @$posts@
allPosts :: Compiler (Page String, [Page String]) (Page String)
allPosts = setFieldA "posts" $
        arr recentFirst
        >>> arr mconcat
        >>> arr pageBody

-- Given a list of posts, generate a post list and
-- add it to the current page under @$postlist@
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "postlist" $
        arr recentFirst
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
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

-- Split list into equal sized sublists.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs   
   
-- generate appropriate number of index pages with correct names and 
-- the appropriate posts on each one.
makeIndexPages :: [[Page String]] -> 
                  [(Identifier (Page String), Compiler () (Page String))]
makeIndexPages pageString = zipWith (curry doOne) [maxn, maxn - 1 .. 1] pageString
  where doOne (n, ps) = (indexIdentifier n, makeIndexPage n maxn ps)
        maxn = nposts `div` articlesPerIndexPage +
               if nposts `mod` articlesPerIndexPage /= 0 then 1 else 0
        nposts = sum $ map length pageString
        indexIdentifier n = parseIdentifier url
          where url = "index" ++ (if n == 1 then "" else show n) ++ ".html" 


-- Creates an index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
makeIndexPage :: Int -> Int -> [Page String] -> Compiler a (Page String)
makeIndexPage n maxn posts = 
  constA (mempty, posts)
  >>> allPosts
  >>> arr (setField "navlinkolder" (indexNavLink n 1 maxn))
  >>> arr (setField "navlinknewer" (indexNavLink n (-1) maxn))
  >>> arr (setField "title" "Home")
  >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
  >>> applyTemplateCompiler "templates/index.html"
  >>> applyTemplateCompiler "templates/default.html"
  >>> relativizeUrlsCompiler
                  
  
-- Generate navigation link HTML for stepping between index pages.
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if refPage == "" then ""
              else H.a ! A.class_ "navlink" ! A.href (toValue $ toUrl refPage) $ 
                   H.preEscapedToMarkup lab
        lab :: String
        lab = if d > 0 then "&laquo; OLDER POSTS" else "NEWER POSTS &raquo;"
        refPage = if n + d < 1 || n + d > maxn then ""
                  else case n + d of
                    1 -> "pages/index.html"
                    _ -> "pages/index" ++ show (n + d) ++ ".html" 

recipeToHtml :: Maybe Recipe -> HtmlString
recipeToHtml maybeRecipe = let recipe = fromJust maybeRecipe in renderHtml $(shamletFile "recipe.hamlet")

recipeCompiler :: Compiler Resource (Page String)
recipeCompiler = (getResourceString >>> arr createRecipe >>> arr recipeToHtml >>^ readPage) >>> addDefaultFields
              