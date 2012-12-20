{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), (&&&), arr, (>>^))
import Control.Category (id)
import Data.Monoid (mempty, mconcat, mappend)

import Hakyll

import Data.Maybe (isNothing)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath ((</>))
import Debug.Trace

-- | Number of article teasers displayed per sorted index page.
--
articlesPerIndexPage :: Int
articlesPerIndexPage = 2

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
    match "index*.html" $ route $ customRoute (\i -> "pages" </> trace ("routing: " ++ (show i)) (toFilePath i) ) 
    metaCompile $ requireAll_ (inGroup (Just "index"))
		>>> arr (chunk articlesPerIndexPage)
        >>^ (makeIndexPages)
		 
        
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
        >>> requireAllA (inGroup (Just "seperate")) (addPostList)
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

--  | Auxiliary compiler: generate all post in detail from a list of given posts, and
-- add it to the current page under @$posts@
allPosts :: Compiler (Page String, [Page String]) (Page String)
allPosts = setFieldA "posts" $
		arr recentFirst
       -- >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$postlist@
--
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
	
-- | Split list into equal sized sublists.
--
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs   
   
-- | Helper function for index page metacompilation: generate
-- appropriate number of index pages with correct names and the
-- appropriate posts on each one.
--
makeIndexPages :: [[Page String]] -> 
                  [(Identifier (Page String), Compiler () (Page String))]
makeIndexPages ps = map doOne (zip [maxn,maxn-1..1] ps)
  where doOne (n, ps) = (indexIdentifier n, makeIndexPage n maxn ps)
        maxn = nposts `div` articlesPerIndexPage +
               if (nposts `mod` articlesPerIndexPage /= 0) then 1 else 0
        nposts = sum $ map length ps
        indexIdentifier n = parseIdentifier url
          where url = "index" ++ (if (n == 1) then "" else show n) ++ ".html" 


-- | Make a single index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
--
-- makeIndexPage :: Int -> Int -> [Page String] -> Compiler () (Page String)
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
                  
  
-- | Generate navigation link HTML for stepping between index pages.
--
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $ 
                   (H.preEscapedToMarkup lab)
        lab :: String
        lab = if (d > 0) then "&laquo; OLDER POSTS" else "NEWER POSTS &raquo;"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "pages/index.html"
                    _ -> "pages/index" ++ (show $ n + d) ++ ".html" 