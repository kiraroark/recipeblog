{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Text.Hamlet (shamlet,shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)
import System.IO
import Text.ParserCombinators.Parsec
import Data.String.Utils
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import System.Environment
import Debug.Trace

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
    group "index" $ do
        match "posts/*.txt" $ do
            compile $ recipeCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")      
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> relativizeUrlsCompiler
                
    group "seperate" $ do
         match "posts/*.txt"  $ do
            route $ (setExtension ".html")
            compile $ recipeCompiler
                >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")  
                >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>> applyTemplateCompiler "templates/post.html"
                >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
                >>> applyTemplateCompiler "templates/default.html"                                
                >>> relativizeUrlsCompiler
                
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
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

-- Split list into equal sized sublists.
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs   
   
-- generate appropriate number of index pages with correct names and 
-- the appropriate posts on each one.
makeIndexPages :: [[Page String]] -> 
                  [(Identifier (Page String), Compiler () (Page String))]
makeIndexPages ps = map doOne (zip [maxn,maxn-1..1] ps)
  where doOne (n, ps) = (indexIdentifier n, makeIndexPage n maxn ps)
        maxn = nposts `div` articlesPerIndexPage +
               if (nposts `mod` articlesPerIndexPage /= 0) then 1 else 0
        nposts = sum $ map length ps
        indexIdentifier n = parseIdentifier url
          where url = "index" ++ (if (n == 1) then "" else show n) ++ ".html" 


-- Creates an index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
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
                  
  
-- Generate navigation link HTML for stepping between index pages.
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.class_ "navlink" ! A.href (toValue $ toUrl $ refPage) $ 
                   (H.preEscapedToMarkup lab)
        lab :: String
        lab = if (d > 0) then "&laquo; OLDER POSTS" else "NEWER POSTS &raquo;"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "pages/index.html"
                    _ -> "pages/index" ++ (show $ n + d) ++ ".html" 

data Recipe = Recipe
    { metadata :: String
	, summary :: String
	, pictures :: [String]
	, prep    :: String
	, cook    :: String
	, total   :: String
	, ingredientBlock  :: [SubSection]
	, instructionBlock :: [SubSection]
    } deriving Show

data SubSection = SubSection 
	{ name	    :: String
	, content	:: [String]
	} deriving Show

type PureString = String

type HtmlString = String
                                
createRecipe :: PureString -> Maybe Recipe
createRecipe fileStr = case parse parseRecipe " " fileStr of
                            Left _ -> Nothing
                            Right recipe -> Just recipe

recipeToHtml :: Maybe Recipe -> HtmlString
recipeToHtml maybeRecipe = let recipe = fromJust maybeRecipe in renderHtml $(shamletFile "recipe.hamlet")

-- createRecipeArrow :: Compiler PureString (Maybe Recipe)
-- createRecipeArrow = arr createRecipe
-- 
-- recipeToHtmlArrow :: Compiler (Maybe Recipe) HtmlString
-- recipeToHtmlArrow = arr recipeToHtml

recipeCompiler :: Compiler Resource (Page String)
recipeCompiler = (getResourceString >>> arr createRecipe >>> arr recipeToHtml >>^ readPage) >>> addDefaultFields

createSections :: String -> Maybe [SubSection]
createSections inputStr = case parse parseSections " " inputStr of
								 Left err ->  trace (show err) $ Nothing
								 Right subSections -> Just subSections

parseSections :: Parser [SubSection]
parseSections = many1 $ do 
				  string "#"
				  name <- manyTill anyChar (try (newline))
				  content <- manyTill anyChar (try (string "@"))
				  newline
				  return (SubSection name (lines content))

parseRecipe :: Parser Recipe
parseRecipe = do
				 metadata <- manyTill anyChar (try (string "*Summary*"))
				 summary <- manyTill anyChar (try (string "*Pictures*"))
				 pictures <- manyTill anyChar (try (string "*Prep*"))
				 prep <- manyTill anyChar (try (string "*Cook*"))
				 cook <- manyTill anyChar (try (string "*Total*"))
				 total <- manyTill anyChar (try (string "*Ingredients*"))
				 newline
				 ingredientBlock <- manyTill anyChar (try (string "*Instructions*"))
				 newline
				 instructionBlock <- manyTill anyChar (try eof)
				 return (Recipe metadata summary (lines pictures) prep cook total (fromMaybe [] (createSections ingredientBlock)) (fromMaybe [] (createSections instructionBlock)))
                 