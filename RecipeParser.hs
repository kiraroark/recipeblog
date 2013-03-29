module RecipeParser where
    
import Text.ParserCombinators.Parsec
import Data.Maybe
import Debug.Trace
    
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