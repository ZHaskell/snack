



data TermCommand = TermCommand { getCmd :: String, 
                                 argNum :: Int, 
                                 arguments :: [Argument]
                                 getFunc :: -> a 
                               }


data Argument = Argument String String Value

data Value = Number 
           | List 


example = TermCommand "example" 2 [Argument "arg1" "this is the first argument of the example" Number,
                                   Argument "arg2" "this is the second argument of the example" List ]



commands = map getCmd

completeExample :: MonadIO m => String -> m [Completion]
completeExample str = return . map simpleCompletion . filter (str `isPrefixOf`) $ commands


data Completion = Completion {replacement  :: String, -- ^ Text to insert in line.
                        display  :: String,
                                -- ^ Text to display when listing
                                -- alternatives.
                        isFinished :: Bool
                            -- ^ Whether this word should be followed by a
                            -- space, end quote, etc.
                            }
                    deriving (Eq, Ord, Show)