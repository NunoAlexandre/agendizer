import Control.Arrow
import System.Environment
import Data.List
import Data.Ord
import Data.Function

-- Model
type Name = [String]
type Number = String
data Contact = Contact {name::Name, number::Number } deriving (Show, Ord, Eq, Read)

main = do
    [agendaX, agendaY, finalAgenda] <- getArgs
    mergeAgendas agendaX agendaY finalAgenda

mergeAgendas :: FilePath -> FilePath -> FilePath -> IO ()
mergeAgendas x y final = allEntries >>= writeFile final . toEntries . mergeContacts . groupContacts
        where allEntries = (++) <$> (readFile x) <*> (readFile y)

toEntries :: [Contact] -> String
toEntries = concatMap (\(Contact name number) -> ((unwords name) ++ " " ++ number ++"\n"))

groupContacts :: String -> [[Contact]]
groupContacts =  groupBy (on (==) name) . sortBy (comparing name) . map toContact . lines

toContact :: String -> Contact
toContact xs = uncurry Contact $ (init &&& last) . words $ xs

mergeContacts :: [[Contact]] -> [Contact]
mergeContacts = map mergeContact

mergeContact :: [Contact] -> Contact
mergeContact [x] = x
mergeContact [(Contact name numX),(Contact _ numY)] = Contact (name) (chooseNumber name numX numY)

chooseNumber :: Name -> Number -> Number -> Number
chooseNumber _ = min
