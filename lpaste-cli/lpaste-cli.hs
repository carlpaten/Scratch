import Data.Monoid		(mempty)
import Data.Maybe		(fromMaybe)
import Control.Lens		((&), (<&>))
import Control.Arrow		((>>>))
import Network.Browser
import Network.HTTP.Base	(Response, RequestMethod (POST))
import Network.URI		(URI, parseURI)
import System.Directory		(doesFileExist)
import System.Environment	(getEnv)
import Options.Applicative

argsToFilename :: Parser String
argsToFilename = argument str (metavar "FILENAME")

lpasteURI :: URI
lpasteURI = parseURI "http://www.lpaste.net/new" & fromMaybe (error "Broken URI")

fileContentsToForm :: String -> Form
fileContentsToForm contents = 
    Form POST (lpasteURI) 
	[
	    ("paste", contents),
	    ("public", "public"),
	    ("title", "Automated Haskell paste"),
	    ("author", "Anonymous Cowerd"),
	    ("email", ""),
	    ("language", "haskell"),
	    ("channel", "")
	]

postForm :: Form -> IO (URI, Response String)
postForm form = do
    (url, response) <- Network.Browser.browse $ do
	setOutHandler $ const (return ())
	setAllowRedirects True
	request $ formToRequest form
    return (url, response)
    
main :: IO ()
main = do
    filename <- execParser $ info argsToFilename mempty
    form <- readFile filename <&> fileContentsToForm
    postForm form >>= print
