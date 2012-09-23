import System.Exit
import System.FilePath
import System.Directory
import System.Process
import Control.Monad
import qualified Data.Monoid as M

import Language.MessagePack.IDL.Internal

generate :: FilePath -> IO ExitCode
generate idl = rawSystem "mpidl" ["ruby", "-o", "/tmp/", idl] 

instance M.Monoid ExitCode where
         mempty = ExitSuccess
         mappend e@(ExitFailure n) _ = e
         mappend _ x = x

isIDLFile :: FilePath -> Bool
isIDLFile = (== ".idl") . takeExtension

main :: IO ExitCode
main = withDirectory "./test/" $ do
  setCurrentDirectory "idls/"
  idls <- fmap (filter isIDLFile) $ getDirectoryContents "." 
  exitcodes <- mapM generate idls
  print $ zip idls exitcodes
  exitWith $ M.mconcat exitcodes
