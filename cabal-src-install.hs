import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import Control.Monad (unless, when)
import System.Directory
import Data.List (isSuffixOf, isPrefixOf)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TE
import Data.Monoid (mempty)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))

rawSystem' :: String -> [String] -> IO ()
rawSystem' a b = do
    ec <- rawSystem a b
    unless (ec == ExitSuccess) $ exitWith ec

main :: IO ()
main = do
    args <- getArgs
    unless (args == ["--src-only"]) $ rawSystem' "cabal" $ "install" : args
    putStrLn "Installing source package"
    distExists <- doesDirectoryExist "dist"
    when distExists $
        getDirectoryContents "dist" >>= mapM_ (\fp ->
            when (".tar.gz" `isSuffixOf` fp) $ removeFile $ "dist/" ++ fp)
    rawSystem' "cabal" ["sdist"]
    files <- getDirectoryContents "dist"
    case filter (".tar.gz" `isSuffixOf`) files of
        [x] -> do
            let y = drop 1 $ dropWhile (/= '.')
                  $ drop 1 $ dropWhile (/= '.')
                  $ reverse x
            let (ver', name') = break (== '-') y
            let ver = reverse ver'
            let name = reverse $ drop 1 name'
            addToDB name ver
        [] -> error "Missing tarball"
        _ -> error "Too many tarballs"

addToDB name ver = do
    cabal <- getAppUserDataDirectory "cabal"
    let pd = cabal ++ "/packages/cabal-src/"
    createDirectoryIfMissing True pd
    let tb = pd ++ "00-index.tar"
    e <- doesFileExist tb
    entries <-
        if e
            then Tar.foldEntries (:) [] error . Tar.read . L.fromChunks . return
                <$> S.readFile tb
            else return []
    cabalLBS <- L.readFile $ name ++ ".cabal"
    Right tarPath <- return $ TE.toTarPath False $ concat
        [name, "/", ver, "/", name, "-", ver, ".cabal"]
    let entry = TE.fileEntry tarPath cabalLBS
    let entries' = entry : filter (\e -> TE.entryTarPath e /= tarPath) entries
    L.writeFile tb $ Tar.write entries'

    let dir = pd ++ concat [name, "/", ver, "/"]
    createDirectoryIfMissing True dir
    let filename = concat [name, "-", ver, ".tar.gz"]
    copyFile ("dist/" ++ filename) (dir ++ filename)
    fixConfig pd $ cabal ++ "/config"

fixConfig pd fn = do
    ls' <- lines <$> readFile fn
    let oldLines =
            [ "remote-repo: cabal-src:http://www.haskell.org/"
            ]
    let s = "local-repo: " ++ pd
    let ls = filter (not . flip elem oldLines) ls'
    unless (s `elem` ls) $ writeFile fn $ unlines $ addRepo s ls

addRepo s [] = [s]
addRepo s (x:xs)
    | "remote-repo:" `isPrefixOf` x = s : x : xs
    | otherwise = x : addRepo s xs
