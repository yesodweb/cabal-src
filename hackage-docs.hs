{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Package
import ClassyPrelude.Conduit
import Distribution.Text (display)
import Data.Conduit.Process
import System.IO.Temp
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS
import Filesystem (isDirectory)

main :: IO ()
main = withManager tlsManagerSettings $ \man -> do
    rawCreds <- readFile "/hackage-creds" `catchIO` \_ ->
        error "This tool requires a file /hackage-creds to be present and readable,\ncontaining your username and password for Hackage"
    (username, password) <-
        case words $ decodeUtf8 rawCreds of
            [x, y] -> return (encodeUtf8 x, encodeUtf8 y)
            _ -> error "/hackage-creds didn't look like how I wanted"

    whenM (isDirectory "tarballs") $ runResourceT
        $ sourceDirectory "tarballs"
       $$ mapM_C (liftIO . uploadTarball username password man)

    srcsRaw <- readFile "sources.txt" `catchIO` \_ -> return "."
    forM_ (lines $ decodeUtf8 srcsRaw) (processDir username password man . fpFromText)

uploadTarball :: ByteString -> ByteString -> Manager -> FilePath -> IO ()
uploadTarball username password man fp = do
    let formData =
            [ partFile "package" $ fpToString fp
            ]
    req1 <- formDataBody formData "http://hackage.haskell.org/packages/"
    let req2 = applyBasicAuth username password req1
    tryAny (httpLbs req2 man) >>= print

terror :: Text -> a
terror = error . unpack

processDir :: ByteString -> ByteString -> Manager -> FilePath -> IO ()
processDir username password man dir = do
    putStrLn $ "Processing " ++ fpToText dir
    cabals <- runResourceT $ sourceDirectory dir $$ foldMapC
        (\fp -> if hasExtension fp "cabal"
                    then asSet $ singletonSet fp
                    else mempty)
    cabal <- case setToList cabals of
        [] -> terror $ "No cabal files found in " ++ fpToText dir
        [x] -> return x
        xs -> terror $ "Multiple cabal files found in " ++ fpToText dir ++ ": " ++ tshow xs
    gpd <- readPackageDescription normal $ fpToString cabal
    case condLibrary gpd of
        Nothing -> putStrLn $ concat
            [ "No library in "
            , fpToText cabal
            , ", skipping"
            ]
        Just _ -> do
            let pd = packageDescription gpd
                PackageIdentifier name' version' = package pd
                name = pack $ display name'
                version = pack $ display version'
            runIn dir "cabal"
                [ "haddock"
                , "--hoogle"
                , "--hyperlink-source"
                , "--html-location=/package/$pkg-$version/docs"
                , "--contents-location=/package/$pkg-$version"
                ]
            withSystemTempDirectory "build-docs.XXXXXX" $ \tmp -> do
                let docsdir = concat
                        [ pack tmp
                        , "/"
                        , name
                        , "-"
                        , version
                        , "-docs"
                        ]
                    tarball = docsdir ++ ".tar.gz"
                runIn dir "cp"
                    [ "-R"
                    , "dist/doc/html/" ++ name
                    , docsdir
                    ]
                runIn dir "tar"
                    [ "cvz"
                    , "-C"
                    , pack tmp
                    , "--format=ustar"
                    , "-f"
                    , tarball
                    , concat
                        [ name
                        , "-"
                        , version
                        , "-docs"
                        ]
                    ]
                let url = unpack $ concat
                        [ "https://hackage.haskell.org/package/"
                        , name
                        , "-"
                        , version
                        , "/docs"
                        ]
                bs <- readFile $ fpFromText tarball
                let req = applyBasicAuth username password $ (fromString url)
                        { method = "PUT"
                        , requestHeaders =
                            [ ("Content-Type", "application/x-tar")
                            , ("Content-Encoding", "gzip")
                            ]
                        , requestBody = RequestBodyBS bs
                        }
                httpLbs req man >>= print

runIn :: FilePath -> Text -> [Text] -> IO ()
runIn dir cmd args = do
    print (dir, cmd, args)
    withCheckedProcess cp $ \ClosedStream Inherited Inherited -> return ()
  where
    cp = (proc (unpack cmd) (map unpack args)) { cwd = Just $ fpToString dir }
