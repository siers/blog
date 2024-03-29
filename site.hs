{-# LANGUAGE OverloadedStrings #-}

import System.Process
import Hakyll

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

monodateCtx :: Context String
monodateCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

matchMany routes = flip mapM routes . flip match

renderPost :: Rules ()
renderPost = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

main' = hakyll $ do
    matchMany ["images/*", "js/*", "favicon.ico", "source.tar.bz2"] $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    flip matchMany renderPost
        [ "code.md"
        , "portfolio.md"
        , "portfolio-js.md"
        , "portfolio-data.md"
        , "portfolio-intel.md"
        , "toys.md"
        , "about.md"
        , "posts/*"]

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (monodateCtx) (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx

    match "templates/*" $ compile templateCompiler

sourcify = system $
    "ls; it ls-files | " ++
    "xargs tar -c --transform 's:^:source/:' .git | " ++
    "bzip2 -9 - > source.tar.bz2"

main :: IO ()
-- main = sourcify >> main'
main = main'
