{-# LANGUAGE OverloadedStrings #-}

import System.Process
import Data.Monoid (mappend)
import Hakyll

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

monodateCtx :: Context String
monodateCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

main' = hakyll $ do
    (`mapM` ["images/*", "source.tar.bz2"]) . (flip match) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    (`mapM` ["about.md", "posts/*"]) . (flip match) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

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

sourcify = system "git ls-files | xargs tar c .git | bzip2 -9 - > source.tar.bz2"

main :: IO ()
main = sourcify >> main'
