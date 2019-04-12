--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Debug.Trace

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "blocks/sites/*.md" $ do
      route $
        composeRoutes
          (gsubRoute "blocks/sites/" (const ""))
          (setExtension "html")
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        relativizeUrls
    match "blocks/**.md" $ compile pandocCompiler

    create ["about.html"] $ do
      route idRoute
      compile $ do
        about <- thesesCompiler "blocks/about/*.md"
        let aboutCtx =
              constField "title" "Über uns" `mappend` constField "about" about `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/about.html" aboutCtx >>=
          loadAndApplyTemplate "templates/default.html" aboutCtx >>=
          relativizeUrls
    create ["mitmachen.html"] $ do
      route idRoute
      compile $ do
        articles <- thesesCompiler "blocks/mitmachen/*.md"
        let ctx =
              constField "title" "Mitmachen" `mappend` constField "articles" articles `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/mitmachen.html" ctx >>=
          loadAndApplyTemplate "templates/default.html" ctx >>=
          relativizeUrls
    create ["index.html"] $ do
      route idRoute
      compile $ do
        theses <- thesesCompiler "blocks/theses/*.md"
        let indexCtx =
              constField "title" "Home" `mappend` constField "theses" theses `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/index.html" indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
thesesCompiler :: Pattern -> Compiler String
thesesCompiler p = do
  theses <- loadAll p
  thesisTemplate <- loadBody "templates/thesis.html"
  applyTemplateList thesisTemplate defaultContext theses

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
