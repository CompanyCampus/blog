--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Function (on)
import           Data.List (intersperse, sortBy)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/sidebar.html" (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/post.html"
                        (constField "title" title `mappend`
                            constField "body" "" `mappend`
                            constField "date" "" `mappend`
                            constField "prettytags" "" `mappend`
                            constField "posts" "" `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title `mappend`
                            defaultContext)
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList $ fmap (take 3) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    recentPostsCtx `mappend`
    defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    postCtx

recentPostsCtx :: Context String
recentPostsCtx =
    field "posts" $ \_ ->
            depFreePostList $ fmap (take 5) . recentFirst'
  where
    recentFirst' = return . reverse . sortBy (compare `on` (show . fst))


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list


depFreePostList :: ([(Identifier, Metadata)] -> Compiler [(Identifier, Metadata)]) -> Compiler String
depFreePostList sortFilter = do
    postsMetadata <- sortFilter =<< getAllMetadata "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateListWithContexts itemTpl (map mkPair postsMetadata)
  where
    mkPair (i, m) = (makeDefaultContext (i, m), Item i "")

applyJoinTemplateListWithContexts :: String
                                  -> Template
                                  -> [(Context a, Item a)]
                                  -> Compiler String
applyJoinTemplateListWithContexts delimiter tpl pairs = do
    items <- mapM (\p -> applyTemplate tpl (fst p) (snd p)) pairs
    return $ concat $ intersperse delimiter $ map itemBody items

applyTemplateListWithContexts = applyJoinTemplateListWithContexts ""


makeDefaultContext :: (Identifier, Metadata) -> Context String
makeDefaultContext (i, m) =
        makeUrlField i `mappend`
        dateField "date" "%B %e, %Y" `mappend`
        makeMetadataContext m
    where
        makeMetadataContext m =
            (Context $ \k _ -> do
                return $ fromMaybe "" $ M.lookup k m)

        makeUrlField id =
            field "url" $ \_ -> do
                fp <- getRoute id
                return $ fromMaybe "" $ fmap toUrl fp
