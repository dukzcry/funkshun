{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Gallery (
makeGalleryCtx
,galleryRuleset
) where

import Hakyll
import Algorithms.NaturalSort
import System.FilePath
import Control.Applicative
import Data.List
import Hakyll.Images (loadImage, ensureFitCompiler, compressJpgCompiler)

galleryRuleset siteCtx postCtx compress = do
    if compress then
        match galleryLossyImages $ do
          route   idRoute
          compile $ loadImage
            >>= ensureFitCompiler 1280 1024
            >>= compressJpgCompiler 90
    else pure ()

    match galleryFiles $ do
        route   idRoute
        compile copyFileCompiler

    match galleryImages $ version "thumbnail" $ do
      route . customRoute $ (\x -> replaceExtension x (".thumb" ++ takeExtension x)) . toFilePath
      compile $ loadImage
        >>= ensureFitCompiler 300 128

    match (fromGlob $ folder ++ "/*/*.md") $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ pandocCompiler
            >>= relativizeUrls

    galleryDependencies <- makePatternDependency galleryFiles

    rulesExtraDependencies [galleryDependencies] $ do
      match (fromGlob $ folder ++ "/index.html") $ do
        route $ setExtension "html"
        compile $ do
          ctx <- makeGalleryCtx
          getResourceString
            >>= renderPandoc
            >>= applyAsTemplate ctx
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Галерея" `mappend` siteCtx)
            >>= relativizeUrls

    rulesExtraDependencies [galleryDependencies] $ do
      match galleryFiles $ version "page" $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ do
          path <- toFilePath <$> getUnderlying
          galleryUnboxed <- gallery
          let [(_,ctx)] = filter (\ (x,_) -> x `equalFilePath` takeDirectory path) galleryUnboxed
          let [item] = filter (\ x -> elem' x `equalFilePath` path) ctx
          let prevElm = maybe missingField (\ x -> ctxMaker "prev" (\ _ -> x)) $ prev' item
          let nextElm = maybe missingField (\ x -> ctxMaker "next" (\ _ -> x)) $ next' item
          let ctx' = ctxMaker "" (\ _ -> item) `mappend`
                prevElm `mappend`
                nextElm `mappend`
                constField "baseurl" ("/" ++ folder) `mappend`
                (field "body" . return . loadBody . fromFilePath $ path <.> "md") `mappend`
                siteCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (ctx' `mappend` postCtx)
            >>= relativizeUrls

folder = "gallery"

galleryLossyImages =
  fromGlob (folder ++ "/*/*.jpg")
  .||. fromGlob (folder ++ "/*/*.jpeg")

galleryLosslessImages = fromGlob (folder ++ "/*/*.png")

galleryImages = galleryLossyImages .||. galleryLosslessImages

galleryVideos =
  fromGlob (folder ++ "/*/*.mp4")

galleryFiles = galleryImages .||. galleryVideos

data DList a = Empty | Cell { elem :: a, prev :: DList a, next :: DList a } deriving (Eq)

data GalleryItem a = GalleryItem { elem' :: String
                               , prev' :: Maybe (GalleryItem a)
                               , next' :: Maybe (GalleryItem a)
                               , url :: Compiler String
                               , page :: Compiler String
                               , thumbnail :: Compiler String
                               , video :: Bool
                               , previousPageNum :: Bool
                               , nextPageNum :: Bool
                               }

fromList :: [a] -> DList a

fromList = go Empty
    where go :: DList a -> [a] -> DList a
          go prev [] = Empty
          go prev (a:as) = head
              where head = Cell a prev tail
                    tail = go head as
toList :: DList a -> [a]

toList Empty = []

toList (Cell a _ next) = a : toList next

mapDList f = go
    where go Empty = Empty
          go item@Cell { Hakyll.Gallery.elem = e, prev = p, next = n } = Cell { Hakyll.Gallery.elem = f item, prev = go p, next = go n }

gallery = do
  recursiveContents <- unsafeCompiler $ getRecursiveContents (\_ -> return False) folder
  let contents = map (folder </>) recursiveContents

  -- last time modified
  contentsTime <- sequence $ map (getItemModificationTime . fromFilePath) contents
  let contentsWithTime = zip contentsTime contents
  let timeSortedList = sortBy (\ x y -> Prelude.compare (fst x) (fst y)) contentsWithTime
  --let (_,sortedContents) = unzip timeSortedList

  -- natural sorted
  let sortedContents = sortBy (\ x y -> Algorithms.NaturalSort.compare x y) contents

  let filteredContents = filter (matches galleryFiles . fromFilePath) sortedContents
  let groupContents = groupBy (\ x y -> takeDirectory x == takeDirectory y) filteredContents
  let linkedContents = map (\ l -> (takeDirectory $ head l, Hakyll.Gallery.fromList l)) groupContents
  let
    ctxMaker Cell { Hakyll.Gallery.elem = e, prev = p, next = n } =
      let versionUrl version path = fmap (maybe empty toUrl) . getRoute . setVersion version $ fromFilePath path
          prevElm = if p == Empty then empty else pure $ itemMaker (Hakyll.Gallery.elem p) p n empty empty
          nextElm = if n == Empty then empty else pure $ itemMaker (Hakyll.Gallery.elem n) p n empty empty
          itemMaker e p n prevElm nextElm = GalleryItem { elem' = e
                                                  , prev' = prevElm
                                                  , next' = nextElm
                                                  , url = versionUrl Nothing e
                                                  , page = versionUrl (Just "page") e
                                                  , thumbnail = versionUrl (Just "thumbnail") e
                                                  , video = matches galleryVideos $ fromFilePath e
                                                  , previousPageNum = p /= Empty
                                                  , nextPageNum = n /= Empty
                                                  }
      in itemMaker e p n prevElm nextElm
  return $ map (\ (dir,l) -> (dir, toList $ mapDList ctxMaker l)) linkedContents

ctxMaker prefix f =
  field (prefix ++ "url") (url . f) `mappend`
  field (prefix ++ "page") (page . f) `mappend`
  field (prefix ++ "thumbnail") (thumbnail . f) `mappend`
  boolField (prefix ++ "video") (video . f) `mappend`
  if prefix == "" then missingField else boolField "previousPageNum" (previousPageNum . f) `mappend`
  if prefix == "" then missingField else boolField "nextPageNum" (nextPageNum . f)

makeGalleryCtx = do
  galleryUnboxed <- gallery
  let listfieldMaker (folder,items) =
        let items' = map makeItem items
        -- variable names are fragile
        in listField (takeFileName folder) (ctxMaker "" itemBody) (sequence items') `mappend`
           listField (takeFileName folder ++ "preview") (ctxMaker "" itemBody) (sequence $ take 5 items')
  let ctx = map listfieldMaker galleryUnboxed
  return $ foldl1 mappend ctx `mappend` galleryField

galleryField = functionField "gallery" $ \[args] _ ->
  return $ unlines [
    "$for(" ++ args ++ ")$",
    "$if(video)$",
    "<a href=\"$page$\"><video width=\"128\" height=\"128\" preload=\"metadata\"><source src=\"$url$\"></video></a>",
    "$else$",
    "<a href=\"$page$\"><img src=\"$thumbnail$\"/></a>",
    "<link rel=\"prefetch\" href=\"$url$\">",
    "$endif$",
    "$endfor$"
  ]
