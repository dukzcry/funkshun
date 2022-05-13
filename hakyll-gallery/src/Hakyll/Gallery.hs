{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Gallery (
makeGalleryCtx,galleryRuleset
,GallerySettings,GalleryImage,defaultGallerySettings,matchExtensions
) where

import Hakyll
import Algorithms.NaturalSort
import System.FilePath
import Control.Applicative
import Data.List
import Hakyll.Images (loadImage, ensureFitCompiler, compressJpgCompiler)

data GalleryImage = GalleryImage {
  x :: Int,
  y :: Int,
  compress :: Bool,
  ratio :: Int,
  exts :: Pattern
}
defaultGalleryImage = GalleryImage {
  compress = False, x = 0, y = 0, ratio = 0, exts = matchExtensions [] ""
}
data GallerySettings = GallerySettings {
  title :: String,
  folder :: String,
  compressImages :: GalleryImage,
  imageThumbs :: GalleryImage,
  videoThumbs :: GalleryImage,
  filesExts :: Pattern,
  naturalSort :: Bool,
  previewNum :: Int
}
defaultGallerySettings = GallerySettings {
  title = "Галерея",
  folder = folder,
  compressImages = defaultGalleryImage {
    x = 1280, y = 1024, ratio = 90, exts = galleryLossyImages folder
  },
  imageThumbs = defaultGalleryImage {
    x = 300, y = 128, exts = galleryImages folder
  },
  videoThumbs = defaultGalleryImage {
    x = 128, y = 128, exts = galleryVideos folder
  },
  filesExts = galleryFiles folder,
  naturalSort = True,
  previewNum = 5
} where
  folder = "gallery"
  galleryLossyImages = matchExtensions ["jpg", "jpeg"]
  galleryLosslessImages = matchExtensions ["png"]
  galleryImages folder = galleryLossyImages folder .||. galleryLosslessImages folder
  galleryVideos = matchExtensions ["mp4"]
  galleryFiles folder = galleryImages folder .||. galleryVideos folder


galleryRuleset siteCtx postCtx settings = do
    let folder' = folder settings
    let image = compressImages settings
    let thumb = imageThumbs settings
    if (compress image) then
        match (exts image) $ do
          route   idRoute
          compile $ loadImage
            >>= ensureFitCompiler (x image) (y image)
            >>= compressJpgCompiler (ratio image)
    else pure ()

    match (filesExts settings) $ do
        route   idRoute
        compile copyFileCompiler

    match (exts thumb) $ version "thumbnail" $ do
      route . customRoute $ (\x -> replaceExtension x (".thumb" ++ takeExtension x)) . toFilePath
      compile $ loadImage
        >>= ensureFitCompiler (x thumb) (y thumb)

    -- photo descriptions
    match (fromGlob $ folder' ++ "/*/*.md") $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ pandocCompiler
            >>= relativizeUrls

    -- actually when some page is modified we only need to update adjacent pages so they link to our page, but due
    -- hakyll limitation we rebuild all gallery pages :(
    galleryDependencies <- makePatternDependency $ filesExts settings

    rulesExtraDependencies [galleryDependencies] $ do
      match (fromGlob $ folder' ++ "/index.html") $ do
        route $ setExtension "html"
        compile $ do
          -- here we get listField for all gallery items
          ctx <- makeGalleryCtx settings
          getResourceString
            >>= renderPandoc
            -- apply twice first $gallery()$ into $for()$ template then $for()$ into html code
            >>= applyAsTemplate ctx
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" (constField "title" (title settings) `mappend` siteCtx)
            >>= relativizeUrls

    rulesExtraDependencies [galleryDependencies] $ do
      match (filesExts settings) $ version "page" $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ do
          -- here we find metadata for our item and its adjacent pages and put it into context
          path <- toFilePath <$> getUnderlying
          galleryUnboxed <- gallery settings
          let [(_,ctx)] = filter (\ (x,_) -> x `equalFilePath` takeDirectory path) galleryUnboxed
          let [item] = filter (\ x -> elem' x `equalFilePath` path) ctx
          let prevElm = maybe missingField (\ x -> ctxMaker "prev" (\ _ -> x)) $ prev' item
          let nextElm = maybe missingField (\ x -> ctxMaker "next" (\ _ -> x)) $ next' item
          let ctx' = ctxMaker "" (\ _ -> item) `mappend`
                prevElm `mappend`
                nextElm `mappend`
                constField "baseurl" ("/" ++ folder') `mappend`
                -- here we get description for gallery item
                (field "body" . return . loadBody . fromFilePath $ path <.> "md") `mappend`
                siteCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (ctx' `mappend` postCtx)
            >>= relativizeUrls

matchExtensions exts folder =
  let
    exts' = map (fromGlob . ((++) $ folder ++ "/*/*.")) exts
  in
    foldl1 (.||.) exts'

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

-- create metadata list for futher search: image or video, thumbnail, previous and next item
gallery settings = do
  let folder' = folder settings
  let thumb = videoThumbs settings
  recursiveContents <- unsafeCompiler $ getRecursiveContents (\_ -> return False) folder'
  let contents = map (folder' </>) recursiveContents

  contentsTime <- sequence $ map (getItemModificationTime . fromFilePath) contents
  let contentsWithTime = zip contentsTime contents
  let timeSortedList = sortBy (\ x y -> Prelude.compare (fst x) (fst y)) contentsWithTime
  let (_,timeSortedContents) = unzip timeSortedList

  let natSortedContents = sortBy (\ x y -> Algorithms.NaturalSort.compare x y) contents

  let sortedContents = if (naturalSort settings) then natSortedContents else timeSortedContents

  let filteredContents = filter (matches (filesExts settings) . fromFilePath) sortedContents
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
                                                  , video = matches (exts thumb) $ fromFilePath e
                                                  , previousPageNum = p /= Empty
                                                  , nextPageNum = n /= Empty
                                                  }
      in itemMaker e p n prevElm nextElm
  return $ map (\ (dir,l) -> (dir, toList $ mapDList ctxMaker l)) linkedContents

-- build actual context with all needed fields
ctxMaker prefix f =
  field (prefix ++ "url") (url . f) `mappend`
  field (prefix ++ "page") (page . f) `mappend`
  field (prefix ++ "thumbnail") (thumbnail . f) `mappend`
  boolField (prefix ++ "video") (video . f) `mappend`
  if prefix == "" then missingField else boolField "previousPageNum" (previousPageNum . f) `mappend`
  if prefix == "" then missingField else boolField "nextPageNum" (nextPageNum . f)

-- here we build listField for all gallery items and also teaser variant where are only first previewNum items available
makeGalleryCtx settings = do
  let thumb = videoThumbs settings
  galleryUnboxed <- gallery settings
  let listfieldMaker (folder,items) =
        let items' = map makeItem items
        -- variable names are fragile
        in listField (takeFileName folder) (ctxMaker "" itemBody) (sequence items') `mappend`
           listField (takeFileName folder ++ "preview") (ctxMaker "" itemBody) (sequence $ take (previewNum settings) items')
  let ctx = map listfieldMaker galleryUnboxed
  return $ foldl1 mappend ctx `mappend` (galleryField thumb)

-- this is template used for gallery
galleryField thumb = functionField "gallery" $ \[args] _ ->
  return $ unlines [
    "$for(" ++ args ++ ")$",
    "$if(video)$",
    "<a href=\"$page$\"><video width=\"" ++ show (x thumb) ++ "\" height=\"" ++ show (y thumb) ++ "\" preload=\"metadata\"><source src=\"$url$\"></video></a>",
    "$else$",
    "<a href=\"$page$\"><img src=\"$thumbnail$\"/></a>",
    "<link rel=\"prefetch\" href=\"$url$\">",
    "$endif$",
    "$endfor$"
  ]
