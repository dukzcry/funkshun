#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.network-uri pkgs.haskeline pkgs.scrypt pkgs.cryptohash-sha256 ])"

-- Written by Artem Falcon <lomka@gero.in>

import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad

import Network.URI
import System.Console.Haskeline
import Crypto.Scrypt
import Crypto.Hash.SHA256

af l n = l !! (n `mod` length l)

vow = "aeiou";
con = ['a'..'z'] \\ vow
ccon = map toUpper con
num = ['0'..'9']
pun = "@&%?,=[]_:-+*$#!^~;()/."

long = map (take 14 . concat) $ take 21 $ permutations $ [af num]:[af pun]:permutations [af ccon,af vow,af con]
short = [[af ccon,af vow,af con,af num]]

types n = [long,short] !! read n

fromByteString = B.foldr (\ b l -> ord b:l) []
finalPass a b = getZipList $ ZipList a <*> ZipList b

getAuth url = do
  uri <- parseURI url
  auth <- uriAuthority uri
  Just (uriRegName auth)
getSite site =
  case getAuth site of
    Nothing -> site
    Just domain -> domain
checkSite site_ = runInputT defaultSettings loop
  where
    site = getSite site_
    loop = getInputLineWithInitial "site " ("", site)
encodeSite key site counter =
  hmac key (B.pack $ show (length site) ++ site ++ counter)

checkPass = do
    pass1 <- checkPass_
    pass2 <- checkPass_
    guard (pass1 == pass2)
    return pass1
  where
    checkPass_ = runInputT defaultSettings loop
    loop = getPassword (Just '*') "pass "
encodePass name pass =
  getHash $ scrypt (fromJust $ scryptParams 16 8 1)
    (Salt $ B.pack $ show (length name) ++ name) (Pass $ B.pack pass)

main = do
  [ptype,counter,name,site_] <- getArgs
  site <- checkSite site_
  pass <- checkPass
  let key = encodePass <$> pure name <*> pass
  let secret = encodeSite <$> key <*> site <*> pure counter
  let template = af (types ptype) <$> ord . B.head <$> secret
  let secretList = fromByteString <$> B.tail <$> secret
  print $ finalPass <$> template <*> secretList
