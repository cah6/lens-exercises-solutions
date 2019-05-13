{-# LANGUAGE QuasiQuotes #-}
module Traversals where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import Control.Lens
import Data.Aeson.Types
import Data.Aeson.Lens
import Data.Aeson.QQ
import Data.IORef
import Data.Monoid

users :: Value
users = [aesonQQ|
  {
    "users": [
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com",
        "metadata": {
          "num_logins": 5
        }
      },
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27,
          "associated_ips": [
            "52.49.1.233",
            "52.49.1.234"
          ]
        }
      },
      {
        "name": "su.mucheng",
        "email": "smucheng@xingxin.com",
        "metadata": {
          "associated_ips": [
            "51.2.244.193"
          ]
        }
      }
    ]
  }
|]

-- Part 2 of Traversals / Folds
f1 = do ref <- newIORef 0
        users &
          traverseOf
            (key "users"
              ._Array
              .traversed
              .key "metadata"
              .key "num_logins"
              ._Integer)
            (\x -> modifyIORef' ref (+x) *> readIORef ref)

f2 = users &
      traverseOf
        (key "users".values.key "email"._String)
        (\x -> Text.putStrLn x *> pure (Text.reverse x))

getAliasMay :: Text.Text -> Maybe Text.Text
getAliasMay "ye.xiu" = Just "ye.qiu"
getAliasMay x        = Nothing

-- Part 3 of Traversals / Folds
userNamesWithU :: [Text.Text]
userNamesWithU = users & 
  foldMapOf 
    (key "users" . _Array . folded . key "name" . _String . filtered (Text.isInfixOf "u")) 
    pure

hasSpecificIp :: Bool
hasSpecificIp = getAny $ users & 
  foldMapOf
    (key "users" . _Array . folded . key "metadata" . key "associated_ips" . values . _String)
    (\x -> Any $ x == "51.2.244.193")

printAllIps :: IO ()
printAllIps = users & 
  traverseOf_
    (key "users" . _Array . folded . key "metadata" . key "associated_ips" . values . _String)
    (\x -> Text.putStrLn x)

sumLogins :: Integer
sumLogins = getSum $ users & 
  foldMapOf
    (key "users" . _Array . folded . key "metadata" . key "num_logins" . _Integer)
    (\x -> Sum x)

prefixNames :: IO Value
prefixNames = do 
  prefix <- Text.getLine
  return $ users & key "users".values.key "name"._String %~ (\x -> prefix <> x)

-- not sure what this one is doing...
-- t1 = users & sequenceAOf (key "users". values . key "associated_ips". _Array)

t2 = users & key "users". _Array . traversed . key "name" . _String .~ "<unknown>"

t3 = users & key "users" . values . key "email" . _String %~ (<> ".cn")

t4 = users & 
  foldMapOf
    (key "users" . values . key "metadata" . key "num_logins" . _Integer)
    (\x -> All $ x > 1)
 
t5 = users ^.. key "users" . _Array . traversed . key "metadata"

