{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Fancy where 

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.QQ
import Data.Text

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

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

f1 = users & key "users" . values . key "name" . _String <>~ ".dev"

-- unsure how to divide without changing numLogins type
-- f2 = user1 & metadata . numLogins //~ 2

f3 = users & key "users" . values . key "metadata" . key "num_logins" . _Integer +~ 1

f4 = user1 & userid *~ 4

infixr 4 `mappendUpdated`
mappendUpdated :: Monoid a
       => ((a -> (a, a)) -> s -> (a, t))
       -> a
       -> s
       -> (a, t)
mappendUpdated lens a s = flip lens s $ \val -> (val <> a, val <> a)

testUpdatedReal = users & key "users".values.key "name"._String <<>~ ".test"
testUpdatedMine = users & key "users".values.key "name"._String `mappendUpdated` ".test"

infixr 4 `mappendOld`
mappendOld :: Monoid a
       => ((a -> (a, a)) -> s -> (a, t))
       -> a
       -> s
       -> (a, t)
mappendOld lens a s = flip lens s $ \val -> (val, val <> a)

testOldReal = users & key "users".values.key "name"._String <<<>~ ".test"
testOldMine = users & key "users".values.key "name"._String `mappendOld` ".test"

infixr 4 `leftAppend`
leftAppend :: Monoid a
       => ((a -> (a, a)) -> s -> (a, t))
       -> a
       -> s
       -> (a, t)
leftAppend lens a s = flip lens s $ \val -> (val, a <> val)

testLeftAppend = users & key "users".values.key "name"._String `leftAppend` "test."