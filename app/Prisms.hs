{-# LANGUAGE QuasiQuotes #-}
module Prisms where

import qualified Data.Text as Text

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.QQ

user1 = [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

user2 = [aesonQQ|
  {
    "name": "ye.xiu",
    "metadata": {
      "num_logins": 27
    }
  }
|]
