{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Slack where

import           Data.Aeson   as A
import           Data.Functor (void)
import           Data.Text    (pack)
import           Network.Wreq

data SlackMessage = SlackMessage { username :: String
                                 , text     :: String
                                 , channel  :: String
                                 }

instance ToJSON SlackMessage where
  toJSON SlackMessage{..} = object [ "username" .= (toJSON $ pack username)
                                   , "text"     .= (toJSON $ pack text)
                                   , "channel"  .= (toJSON $ pack channel)
                                   ]

notifyToSlack :: String
              -> String
              -> String
              -> IO ()
notifyToSlack uri channel msg = do
  void $ post uri (toJSON $ SlackMessage "toolbox" msg channel)


