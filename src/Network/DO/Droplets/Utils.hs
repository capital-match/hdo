{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module Network.DO.Droplets.Utils
       (publicIP, findByIdOrName)
       where

import           Data.IP
import           Data.Maybe
import           Network.DO.Types

-- |Lookup (first) public IP for given @Droplet@, if any.
publicIP :: Droplet -> Maybe IP
publicIP (networks -> NoNetworks)   = Nothing
publicIP (networks -> Networks{..}) = ip_address <$> (listToMaybe $ filter ((== Public) . netType) v4)


-- |Find the first droplet that matches given Id or name
findByIdOrName :: String -> [ Droplet ] -> [ Droplet ]
findByIdOrName label = filter (matchIdOrName idOrName)
  where
    idOrName = case readsPrec 10 label of
                (did,""):_ -> Left did
                _          -> Right label
    matchIdOrName (Left did)    d = dropletId d == did
    matchIdOrName (Right dname) d = name d == dname

