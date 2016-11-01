{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |Contains Haskell representation of most data types used for interacting with
-- DigitalOcean's API v2
--
-- See https://developers.digitalocean.com/documentation/v2/
module Network.DO.Types where

import           Data.Aeson        as A hiding (Error, Result)
import           Data.Aeson.Types  as A hiding (Error, Result)
import           Data.Default
import qualified Data.HashMap.Lazy as H
import           Data.IP
import           Data.List         (elemIndex)
import           Data.Monoid       ((<>))
import           Data.Text         (pack, unpack)
import           Data.Time         (UTCTime)
import           GHC.Generics


type AuthToken = String

type Slug = String

type URI = String

newtype Error = Error { msg :: String } deriving (Eq, Show, Read)

type Result a = Either Error a

error :: String -> Result a
error = Left . Error

data ToolConfiguration = Tool { slackUri  :: Maybe URI
                              , authToken :: Maybe AuthToken
                              , quiet     :: Bool
                              } deriving (Show,Read)

instance Default ToolConfiguration where
  def = Tool Nothing Nothing False

-- | A type for describing @Region@
-- A region can be assigned an empty object when it is undefined, or be referenced simply
-- by its @slug@
-- https://developers.digitalocean.com/documentation/v2/#regions
data Region = Region { regionName      :: String
                     , regionSlug      :: Slug
                     , regionSizes     :: [ SizeSlug ]
                     , regionAvailable :: Bool
                     }
            | RegionSlug Slug
            | NoRegion

instance ToJSON Region where
  toJSON (RegionSlug s) = toJSON s
  toJSON  NoRegion      = object []
  toJSON Region{..}     = object [ "name" .= regionName
                                 , "slug" .= regionSlug
                                 , "sizes" .= regionSizes
                                 , "available" .= regionAvailable
                                 ]

instance Show Region where
  show (RegionSlug s) = s
  show  NoRegion      = "NoRegion"
  show Region{..}     = "Region { regionName = " <> show regionName <>
                        ", regionSlug = " <> show regionSlug <>
                        ", regionSizes = " <> show regionSizes <>
                        ", regionAvailable = " <> show regionAvailable <>
                        "}"

instance FromJSON Region where
  parseJSON (String s) = return $ RegionSlug (unpack s)
  parseJSON (Object o) = if H.null o
                         then return NoRegion
                         else Region
                              <$> o .: "name"
                              <*> o .: "slug"
                              <*> o .: "sizes"
                              <*> o .: "available"
  parseJSON e          = failParse e

-- | String representation of size slugs
-- This maps to corresponding expected JSON string value.
sizeSlugs :: [String]
sizeSlugs = [ "512mb", "1gb",  "2gb",  "4gb",  "8gb",  "16gb", "32gb", "48gb", "64gb", "96gb"  ]

-- | Enumeration of all possible size slugs
data SizeSlug = M512 | G1 | G2 | G4 | G8 | G16 | G32 | G48 | G64 | G96
              deriving (Enum,Ord,Eq)

instance Show SizeSlug where
  show sz = sizeSlugs !! fromEnum sz

instance Read SizeSlug where
  readsPrec _ sz = case elemIndex sz sizeSlugs of
                    Just i  -> return (toEnum i, "")
                    Nothing -> fail $ "cannot parse " <> sz

instance ToJSON SizeSlug where
  toJSON sz = toJSON $ sizeSlugs !! fromEnum sz

instance FromJSON SizeSlug where
  parseJSON (String s) = case elemIndex (unpack s) sizeSlugs of
                          Just i -> return $ toEnum i
                          Nothing -> fail $ "cannot parse " <> unpack s
  parseJSON e          = failParse e


type ImageSlug = String
type KeyId = Int

defaultImage :: ImageSlug
defaultImage = "ubuntu-14-04-x64"

data BoxConfiguration = BoxConfiguration { configName       :: String
                                         , boxRegion        :: Region
                                         , size             :: SizeSlug
                                         , configImageSlug  :: ImageSlug
                                         , keys             :: [KeyId]
                                         , backgroundCreate :: Bool
                                         } deriving (Show)

instance ToJSON BoxConfiguration where
  toJSON BoxConfiguration{..} = object [ "name"     .=  configName
                                       , "region"   .=  boxRegion
                                       , "size"     .=  size
                                       , "image"    .=  configImageSlug
                                       , "ssh_keys" .=  keys
                                       , "backups"  .=  False
                                       , "ipv6"     .=  False
                                       , "private_networking" .=  False
                                       ]

type Id = Integer

data Mega
data Giga

-- | A type for various sizes
-- Type parameter is used to define number's magnitude
newtype Bytes a = Bytes { bytesSize :: Int } deriving Show

jsonBytes :: Int -> Parser (Bytes a)
jsonBytes = return . Bytes

instance FromJSON (Bytes Mega) where
  parseJSON (Number n) = jsonBytes (truncate n)
  parseJSON e          = failParse e

instance FromJSON (Bytes Giga) where
  parseJSON (Number n) = jsonBytes (truncate n)
  parseJSON e          = failParse e

newtype Date = Date { theDate :: UTCTime } deriving Show

instance FromJSON Date where
  parseJSON d@(String _) = Date <$> parseJSON d
  parseJSON e            = failParse e

data Status = New
            | Active
            | Off
            | Archive
            deriving (Eq,Show)

instance FromJSON Status where
  parseJSON (String s) = case s of
                          "new" -> return New
                          "active" -> return Active
                          "off" -> return Off
                          "archive" -> return Archive
                          _        -> fail $ "cannot parse " <> unpack s
  parseJSON e          = failParse e

data NetType = Public | Private deriving (Show, Eq)

-- | Type of a single Network definition
--
-- This type is parameterized with a phantom type which lifts the network address type at
-- the type level (could use DataKinds extension...). This allows distinguishing types of
-- of networks while using same parsing.
data Network a = NetworkV4 { ip_address :: IP
                           , netmask    :: IP
                           , gateway    :: IP
                           , netType    :: NetType
                           }
               | NetworkV6 { ip_address :: IP
                           , netmask_v6 :: Int
                           , gateway    :: IP
                           , netType    :: NetType
                           } deriving Show

instance FromJSON IP where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON e          = fail $ "cannot parse IP " <> show e

instance ToJSON IP where
  toJSON = String . pack . show

instance FromJSON NetType where
  parseJSON (String s) = case s of
                          "public" -> return Public
                          "private" -> return Private
                          e         -> failParse e
  parseJSON e          = failParse e

data V4
data V6

jsonNetwork :: (FromJSON a3, FromJSON a2, FromJSON a1, FromJSON a) => (a3 -> a2 -> a1 -> a -> b) -> Object -> Parser b
jsonNetwork f n = f
                  <$> (n .: "ip_address")
                  <*> (n .: "netmask")
                  <*> (n .: "gateway")
                  <*> (n .: "type")

instance FromJSON (Network V4) where
  parseJSON (Object n) = jsonNetwork NetworkV4 n
  parseJSON e          = failParse e

instance FromJSON (Network V6) where
  parseJSON (Object n) = jsonNetwork NetworkV6 n
  parseJSON e          = fail $ "cannot parse network v6 " <> show e


-- | Type of Networks configured for a @Droplet@
--
-- A network is either a list of IPv4 and IPv6 NICs definitions, or no network. We need this
-- because a droplet can contain an ''empty'' @networks@  JSON Object entry, instead of @null@.
data Networks = Networks { v4 :: [ Network V4 ]
                         , v6 :: [ Network V6 ]
                         }
              | NoNetworks
              deriving (Generic, Show)

instance FromJSON Networks where
  parseJSON (Object n) = if H.null n
                         then return NoNetworks
                         else Networks
                              <$> (n .: "v4")
                              <*> (n .: "v6")
  parseJSON e          = fail $ "cannot parse network v6 " <> show e

-- | (Partial) Type of Droplets
--
-- https://developers.digitalocean.com/documentation/v2/#droplets
data Droplet = Droplet { dropletId    :: Id
                       , name         :: String
                       , memory       :: Bytes Mega
                       , vcpus        :: Int
                       , disk         :: Bytes Giga
                       , locked       :: Bool
                       , created_at   :: Date
                       , status       :: Status
                       , backup_ids   :: [ Id ]
                       , snapshot_ids :: [ Id ]
                       , region       :: Region
                       , size_slug    :: SizeSlug
                       , networks     :: Networks
                       } deriving (Show)

instance FromJSON Droplet where
  parseJSON (Object o) = Droplet
                         <$> o .: "id"
                         <*> o .: "name"
                         <*> o .: "memory"
                         <*> o .: "vcpus"
                         <*> o .: "disk"
                         <*> o .: "locked"
                         <*> o .: "created_at"
                         <*> o .: "status"
                         <*> o .: "backup_ids"
                         <*> o .: "snapshot_ids"
                         <*> o .: "region"
                         <*> o .: "size_slug"
                         <*> o .: "networks"
  parseJSON e          = fail $ "cannot parse network v6 " <> show e


data ImageType = Snapshot
               | Temporary
               | Backup
                 deriving Show

instance FromJSON ImageType where
  parseJSON (String s) = case s of
                          "snapshot" -> return Snapshot
                          "temporary" -> return Temporary
                          "backup" -> return Backup
                          _        -> fail $ "cannot parse " <> unpack s
  parseJSON e          = failParse e

-- | Type of droplet images
--
-- https://developers.digitalocean.com/documentation/v2/#images
data Image = Image { imageId          :: Id
                   , imageName        :: String
                   , distribution     :: String
                   , imageSlug        :: Maybe Slug
                   , publicImage      :: Bool
                   , imageRegions     :: [ Region ]
                   , min_disk_size    :: Bytes Giga
                   , image_created_at :: Date
                   , imageType        :: ImageType
                   } deriving Show

instance FromJSON Image where
  parseJSON (Object o) = Image
                         <$> o .: "id"
                         <*> o .: "name"
                         <*> o .: "distribution"
                         <*> o .:? "slug"
                         <*> o .: "public"
                         <*> o .: "regions"
                         <*> o .: "min_disk_size"
                         <*> o .: "created_at"
                         <*> o .: "type"
  parseJSON e          = failParse e

-- | Type of SSH @Key@s
--
--https://developers.digitalocean.com/documentation/v2/#ssh-keys
data Key = Key { keyId          :: Id
               , keyFingerprint :: String
               , publicKey      :: String
               , keyName        :: String
               } deriving Show

instance FromJSON Key where
  parseJSON (Object o) = Key
                         <$> o .: "id"
                         <*> o .: "fingerprint"
                         <*> o .: "public_key"
                         <*> o .: "name"
  parseJSON e          = failParse e

type TransferRate = Double
type Price = Double

-- | Type of Size objects
--
-- https://developers.digitalocean.com/documentation/v2/#sizes
data Size = Size { szSlug          :: SizeSlug
                 , szMemory        :: Bytes Mega
                 , szVcpus         :: Int
                 , szDisk          :: Bytes Giga
                 , szTransfer      :: TransferRate
                 , szPrice_Monthly :: Price
                 , szPrice_Hourly  :: Price
                 , szRegions       :: [ Region ]
                 , szAvailable     :: Bool
                 } deriving (Show)


instance FromJSON Size where
  parseJSON (Object o) = Size
                         <$> o .: "slug"
                         <*> o .: "memory"
                         <*> o .: "vcpus"
                         <*> o .: "disk"
                         <*> o .: "transfer"
                         <*> o .: "price_monthly"
                         <*> o .: "price_hourly"
                         <*> o .: "regions"
                         <*> o .: "available"

  parseJSON e          = failParse e

-- * Droplets Actions

-- | Type of action status
-- This is returned when action is initiated or when status of some action is requested

data ActionResult result = ActionResult { actionId           :: Id
                                        , actionStatus       :: ActionStatus
                                        , actionType         :: result
                                        , actionStartedAt    :: Maybe Date
                                        , actionCompletedAt  :: Maybe Date
                                        , actionResourceId   :: Id
                                        , actionResourceType :: String
                                        , actionRegionSlug   :: Region
                                        } deriving (Show)

instance (FromJSON r) => FromJSON (ActionResult r) where
  parseJSON (Object o) = ActionResult
                         <$> o .: "id"
                         <*> o .: "status"
                         <*> o .: "type"
                         <*> o .:? "started_at"
                         <*> o .:? "completed_at"
                         <*> o .: "resource_id"
                         <*> o .: "resource_type"
                         <*> o .: "region_slug"
  parseJSON v          = fail $ "cannot parse action " ++ show v

data ActionStatus = InProgress
                  | Completed
                  | Errored
                  deriving (Show)

instance FromJSON ActionStatus where
  parseJSON (String s) = case s of
                          "in-progress" -> return InProgress
                          "completed"   -> return Completed
                          "errored"     -> return Errored
                          _             -> fail $ "unknown action status " ++ show s
  parseJSON v          = fail $ "cannot parse action status " ++ show v

data DropletActionType = PowerOff
                | PowerOn
                | MakeSnapshot
                deriving (Show)

instance FromJSON DropletActionType where
  parseJSON (String s) = case s of
                          "power_off" -> return PowerOff
                          "power_on"  -> return PowerOn
                          "snapshot"  -> return MakeSnapshot
                          _           -> fail $ "unknown action type " ++ show s
  parseJSON v          = fail $ "cannot parse action type " ++ show v

instance ToJSON DropletActionType where
  toJSON PowerOff = String "power_off"
  toJSON PowerOn  = String "power_on"
  toJSON MakeSnapshot = String "snapshot"

data Action = DoPowerOff
            | DoPowerOn
            | CreateSnapshot String
            deriving Show

instance ToJSON Action where
  toJSON DoPowerOff                    = object [ "type" .= PowerOff ]
  toJSON DoPowerOn                     = object [ "type" .= PowerOn ]
  toJSON (CreateSnapshot snapshotName) = object [ "type" .= MakeSnapshot
                                                , "name" .= snapshotName
                                                ]

-- |Type of Domain zones
--
-- https://developers.digitalocean.com/documentation/v2/#domains

newtype DomainName = DomainName { domain :: String }

instance Show DomainName where
  show = domain

instance Read DomainName where
  readsPrec _ s = [(DomainName s,[])]

instance FromJSON DomainName where
  parseJSON (String s) = pure $ DomainName $ unpack s
  parseJSON e          = failParse e

instance ToJSON DomainName where
  toJSON (DomainName n) = String (pack n)

data Domain = Domain { domainName :: DomainName
                     , domainTTL  :: Maybe Int
                     , zone_file  :: Maybe String
                     } deriving (Show)

instance FromJSON Domain where
  parseJSON (Object o) = Domain
                         <$> o .: "name"
                         <*> o .: "ttl"
                         <*> o .: "zone_file"

  parseJSON e          = failParse e

data DomainConfig = DomainConfig DomainName IP

instance ToJSON DomainConfig where
  toJSON (DomainConfig name ip) = object [ "name" .= name
                                         , "ip_address" .= ip
                                         ]

-- | Enumeration of possible DNS records types
data DNSType = A | CNAME | TXT | PTR | SRV | NS | AAAA | MX
             deriving (Show, Read, Generic)

instance FromJSON DNSType
instance ToJSON DNSType

-- | Type of Domain zone file entries
--
-- https://developers.digitalocean.com/documentation/v2/#domain-records
data DomainRecord = DomainRecord { recordId       :: Id
                                 , recordType     :: DNSType
                                 , recordName     :: String
                                 , recordData     :: String
                                 , recordPriority :: Maybe Double
                                 , recordPort     :: Maybe Int
                                 , recordWeight   :: Maybe Double
                                 } deriving (Show)


instance FromJSON DomainRecord where
  parseJSON (Object o) = DomainRecord
                         <$> o .: "id"
                         <*> o .: "type"
                         <*> o .: "name"
                         <*> o .: "data"
                         <*> o .: "priority"
                         <*> o .: "port"
                         <*> o .: "weight"

  parseJSON e          = failParse e

failParse :: (Show a1, Monad m) => a1 -> m a
failParse e = fail $ "cannot parse " <> show e

-- | Floating IPs
-- https://developers.digitalocean.com/documentation/v2/#floating-ips

data FloatingIP = FloatingIP { floatingIp      :: IP
                             , floatingDroplet :: Maybe Droplet
                             , floatingRegion  :: Region
                             } deriving (Show)

instance FromJSON FloatingIP where
  parseJSON (Object o) = FloatingIP
                         <$> o .: "ip"
                         <*> o .:? "droplet"
                         <*> o .: "region"

  parseJSON e          = failParse e

data FloatingIPTarget = TargetRegion Slug
                      | TargetDroplet Id
                        deriving (Show)

instance ToJSON FloatingIPTarget where
  toJSON (TargetRegion r)  = object [ "region" .= r ]
  toJSON (TargetDroplet i) = object [ "droplet_id" .= i ]

data IPAction = AssignIP Id
              | UnassignIP
  deriving (Show, Read)

instance ToJSON IPAction where
  toJSON (AssignIP did) = object [ "type" .= ("assign" :: String)
                                 , "droplet_id" .= did
                                 ]
  toJSON UnassignIP     = object [ "type" .= ("unassign" :: String)]

data IPActionType = Assign
                  | Unassign
                deriving (Show)

instance FromJSON IPActionType where
  parseJSON (String s) = case s of
                          "assign_ip" -> return Assign
                          "unassign_ip"  -> return Unassign
                          _           -> fail $ "unknown action type " ++ show s
  parseJSON v          = fail $ "cannot parse action type " ++ show v
