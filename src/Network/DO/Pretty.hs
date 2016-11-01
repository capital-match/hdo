{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.DO.Pretty where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IP                (IP)
import           Network.DO.Types
import           Text.PrettyPrint

class (Show a) => Pretty a where
  pretty :: a -> Doc
  pretty = text . show

instance Pretty () where
  pretty () = text ""

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = text "Error:" <+> pretty a
  pretty (Right a) = pretty a

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just a) = pretty a
  pretty Nothing  = text "-"

instance Pretty Char where
  pretty = char

instance Pretty Error where
  pretty (Error m) = text m

instance Pretty Date where
  pretty (Date d) = text $ show d

instance Pretty Droplet where
  pretty Droplet{..} = integer dropletId $$
                       (nest 5 $ (text name) <+> brackets (pretty status) <+> pretty region) $$
                       (nest 5 $ hcat $ punctuate (char '/') [pretty memory, pretty disk, int vcpus <+> text "cores"]) $$
                       (nest 5 $ pretty networks)

instance Pretty Status
instance Pretty NetType
instance Pretty IP

instance Pretty Region where
  pretty Region{..}     = text (regionSlug ++ ": "
                                ++regionName)
  pretty (RegionSlug s) = text s
  pretty NoRegion       = empty

instance Pretty Networks where
  pretty Networks{..} = text "IPv4" $$ nest 2 (pretty v4) $$
                        text "IPv6" $$ nest 2 (pretty v6)
  pretty NoNetworks   = text "N/A"

instance Pretty (Network a) where
  pretty NetworkV4{..} = pretty ip_address <> char '/' <> pretty netmask <+> brackets (pretty netType)
  pretty NetworkV6{..} = pretty ip_address <> char '/' <> int netmask_v6 <+> brackets (pretty netType)

instance Pretty (Bytes Giga) where
  pretty Bytes{..} = int bytesSize <> text "G"

instance Pretty (Bytes Mega) where
  pretty Bytes{..} = int bytesSize <> text "M"

instance (Pretty a) => Pretty [a] where
  pretty = sep . map pretty

instance Pretty Key where
  pretty Key{..} = integer keyId <+> text keyName <+> text keyFingerprint <+> text publicKey

instance Pretty Image where
  pretty Image{..} = integer imageId <+> text imageName <+> text distribution

instance Pretty TransferRate
instance Pretty SizeSlug

instance Pretty Size where
  pretty Size{..} = pretty szSlug $$
                    nest 5 (hcat $ punctuate (char '/') [pretty szMemory, int szVcpus, pretty szDisk, pretty szTransfer]) $$
                    nest 5 (pretty szPrice_Hourly <> text "$/h, " <> pretty szPrice_Monthly <> text "$/mo" ) $$
                    nest 5 (hcat $ punctuate (char ',') $ map pretty szRegions)

instance (Pretty r) => Pretty (ActionResult r) where
  pretty ActionResult{..} = brackets (integer actionId) <+> pretty actionStartedAt <+> text "->" <+> pretty actionCompletedAt $$
                            (nest 5 $ integer actionResourceId <+> pretty actionType <> char ':' <+> text (show actionStatus))

instance Pretty DropletActionType

instance Pretty FloatingIP where
  pretty FloatingIP{..} =
    case floatingDroplet of
      Nothing -> ipAndRegion <+> text "?"
      Just d  -> ipAndRegion <+> integer (dropletId d) <> char '/' <> text (name d)
    where
      ipAndRegion = pretty floatingIp <> text "/" <>  text (regionSlug floatingRegion) <+> text "->"

instance Pretty IPActionType

instance Pretty Domain where
  pretty Domain{..} = text (domain domainName) $$
                      maybe mempty (nest 5 . vcat . map text .lines) zone_file

instance Pretty DomainRecord where
  pretty DomainRecord{..} = brackets (integer recordId) <+> pretty recordType <+> text recordName <+> text recordData
                            <+> prettyInt recordPriority
                            <+> prettyInt recordPort
                            <+> prettyInt recordWeight
    where
      prettyInt = maybe (char '-') int

instance Pretty DNSType

outputResult :: (Pretty a, MonadIO m) => a -> m  ()
outputResult = liftIO . putStrLn . render . pretty

