{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Utils.Email where

import Addy (EmailAddr, decode, emailAddr, encode)
import Addy.Internal.Types (DomainName (DN), LocalPart (LP))
import Control.Arrow (ArrowChoice ((|||)), (>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Beam.AutoMigrate (ColumnType (SqlStdType), HasColumnType (..))
import Database.Beam.Backend (BeamBackend, FromBackendRow (..), HasSqlValueSyntax (..), varCharType)
import Servant.Docs (ToSample (..), singleSample)
import qualified Text.ParserCombinators.ReadP as RP (get)
import qualified Text.ParserCombinators.ReadPrec as RP (lift)
import qualified Text.Read as TR (parens, prec, readPrec)
import qualified Text.Show as TS (showString, showsPrec)
import Universum

-- | Email
newtype Email = Email {unEmail :: EmailAddr}

-- >>> show $ Email $ emailAddr (LP "example") (DN "exmail.com")
-- "example@exmail.com"
instance Show Email where
  showsPrec _ (Email eAddr) = TS.showString $ toString $ encode eAddr

-- >>> readEither @Text @Email "example@xmail.com"
-- Right example@xmail.com
instance Read Email where
  readPrec =
    TR.parens $
      TR.prec 0 $ RP.lift $ many RP.get >>= (toText >>> decode >>> fail . show ||| return . Email)

-- >>> toJSON $ Email $ emailAddr (LP "example") (DN "exmail.com")
-- String "example@exmail.com"
instance ToJSON Email where
  toJSON (Email e) = String $ encode e

-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode @Email "\"example@exmail.com\""
-- Right example@exmail.com
instance FromJSON Email where
  parseJSON = \case
    (String s) -> fail . toString ||| return $ readEither s
    x -> typeMismatch "emails are strings" x

-- >>> toSamples @Email Proxy
-- [("",example@exmail.com)]
instance ToSample Email where
  toSamples _ =
    singleSample $ Email $ emailAddr (LP "example") (DN "exmail.com")

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Email where
  fromBackendRow = fromBackendRow >>= (decode >>> fail . show ||| return . Email)

instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be Email where
  sqlValueSyntax = sqlValueSyntax . encode . unEmail

instance HasColumnType Email where
  defaultColumnType _ = SqlStdType $ varCharType Nothing Nothing
  defaultTypeCast _ = Just "character varying"
