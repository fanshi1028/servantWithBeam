{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Databases.HitmenBusiness.Util.Chronos where

import Chronos (Datetime, DatetimeFormat (DatetimeFormat), SubsecondPrecision (SubsecondPrecisionFixed), builderUtf8_YmdHMS, decode_YmdHMS_lenient, now, timeToDatetime)
import Data.Aeson (FromJSON (..))
import Data.Text.Encoding (decodeUtf8)
import Data.Time (LocalTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Database.Beam (FromBackendRow, QGenExpr (..))
import Database.Beam.AutoMigrate (HasColumnType (..))
import Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax (..), currentTimestampE, timestampType)
import Database.Beam.Migrate (HasDefaultSqlDataType (..))
import Database.Beam.Postgres (Postgres, ResultError (ConversionFailed, Incompatible, UnexpectedNull))
import Database.Beam.Postgres.Syntax (PgValueSyntax, defaultPgValueSyntax)
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError, typeOid)
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (..), inQuotes)
import Database.PostgreSQL.Simple.TypeInfo.Static (timestampOid)
import Servant (Proxy (Proxy))

instance HasDefaultSqlDataType Postgres Datetime where
  defaultSqlDataType _ _ _ = timestampType Nothing False

instance ToField Datetime where
  toField = Plain . inQuotes . builderUtf8_YmdHMS (SubsecondPrecisionFixed 6) (DatetimeFormat (Just '-') (Just ' ') (Just ':'))

instance HasSqlValueSyntax PgValueSyntax Datetime where
  sqlValueSyntax = defaultPgValueSyntax

instance FromJSON Datetime where
  parseJSON v = parseJSON v >>= (maybe (fail "parse Datetime failed") return . decode_YmdHMS_lenient)

instance FromField Datetime where
  fromField f
    | typeOid f /= timestampOid = return $ returnError Incompatible f ""
    | otherwise =
      maybe
        (returnError UnexpectedNull f "")
        $ maybe (returnError ConversionFailed f "") return <$> decode_YmdHMS_lenient . decodeUtf8

instance FromBackendRow Postgres Datetime

instance HasColumnType Datetime where
  defaultColumnType _ = defaultColumnType @LocalTime Proxy
  defaultTypeCast _ = defaultTypeCast @LocalTime Proxy

currentTimestamp_' :: (BeamSqlBackend be) => QGenExpr ctxt be s Datetime
currentTimestamp_' = QExpr (pure currentTimestampE)

-- >>> toField . timeToDatetime <$> now
-- Plain "'2020-12-06 05:26:00.836014'"

-- >>> toField <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
-- Plain "'2020-12-06 13:26:08.791229'"
