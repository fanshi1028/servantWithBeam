{-# LANGUAGE MultiParamTypeClasses #-}

module Databases.HitmenBusiness.Utils.Chronos where

import Chronos (Date (Date), Datetime (Datetime), DatetimeFormat (DatetimeFormat), DayOfMonth (DayOfMonth, getDayOfMonth), Month (getMonth), SubsecondPrecision (SubsecondPrecisionFixed), TimeOfDay (TimeOfDay), Year (Year, getYear), builderUtf8_YmdHMS, decode_YmdHMS_lenient)
import Data.Validity (Validity (..), declare)
import Database.Beam (FromBackendRow, QGenExpr (..))
import Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax (..), currentTimestampE)
import Database.Beam.Postgres (Postgres, ResultError (ConversionFailed, Incompatible, UnexpectedNull))
import Database.Beam.Postgres.Syntax (PgValueSyntax, defaultPgValueSyntax)
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError, typeOid)
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (..), inQuotes)
import Database.PostgreSQL.Simple.TypeInfo.Static (timestampOid)
import Universum

instance ToField Datetime where
  toField = Plain . inQuotes . builderUtf8_YmdHMS (SubsecondPrecisionFixed 6) (DatetimeFormat (Just '-') (Just ' ') (Just ':'))

instance HasSqlValueSyntax PgValueSyntax Datetime where
  sqlValueSyntax = defaultPgValueSyntax

instance FromField Datetime where
  fromField f
    | typeOid f /= timestampOid = return $ returnError Incompatible f ""
    | otherwise =
      maybe
        (returnError UnexpectedNull f "")
        $ maybe (returnError ConversionFailed f "") return <$> decode_YmdHMS_lenient . decodeUtf8

instance FromBackendRow Postgres Datetime

instance Validity Year where
  validate (Year y) = declare "Must between 1800 and 2100" $ 2100 >= y && y >= 1800

-- >>> validate $ Month 12
-- Validation {unValidation = [Violated "Must between January and December"]}
instance Validity Month where
  validate m = declare "Month must be between January and December" $ maxBound >= m && m >= minBound

instance Validity DayOfMonth where
  validate (DayOfMonth d) = declare "DayOfMonth must be between 1 and 31" $ 31 >= d && d >= 1

instance Validity Date where
  validate (Date y m d) =
    validate y <> validate m <> declare "Day must be between 1 and 31" (d' >= 1 && d' <= lastDay)
    where
      d' = getDayOfMonth d
      m' = getMonth m
      y' = getYear y
      bigMonth = [0, 2, 4, 6, 7, 9, 11]
      smallMonth = [3, 5, 8, 10]
      lastDay
        | m' `elem` bigMonth = 31
        | m' `elem` smallMonth = 30
        | y' `mod` 400 == 0 = 29
        | y' `mod` 100 == 0 = 28
        | y' `mod` 4 == 0 = 29
        | otherwise = 28

instance Validity TimeOfDay where
  validate (TimeOfDay h m s) =
    mconcat
      [ declare "Hour must be between 0 and 23" $ h >= 0 && h <= 23,
        declare "Minute must be between 0 and 59" $ m >= 0 && h <= 59,
        declare "Second(up to nanosecond) must be stored as value between 0 and 59999999999" $ s >= 0 && h <= 59999999999
      ]

instance Validity Datetime where
  validate (Datetime d t) = validate d <> validate t

currentTimestamp_' :: (BeamSqlBackend be) => QGenExpr ctxt be s Datetime
currentTimestamp_' = QExpr (pure currentTimestampE)

-- >>> toField . timeToDatetime <$> now
-- Plain "'2020-12-06 05:26:00.836014'"

-- >>> toField <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
-- Plain "'2020-12-06 13:26:08.791229'"
