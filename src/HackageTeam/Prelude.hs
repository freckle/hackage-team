module HackageTeam.Prelude
  ( module X
  , logError
  , logWarn
  , logInfo
  , logDebug
  , logOther
  , decodeUtf8
  ) where

import RIO as X hiding
  ( LogLevel(..)
  , LogSource
  , logDebug
  , logDebugS
  , logError
  , logErrorS
  , logInfo
  , logInfoS
  , logOther
  , logOtherS
  , logWarn
  , logWarnS
  )

import Control.Monad.Logger as X hiding
  (logDebug, logError, logInfo, logOther, logWarn)
import RIO.Text as X (pack, unpack)

logError :: MonadLogger m => Utf8Builder -> m ()
logError = logErrorN . utf8BuilderToText

logWarn :: MonadLogger m => Utf8Builder -> m ()
logWarn = logWarnN . utf8BuilderToText

logInfo :: MonadLogger m => Utf8Builder -> m ()
logInfo = logInfoN . utf8BuilderToText

logDebug :: MonadLogger m => Utf8Builder -> m ()
logDebug = logDebugN . utf8BuilderToText

logOther :: MonadLogger m => LogLevel -> Utf8Builder -> m ()
logOther l = logOtherN l . utf8BuilderToText

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
