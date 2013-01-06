module Bindings.Spotify.Error (
  Error(..)

, ok
, badApiVersion
, apiInitializationFailed
, trackNotPlayable
, badApplicationKey
, badUsernameOrPassword
, userBanned
, unableToContactServer
, clientTooOld
, otherPermanent
, badUserAgent
, missingCallback
, invalidInData
, indexOutOfRange
, userNeedsPremium
, otherTransient
, isLoading
, noStreamAvailable
, permissionDenied
, inboxIsFull
, noCache
, noSuchUser
, noCredentials
, networkDisabled
, invalidDeviceId
, cantOpenTraceFile
, applicationBanned
, offlineTooManyTracks
, offlineDiskCache
, offlineExpired
, offlineNotAllowed
, offlineLicenseLost
, offlineLicenseError
, lastFmAuthError
, invalidArgument
, systemFailure

, sp_error_message
) where

import Foreign.C.String
import Foreign.C.Types

#include <libspotify/api.h>

newtype Error = Error { unError :: CInt }
 deriving (Show, Eq)

#{enum Error, Error
, ok = SP_ERROR_OK
, badApiVersion = SP_ERROR_BAD_API_VERSION
, apiInitializationFailed = SP_ERROR_API_INITIALIZATION_FAILED
, trackNotPlayable = SP_ERROR_TRACK_NOT_PLAYABLE
, badApplicationKey = SP_ERROR_BAD_APPLICATION_KEY
, badUsernameOrPassword = SP_ERROR_BAD_USERNAME_OR_PASSWORD
, userBanned = SP_ERROR_USER_BANNED
, unableToContactServer = SP_ERROR_UNABLE_TO_CONTACT_SERVER
, clientTooOld = SP_ERROR_CLIENT_TOO_OLD
, otherPermanent = SP_ERROR_OTHER_PERMANENT
, badUserAgent = SP_ERROR_BAD_USER_AGENT
, missingCallback = SP_ERROR_MISSING_CALLBACK
, invalidInData = SP_ERROR_INVALID_INDATA
, indexOutOfRange = SP_ERROR_INDEX_OUT_OF_RANGE
, userNeedsPremium = SP_ERROR_USER_NEEDS_PREMIUM
, otherTransient = SP_ERROR_OTHER_TRANSIENT
, isLoading = SP_ERROR_IS_LOADING
, noStreamAvailable = SP_ERROR_NO_STREAM_AVAILABLE
, permissionDenied = SP_ERROR_PERMISSION_DENIED
, inboxIsFull = SP_ERROR_INBOX_IS_FULL
, noCache = SP_ERROR_NO_CACHE
, noSuchUser = SP_ERROR_NO_SUCH_USER
, noCredentials = SP_ERROR_NO_CREDENTIALS
, networkDisabled = SP_ERROR_NETWORK_DISABLED
, invalidDeviceId = SP_ERROR_INVALID_DEVICE_ID
, cantOpenTraceFile = SP_ERROR_CANT_OPEN_TRACE_FILE
, applicationBanned = SP_ERROR_APPLICATION_BANNED
, offlineTooManyTracks = SP_ERROR_OFFLINE_TOO_MANY_TRACKS
, offlineDiskCache = SP_ERROR_OFFLINE_DISK_CACHE
, offlineExpired = SP_ERROR_OFFLINE_EXPIRED
, offlineNotAllowed = SP_ERROR_OFFLINE_NOT_ALLOWED
, offlineLicenseLost = SP_ERROR_OFFLINE_LICENSE_LOST
, offlineLicenseError = SP_ERROR_OFFLINE_LICENSE_ERROR
, lastFmAuthError = SP_ERROR_LASTFM_AUTH_ERROR
, invalidArgument = SP_ERROR_INVALID_ARGUMENT
, systemFailure = SP_ERROR_SYSTEM_FAILURE
}

foreign import ccall safe "libspotify.h sp_error_message"
  sp_error_message :: Error -> CString
