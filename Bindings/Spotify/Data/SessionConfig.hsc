module Bindings.Spotify.Data.SessionConfig (
  SessionConfig(..),
  defaultSessionConfig
) where

import Control.Applicative

import Foreign
import Foreign.C.String

import Bindings.Spotify.Data.Types (ApplicationKey, ApplicationKeySize, Version)
import Bindings.Spotify.Data.SessionCallbacks (SessionCallbacks)
import Bindings.Spotify.Version (spotifyVersion)

#include <libspotify/api.h>

-- | Configuration options for a new Spotify session.
data SessionConfig = SessionConfig {
  -- | Current API version, unexposed to library users.
  apiVersion                   :: Version
  -- | A location to store cached data. Required.
, cacheLocation                :: CString
  -- | A location to store settings information. Required.
, settingsLocation             :: CString
  -- | The applications unique Spotify API key. Required.
, applicationKey               :: ApplicationKey
  -- | The size of the API key. Required.
, applicationKeySize           :: ApplicationKeySize
  -- | The user agent string of the application, e.g. "My Spotify Application 1.0" . Required.
, userAgent                    :: CString
  -- | Session callbacks. Unimplemented.
, callbacks                    :: Ptr SessionCallbacks
  -- | User session data. Unimplemented.
, userData                     :: Ptr ()
, compressPlaylists            :: Bool
, dontSaveMetadataForPlaylists :: Bool
, initiallyUnloadPlaylists     :: Bool
, deviceId                     :: CString
, proxy                        :: CString
, proxyUsername                :: CString
, proxyPassword                :: CString
, caCertsFilename              :: CString
, tracefile                    :: CString
}
 deriving (Show)

instance Storable SessionConfig where
  sizeOf _      = #size sp_session_config
  alignment     = sizeOf
  peek ptr      =
    SessionConfig
      <$> (#peek sp_session_config, api_version) ptr
      <*> (#peek sp_session_config, cache_location) ptr
      <*> (#peek sp_session_config, settings_location) ptr
      <*> (#peek sp_session_config, application_key) ptr
      <*> (#peek sp_session_config, application_key_size) ptr
      <*> (#peek sp_session_config, user_agent) ptr
      <*> (#peek sp_session_config, callbacks) ptr
      <*> (#peek sp_session_config, userdata) ptr
      <*> (#peek sp_session_config, compress_playlists) ptr
      <*> (#peek sp_session_config, dont_save_metadata_for_playlists) ptr
      <*> (#peek sp_session_config, initially_unload_playlists) ptr
      <*> (#peek sp_session_config, device_id) ptr
      <*> (#peek sp_session_config, proxy) ptr
      <*> (#peek sp_session_config, proxy_username) ptr
      <*> (#peek sp_session_config, proxy_password) ptr
      <*> (#peek sp_session_config, ca_certs_filename) ptr
      <*> (#peek sp_session_config, tracefile) ptr
  poke ptr conf =
    do (#poke sp_session_config, api_version) ptr (apiVersion conf)
       (#poke sp_session_config, cache_location) ptr (cacheLocation conf)
       (#poke sp_session_config, settings_location) ptr (settingsLocation conf)
       (#poke sp_session_config, application_key) ptr (applicationKey conf)
       (#poke sp_session_config, application_key_size) ptr (applicationKeySize conf)
       (#poke sp_session_config, user_agent) ptr (userAgent conf)
       (#poke sp_session_config, callbacks) ptr (callbacks conf)
       (#poke sp_session_config, userdata) ptr (userData conf)
       (#poke sp_session_config, compress_playlists) ptr (compressPlaylists conf)
       (#poke sp_session_config, dont_save_metadata_for_playlists) ptr
         (dontSaveMetadataForPlaylists conf)
       (#poke sp_session_config, initially_unload_playlists) ptr
         (initiallyUnloadPlaylists conf)
       (#poke sp_session_config, device_id) ptr (deviceId conf)
       (#poke sp_session_config, proxy) ptr (proxy conf)
       (#poke sp_session_config, proxy_username) ptr (proxyUsername conf)
       (#poke sp_session_config, proxy_password) ptr (proxyPassword conf)
       (#poke sp_session_config, ca_certs_filename) ptr (caCertsFilename conf)
       (#poke sp_session_config, tracefile) ptr (tracefile conf)

-- | Library defaults for the session config. Missing required fields will thrown an error (cacheLocation, settingsLocation, applicationKey, applicationKeySize, userAgent).
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig {
  apiVersion                   = spotifyVersion
, cacheLocation                = error "You must specify a cache location in your session config."
, settingsLocation             = error "You must specify a settings location in your session config."
, applicationKey               = error "You must specify an application key in your session config."
, applicationKeySize           = error "You must specify an application key size in your session config."
, userAgent                    = error "You must specify a user agent string in your session config"
, callbacks                    = nullPtr
, userData                     = nullPtr
, compressPlaylists            = False
, dontSaveMetadataForPlaylists = False
, initiallyUnloadPlaylists     = False
, deviceId                     = nullPtr
, proxy                        = nullPtr
, proxyUsername                = nullPtr
, proxyPassword                = nullPtr
, caCertsFilename              = nullPtr
, tracefile                    = nullPtr
}
