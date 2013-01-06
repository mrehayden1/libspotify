module Bindings.Spotify.Session (
  ConnectionState,
  Credentials,
  Password,
  Remember,
  Username,
  nullCredentials,
  sp_session_connectionstate,
  sp_session_create,
  sp_session_login,
  sp_session_player_load,
  sp_session_player_play,
  sp_session_process_events
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Bindings.Spotify.Data (Session(..), SessionConfig, Track(..))
import Bindings.Spotify.Error (Error(..))

#include <libspotify/api.h>

foreign import ccall safe "libspotify/api.h sp_session_create"
  sp_session_create :: Ptr SessionConfig -> Ptr Session -> IO Error

type NextTimeout = CInt

foreign import ccall safe "libspotify/api.h sp_session_process_events"
  sp_session_process_events
    :: Session -> Ptr NextTimeout -> IO Error

type Username = CString
type Password = CString
type Remember = CInt
newtype Credentials = Credentials CString

nullCredentials :: Credentials
nullCredentials = Credentials nullPtr

foreign import ccall safe "libspotify/api.h sp_session_login" sp_session_login
  :: Session
  -> Username
  -> Password
  -> Remember
  -> Credentials
  -> IO Error

newtype ConnectionState = ConnectionState CInt
 deriving (Eq)

#{enum ConnectionState, ConnectionState
 , loggedOut    = SP_CONNECTION_STATE_LOGGED_OUT
 , loggedIn     = SP_CONNECTION_STATE_LOGGED_IN
 , disconnected = SP_CONNECTION_STATE_DISCONNECTED
 , unknown      = SP_CONNECTION_STATE_UNDEFINED
 , offline      = SP_CONNECTION_STATE_OFFLINE
 }

foreign import ccall safe "libspotify/api.h sp_session_connectionstate"
  sp_session_connectionstate :: Session -> IO ConnectionState

-- | Load a track ready to be played. You must have implemented the correct
-- session callbacks for this to succeed: music_delivery.
foreign import ccall safe "libspotify/api.h sp_session_player_load"
  sp_session_player_load :: Session -> Track -> IO Error

foreign import ccall safe "libspotify/api.h sp_session_player_play"
  sp_session_player_play :: Session -> CInt -> IO Error
