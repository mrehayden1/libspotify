module Bindings.Spotify.Playlist (
  sp_playlist_add_callbacks
, sp_playlist_create
, sp_playlist_is_loaded
, sp_playlist_name
, sp_playlist_remove_callbacks
, sp_playlist_track
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Bindings.Spotify.Error (Error(..))
import Bindings.Spotify.Data (Link(..), Playlist(..), PlaylistCallbacks,
         PlaylistCallbackUserData, Session(..), Track(..))

#include <libspotify/api.h>

foreign import ccall safe "libspotify/api.h sp_playlist_create"
  sp_playlist_create :: Session -> Link -> IO Playlist

foreign import ccall safe "libspotify/api.h sp_playlist_is_loaded"
  sp_playlist_is_loaded :: Playlist -> IO CInt

foreign import ccall safe "libspotify/api.h sp_playlist_add_callbacks"
  sp_playlist_add_callbacks :: Playlist -> Ptr PlaylistCallbacks
    -> PlaylistCallbackUserData -> IO Error

foreign import ccall safe "libspotify/api.h sp_playlist_remove_callbacks"
  sp_playlist_remove_callbacks :: Playlist -> Ptr PlaylistCallbacks
    -> PlaylistCallbackUserData -> IO Error

foreign import ccall safe "libspotify/api.h sp_playlist_name"
  sp_playlist_name :: Playlist -> IO CString

foreign import ccall safe "libspotify/api.h sp_playlist_track"
  sp_playlist_track :: Playlist -> CInt -> IO Track
