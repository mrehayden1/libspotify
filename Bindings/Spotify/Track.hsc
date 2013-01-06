module Bindings.Spotify.Track (
  sp_track_error,
  sp_track_is_loaded,
  sp_track_name
) where

import Bindings.Spotify.Data.Types (Track(..))
import Bindings.Spotify.Error (Error(..))

import Foreign.C.String
import Foreign.C.Types

#include <libspotify/api.h>

foreign import ccall "libspotify/api.h sp_track_is_loaded"
  sp_track_is_loaded :: Track -> IO CInt

foreign import ccall safe "libspotify/api.h sp_track_name" 
  sp_track_name :: Track -> IO CString

foreign import ccall safe "libspotify/api.h  sp_track_error"
  sp_track_error :: Track -> IO Error
