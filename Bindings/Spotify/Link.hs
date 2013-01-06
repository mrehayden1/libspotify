module Bindings.Spotify.Link (
  sp_link_create_from_string
) where

import Foreign.C.String

import Bindings.Spotify.Data (Link(..))

foreign import ccall safe "libspotify/api.h sp_link_create_from_string"
  sp_link_create_from_string :: CString -> Link
