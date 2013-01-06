module Bindings.Spotify.Version (
  spotifyVersion
) where

import Bindings.Spotify.Data.Types (Version(..))

#include <libspotify/api.h>

-- | The version of the currently installed Spotify API.
spotifyVersion :: Version
spotifyVersion = Version #const SPOTIFY_API_VERSION
