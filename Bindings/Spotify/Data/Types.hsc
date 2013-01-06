module Bindings.Spotify.Data.Types (
  ApplicationKey(..),
  ApplicationKeySize,
  Link(..),
  Playlist(..),
  Session(..),
  Track(..),
  Version(..)
) where

import Foreign
import Foreign.C.Types

#include <libspotify/api.h>

-- | A Spotify API key.
newtype ApplicationKey = ApplicationKey (Ptr Word8)
 deriving (Show, Storable)

-- | The size of a spotify application key.
type ApplicationKeySize = CSize

-- | A link pointing to a Spotify resource; either a Track or Playlist.
newtype Link = Link { unLink :: Ptr () }
 deriving (Show, Storable)

-- | A Spotify playlist, an ordered collection of tracks.
newtype Playlist = Playlist { unPlaylist :: Ptr () }
 deriving (Show, Storable)

-- | A login session for a Spotify user account.
newtype Session = Session { unSession :: Ptr () }
 deriving (Show, Storable)

-- | A Spotify track. 
newtype Track = Track { unTrack :: Ptr () }
 deriving (Show, Storable)

-- | A Spotify API major version number.
newtype Version = Version CInt
 deriving (Show, Storable)
