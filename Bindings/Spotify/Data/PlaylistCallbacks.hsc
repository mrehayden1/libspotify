module Bindings.Spotify.Data.PlaylistCallbacks (
  PlaylistCallbacks(..),
  PlaylistStateChangedCallback,
  PlaylistCallbackUserData,
  emptyPlaylistCallbacks
) where

import Control.Applicative

import Foreign

import Bindings.Spotify.Data.Types (Playlist)

#include <libspotify/api.h>

type PlaylistCallbackUserData = Ptr ()

type PlaylistStateChangedCallback = Playlist -> PlaylistCallbackUserData
       -> IO ()

data PlaylistCallbacks = PlaylistCallbacks {
  onPlaylistTracksAdded         :: Ptr ()
, onPlaylistTracksRemoved       :: Ptr ()
, onPlaylistTracksMoved         :: Ptr ()
, onPlaylistRenamed             :: Ptr ()
, onPlaylistStateChanged        :: FunPtr PlaylistStateChangedCallback
, onPlaylistUpdateInProgress    :: Ptr ()
, onPlaylistMetadataUpdated     :: Ptr ()
, onPlaylistTrackCreatedChanged :: Ptr ()
, onPlaylistTrackSeenChanged    :: Ptr ()
, onPlaylistDescriptionChanged  :: Ptr ()
, onPlaylistImageChanged        :: Ptr ()
, onPlaylistTrackMessageChanged :: Ptr ()
, onPlaylistSubscribersChanged  :: Ptr ()
}

instance Storable PlaylistCallbacks where
  sizeOf _ = #size sp_playlist_callbacks
  alignment = sizeOf
  peek ptr =
    PlaylistCallbacks
      <$> (#peek sp_playlist_callbacks, tracks_added) ptr
      <*> (#peek sp_playlist_callbacks, tracks_removed) ptr
      <*> (#peek sp_playlist_callbacks, tracks_moved) ptr
      <*> (#peek sp_playlist_callbacks, playlist_renamed) ptr
      <*> (#peek sp_playlist_callbacks, playlist_state_changed) ptr
      <*> (#peek sp_playlist_callbacks, playlist_update_in_progress) ptr
      <*> (#peek sp_playlist_callbacks, playlist_metadata_updated) ptr
      <*> (#peek sp_playlist_callbacks, track_created_changed) ptr
      <*> (#peek sp_playlist_callbacks, track_seen_changed) ptr
      <*> (#peek sp_playlist_callbacks, description_changed) ptr
      <*> (#peek sp_playlist_callbacks, image_changed) ptr
      <*> (#peek sp_playlist_callbacks, track_message_changed) ptr
      <*> (#peek sp_playlist_callbacks, subscribers_changed) ptr
  poke ptr callbacks =
    do (#poke sp_playlist_callbacks, tracks_added) ptr
         (onPlaylistTracksAdded callbacks)
       (#poke sp_playlist_callbacks, tracks_removed) ptr
         (onPlaylistTracksRemoved callbacks)
       (#poke sp_playlist_callbacks, tracks_moved) ptr
         (onPlaylistTracksMoved callbacks)
       (#poke sp_playlist_callbacks, playlist_renamed) ptr
         (onPlaylistRenamed callbacks)
       (#poke sp_playlist_callbacks, playlist_state_changed) ptr
         (onPlaylistStateChanged callbacks)
       (#poke sp_playlist_callbacks, playlist_update_in_progress) ptr
         (onPlaylistUpdateInProgress callbacks)
       (#poke sp_playlist_callbacks, playlist_metadata_updated) ptr
         (onPlaylistMetadataUpdated callbacks)
       (#poke sp_playlist_callbacks, track_created_changed) ptr
         (onPlaylistTrackCreatedChanged callbacks)
       (#poke sp_playlist_callbacks, track_seen_changed) ptr
         (onPlaylistTrackSeenChanged callbacks)
       (#poke sp_playlist_callbacks, description_changed) ptr
         (onPlaylistDescriptionChanged callbacks)
       (#poke sp_playlist_callbacks, image_changed) ptr
         (onPlaylistImageChanged callbacks)
       (#poke sp_playlist_callbacks, track_message_changed) ptr
         (onPlaylistTrackMessageChanged callbacks)
       (#poke sp_playlist_callbacks, subscribers_changed) ptr
         (onPlaylistSubscribersChanged callbacks)

emptyPlaylistCallbacks = PlaylistCallbacks {
  onPlaylistTracksAdded         = nullPtr
, onPlaylistTracksRemoved       = nullPtr
, onPlaylistTracksMoved         = nullPtr
, onPlaylistRenamed             = nullPtr
, onPlaylistStateChanged        = nullFunPtr
, onPlaylistUpdateInProgress    = nullPtr
, onPlaylistMetadataUpdated     = nullPtr
, onPlaylistTrackCreatedChanged = nullPtr
, onPlaylistTrackSeenChanged    = nullPtr
, onPlaylistDescriptionChanged  = nullPtr
, onPlaylistImageChanged        = nullPtr
, onPlaylistTrackMessageChanged = nullPtr
, onPlaylistSubscribersChanged  = nullPtr
}
