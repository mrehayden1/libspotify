module Bindings.Spotify.Data.SessionCallbacks (
  LoginCallback,
  MusicDeliveryCallback,
  SessionCallbacks(..),
  emptySessionCallbacks
) where

import Control.Applicative

import Foreign
import Foreign.C.Types

import Bindings.Spotify.Error (Error)
import Bindings.Spotify.Data.AudioFormat (AudioFormat, Sample)
import Bindings.Spotify.Data.Types (Session)

#include <libspotify/api.h>

type LoginCallback = Session -> Error -> IO ()

type SamplesAvailable = CInt
type SamplesConsumed = CInt

type MusicDeliveryCallback
  =  Ptr Session
  -> Ptr AudioFormat
  -> Ptr Sample
  -> SamplesAvailable
  -> IO SamplesConsumed

data SessionCallbacks = SessionCallbacks {
  onLoggedIn                  :: FunPtr LoginCallback
, onLoggedOut                 :: FunPtr ()
, onMetadataUpdated           :: FunPtr ()
, onConnectionError           :: FunPtr ()
, onMessageToUser             :: FunPtr ()
, onNotifyMainThread          :: FunPtr ()
, onMusicDelivery             :: FunPtr MusicDeliveryCallback
, onPlayTokenLost             :: FunPtr ()
, onLogMessage                :: FunPtr ()
, onEndOfTrack                :: FunPtr ()
, onStreamingError            :: FunPtr ()
, onUserInfoUpdated           :: FunPtr ()
, onStartPlayback             :: FunPtr ()
, onStopPlayback              :: FunPtr ()
, onGetAudioBufferStats       :: FunPtr ()
, onOfflineStatusUpdated      :: FunPtr ()
, onOfflineError              :: FunPtr ()
, onCredentialsBlobUpdated    :: FunPtr ()
, onConnectionStateUpdated    :: FunPtr ()
, onScrobbleError             :: FunPtr ()
, onPrivateSessionModeChanged :: FunPtr ()
}

instance Storable SessionCallbacks where
  sizeOf _ = #size sp_session_callbacks
  alignment = sizeOf
  peek ptr = 
    SessionCallbacks
      <$> (#peek sp_session_callbacks, logged_in) ptr
      <*> (#peek sp_session_callbacks, logged_out) ptr
      <*> (#peek sp_session_callbacks, metadata_updated) ptr
      <*> (#peek sp_session_callbacks, connection_error) ptr
      <*> (#peek sp_session_callbacks, message_to_user) ptr
      <*> (#peek sp_session_callbacks, notify_main_thread) ptr
      <*> (#peek sp_session_callbacks, music_delivery) ptr
      <*> (#peek sp_session_callbacks, play_token_lost) ptr
      <*> (#peek sp_session_callbacks, log_message) ptr
      <*> (#peek sp_session_callbacks, end_of_track) ptr
      <*> (#peek sp_session_callbacks, streaming_error) ptr
      <*> (#peek sp_session_callbacks, userinfo_updated) ptr
      <*> (#peek sp_session_callbacks, start_playback) ptr
      <*> (#peek sp_session_callbacks, stop_playback) ptr
      <*> (#peek sp_session_callbacks, get_audio_buffer_stats) ptr
      <*> (#peek sp_session_callbacks, offline_status_updated) ptr
      <*> (#peek sp_session_callbacks, offline_error) ptr
      <*> (#peek sp_session_callbacks, credentials_blob_updated) ptr
      <*> (#peek sp_session_callbacks, connectionstate_updated) ptr
      <*> (#peek sp_session_callbacks, scrobble_error) ptr
      <*> (#peek sp_session_callbacks, private_session_mode_changed) ptr
  poke ptr callbacks =
    do (#poke sp_session_callbacks, logged_in) ptr (onLoggedIn callbacks)
       (#poke sp_session_callbacks, logged_out) ptr (onLoggedOut callbacks)
       (#poke sp_session_callbacks, metadata_updated) ptr
         (onMetadataUpdated callbacks)
       (#poke sp_session_callbacks, connection_error) ptr
         (onConnectionError callbacks)
       (#poke sp_session_callbacks, message_to_user) ptr
         (onMessageToUser callbacks)
       (#poke sp_session_callbacks, notify_main_thread) ptr
         (onNotifyMainThread callbacks)
       (#poke sp_session_callbacks, music_delivery) ptr
         (onMusicDelivery callbacks)
       (#poke sp_session_callbacks, play_token_lost) ptr
         (onPlayTokenLost callbacks)
       (#poke sp_session_callbacks, log_message) ptr (onLogMessage callbacks)
       (#poke sp_session_callbacks, end_of_track) ptr (onEndOfTrack callbacks)
       (#poke sp_session_callbacks, streaming_error) ptr
         (onStreamingError callbacks)
       (#poke sp_session_callbacks, userinfo_updated) ptr
         (onUserInfoUpdated callbacks)
       (#poke sp_session_callbacks, start_playback) ptr
         (onStartPlayback callbacks)
       (#poke sp_session_callbacks, stop_playback) ptr (onStopPlayback callbacks)
       (#poke sp_session_callbacks, get_audio_buffer_stats) ptr
         (onGetAudioBufferStats callbacks)
       (#poke sp_session_callbacks, offline_status_updated) ptr
         (onOfflineStatusUpdated callbacks)
       (#poke sp_session_callbacks, offline_error) ptr
         (onOfflineError callbacks)
       (#poke sp_session_callbacks, credentials_blob_updated) ptr
         (onCredentialsBlobUpdated callbacks)
       (#poke sp_session_callbacks, connectionstate_updated) ptr
         (onConnectionStateUpdated callbacks)
       (#poke sp_session_callbacks, scrobble_error) ptr
         (onScrobbleError callbacks)
       (#poke sp_session_callbacks, private_session_mode_changed) ptr
         (onPrivateSessionModeChanged callbacks)

emptySessionCallbacks :: SessionCallbacks
emptySessionCallbacks = SessionCallbacks {
  onLoggedIn = nullFunPtr 
, onLoggedOut = nullFunPtr
, onMetadataUpdated = nullFunPtr
, onConnectionError = nullFunPtr
, onMessageToUser = nullFunPtr
, onNotifyMainThread = nullFunPtr
, onMusicDelivery = nullFunPtr
, onPlayTokenLost = nullFunPtr
, onLogMessage = nullFunPtr
, onEndOfTrack = nullFunPtr
, onStreamingError = nullFunPtr
, onUserInfoUpdated = nullFunPtr
, onStartPlayback = nullFunPtr
, onStopPlayback = nullFunPtr
, onGetAudioBufferStats = nullFunPtr
, onOfflineStatusUpdated = nullFunPtr
, onOfflineError = nullFunPtr
, onCredentialsBlobUpdated = nullFunPtr
, onConnectionStateUpdated = nullFunPtr
, onScrobbleError = nullFunPtr
, onPrivateSessionModeChanged = nullFunPtr
}
