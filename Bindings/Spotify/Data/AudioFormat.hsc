module Bindings.Spotify.Data.AudioFormat (
  AudioFormat(..),
  Sample,
  SampleType(..),
  signedInt16
) where

import Control.Applicative

import Foreign
import Foreign.C.Types

#include <libspotify/api.h>

type Sample = Word8

data AudioFormat = AudioFormat {
  sampleType :: SampleType,
  sampleRate :: CInt,
  channels   :: CInt
}

instance Storable AudioFormat where
  sizeOf _  = #size sp_audioformat
  alignment = sizeOf 
  peek ptr  =
    AudioFormat
      <$> (#peek sp_audioformat, sample_type) ptr
      <*> (#peek sp_audioformat, sample_rate) ptr
      <*> (#peek sp_audioformat, channels) ptr
  poke ptr format =
    do (#poke sp_audioformat, sample_type) ptr (sampleType format)
       (#poke sp_audioformat, sample_rate) ptr (sampleRate format)
       (#poke sp_audioformat, channels) ptr (channels format)

newtype SampleType = SampleType CInt
 deriving (Show, Eq, Storable)

#{enum SampleType, SampleType
, signedInt16 = SP_SAMPLETYPE_INT16_NATIVE_ENDIAN 
}
