{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Clash.Prelude
import Control.Lens (Iso', iso, over)

{-# ANN topEntity
  (Synthesize
    { t_name     = "pwm"
    , t_inputs   =
        [ PortName "clk"
        , PortName "pmod1"
        , PortName "pmod2"
        , PortName "pmod3"
        , PortName "pmod4"
        ]
    , t_output   = PortProduct "out"
        [ PortName "led1"
        , PortName "led2"
        , PortName "led3"
        , PortName "led4"
        , PortName "led5"
        ]
    }) #-}
topEntity
  :: Clock System Source
  -> Bit -> Bit -> Bit
  -> Signal System (Bit, Bit, Bit, Bit, Bit)
topEntity clk _ _ _ =
  withClockReset clk (unsafeToSyncReset (pure False)) $
    leds <$> counter

  where
    counter = register (0 :: Unsigned 32) (counter + 1)

    leds c =
      ( pwm (  abs' (slice d22 d14 c)) (slice d7 d0 c)
      , pwm (  abs' (slice d24 d16 c)) (slice d7 d0 c)
      , pwm (- abs' (slice d22 d14 c)) (slice d7 d0 c)
      , pwm (- abs' (slice d24 d16 c)) (slice d7 d0 c)
      , low
      )

    abs' = slice d7 d0 . over unpacked (abs @(Signed 9))

pwm :: BitVector m -> BitVector m -> Bit
pwm treshold cnt = boolToBit $ cnt > treshold

unpacked :: BitPack a => Iso' (BitVector (BitSize a)) a
unpacked = iso unpack pack


main :: IO ()
main = print "hello world"
