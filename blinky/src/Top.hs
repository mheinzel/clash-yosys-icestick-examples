{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes #-}
module Top where

import Clash.Prelude

{-# ANN topEntity
  (Synthesize
    { t_name     = "blinky"
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
    counter = register (0 :: Unsigned 32) ((+1) <$> counter)

    leds c =
      ( c ! 24
      , c ! 23
      , c ! 22
      , c ! 21
      , c ! 20
      )

main :: IO ()
main = print "hello world"
