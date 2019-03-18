{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes #-}
module Top where

import Clash.Prelude
import Control.Lens as Lens (Lens, Lens', over)

{-# ANN topEntity
  (Synthesize
    { t_name     = "blank"
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
  -> Bit -> Bit -> Bit -> Bit
  -> (Bit, Bit, Bit, Bit, Bit)
topEntity clk p1 p2 p3 p4 =
  (p1, p2, p3, p4, high)

main :: IO ()
main = print "hello world"
