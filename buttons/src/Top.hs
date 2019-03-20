{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TupleSections              #-}
module Top where

import Button

import Clash.Prelude

{-# ANN topEntity
  (Synthesize
    { t_name     = "buttons"
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
  -> Signal System (Bit, Bit, Bit, Bit)
topEntity clk button1 button2 button3 button4 =
  (,,,)
    <$> button clk button1
    <*> button clk button2
    <*> button clk button3
    <*> button clk button4

main :: IO ()
main = print "hello world"
