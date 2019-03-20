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
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
module Top where

import Button (button)

import Clash.Prelude
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

{-# ANN topEntity
  (Synthesize
    { t_name     = "calculator"
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
  -> Signal System (Bit, Bit, Bit, Bit, Bit)
topEntity clk pmod1 pmod2 pmod3 pmod4 =
  withClockReset clk (unsafeToAsyncReset (pure False)) $
    let
      toUpdate confirm push0 push1
        | confirm   = Just Confirm
        | push0     = Just (Push 0)
        | push1     = Just (Push 1)
        | otherwise = Nothing

      updates
        = toUpdate
            <$> isRising 0 (button clk pmod1)
            <*> isRising 0 (button clk pmod3)
            <*> isRising 0 (button clk pmod4)
    in
      fromOutput <$> calculator updates

  where
    fromOutput o =
      ( displayNumber o ! 0
      , displayNumber o ! 1
      , displayNumber o ! 2
      , displayNumber o ! 3
      , isResult o
      )

data Update
  = Confirm
  | Push Bit
  deriving (Show, Generic, NFData)

data Output
  = Output
      { displayNumber :: Unsigned 4
      , isResult :: Bit
      }
  deriving (Show, Generic, NFData)

calculator
  :: HiddenClockReset domain gated reset
  => Signal domain (Maybe Update)
  -> Signal domain Output

calculator =
  moore updateMaybe view initialState

  where
    updateMaybe state mU
      = case mU of
          Nothing -> state
          Just u  -> update u state

data State
  = State
      { currentStep :: Step
      , firstNumber :: Unsigned 4
      , secondNumber :: Unsigned 4
      }
  deriving (Show)

data Step
  = EnteringFirstNumber
  | EnteringSecondNumber
  | ShowingResult
  deriving (Show)

nextStep :: Step -> Step
nextStep EnteringFirstNumber = EnteringSecondNumber
nextStep EnteringSecondNumber = ShowingResult
nextStep ShowingResult = EnteringFirstNumber

initialState :: State
initialState
  = State
      { currentStep = EnteringFirstNumber
      , firstNumber = 0
      , secondNumber = 0
      }

view :: State -> Output
view State{currentStep, firstNumber, secondNumber}
  = Output {displayNumber, isResult}

  where
    displayNumber
      = case currentStep of
          EnteringFirstNumber -> firstNumber
          EnteringSecondNumber -> secondNumber
          ShowingResult -> firstNumber + secondNumber

    isResult
      = case currentStep of
          ShowingResult -> high
          _ -> low

update :: Update -> State -> State
update update state
  = case update of
      Confirm ->
        case currentStep state of
          ShowingResult ->
            initialState
          _ ->
            state {currentStep = nextStep (currentStep state)}

      Push b  ->
        case currentStep state of
          EnteringFirstNumber ->
            state {firstNumber = push b (firstNumber state)}
          EnteringSecondNumber ->
            state {secondNumber = push b (secondNumber state)}
          ShowingResult ->
            state

  where
    push b n = unpack (slice d2 d0 n ++# pack b)

main :: IO ()
main = print "hello world"
