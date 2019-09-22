{-# LANGUAGE OverloadedStrings #-}

module Civet.Store where

import Civet.Model
import qualified Civet.Utils as Utils
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import Debug.Trace

maxBins :: Int
maxBins = 2048

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

getKey :: Double -> DData -> Int
getKey value env
  | value < -Utils.minValue = -(ceiling ( log (-value) / Utils.gammaLn ) ) - Utils.offset
  | value > Utils.minValue = (ceiling $ ((log value) / Utils.gammaLn) ) + Utils.offset 
  | otherwise = 0


growLeft :: Int -> DData -> DData
growLeft key env
  | (dMinKey env) < key || Seq.length (dBins env) >= maxBins = let 
    fuck = trace ("foo = ") $ env
    in env
  | otherwise = let 
      minKey = if (dMaxKey env) - key >= maxBins then (dMaxKey env) - key + 1 else 1 -- this is incomplete
      foo = trace ("foo = " ++ show minKey) $ minKey 
      -- prepend the zeros to the beginning
      array = Seq.fromList [0 | x <- [0..(minKey - key)]]
      newEnv = env { dBins = array <> (dBins env) }
  in newEnv

growRight :: Int -> DData -> DData
growRight key env 
  | (dMaxKey env) > key              = trace "not growingRight" $ env
  | (key - (dMaxKey env)) >= maxBins = trace ("c1: growingRight by " <> show maxBins) $ env
  | (key - (dMinKey env)) >= maxBins = trace ("c2: growingRight by " <> show (key - (dMaxKey env))) $ env
  | otherwise                        = trace ("c3: growingRight by " <> show (key - (dMaxKey env))) $ env { 
    dMaxKey = key, 
    dBins = (dBins env) <> Seq.fromList [0 | x <- [0..(key - (dMaxKey env))]]
    }

growAndSetRange :: Int -> DData -> DData
growAndSetRange key env
  | (dCount env) == 0 = let 
    minKey = trace "setting min and max" $ key - Seq.length (dBins env) + 1
    newEnv = env {
      dMaxKey = key, 
      dMinKey = minKey
    }
    in newEnv
  | key < (dMinKey env) = 
    growLeft key env
  | key > (dMaxKey env) =
    growRight key env

-- External API
add :: Double -> DData -> DData
add value env = 
  let
    rangeEnv = growAndSetRange (getKey value env) env
    updateIndex = traceThis ((index . key $ rangeEnv) rangeEnv)
    -- ((index . (getKey value) $ rangeEnv) rangeEnv)
    newEnv = rangeEnv { 
      dBins = (Seq.adjust' (+1) updateIndex (dBins rangeEnv)),
      dCount = (dCount rangeEnv) + 1
    }
  in newEnv
  where
    key env = traceThis $ getKey value env
    index val env = traceThis $ max 0 ((getKey value env) - (dMinKey env)) :: Int

keyAtRank :: Double -> DData -> Int
keyAtRank value env = atRank value 0 0 (dMinKey env) (dMaxKey env) (dBins env)

-- Internal function
atRank :: Double -> Int -> Int -> Int -> Int -> Seq Integer -> Int
atRank rank n idx minKey maxKey Empty = maxKey
atRank rank n idx minKey maxKey (x :<| xs)
  | fromIntegral n >= rank = idx + minKey
  | otherwise              = atRank rank (n + fromIntegral x) (idx + 1) minKey maxKey xs

-- External function
quantile :: Double -> DData -> Double
quantile q env
  | q < 0 || q > 1 || (dCount env) == 0 = 0
  | q == 0                              = (dMin env)
  | q == 1                              = (dMax env)
  | otherwise                           = let 
    rank     = (q * fromIntegral ((dCount env) - 1))
    key      = keyAtRank rank env
    quan     = if key < 0 then 
      (-2.0 * (Utils.gamma ** (-fromIntegral (key + Utils.offset))) / (1.0 + Utils.gamma))
    else 
       (2.0 * (Utils.gamma ** fromIntegral (key - Utils.offset)) / (1 + Utils.gamma))
    maxValue = max quan (dMin env)
    in maxValue
