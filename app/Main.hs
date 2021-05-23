{-# LANGUAGE OverloadedStrings #-}

module Main where

import LiveView.Examples.Carcassonne qualified as App (main)
import LiveView.Examples.Calculator qualified as Calc (main)

main :: IO ()
main = Calc.main
