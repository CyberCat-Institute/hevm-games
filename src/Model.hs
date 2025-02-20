{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Model
  where

import ActionSpaces
import Components
import SupportFunctions
import Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

