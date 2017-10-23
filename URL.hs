module URL
  ( module Export
  ) where

import URL.Core as Export

import Data.Maybe

localhost :: URL
localhost = URL (Proto "http") auth (Path []) (Params []) (Fragment "")
  where
    auth = Authority Nothing (Domain "localhost") 80
