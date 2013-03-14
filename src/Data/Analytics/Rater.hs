{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeSynonymInstances, FlexibleInstances #-}
module Data.Analytics.Rater where

import Control.Applicative
import Control.Monad.Logic
import Data.Analytics.Datalog
import Data.Text
import Data.Typeable
    
data Thing = Thing {
        name     :: String
    }    
    
data Battle = Battle {
        left   :: Thing,
        right  :: Thing,
        winner :: Bool
    }    
    
-- try to infer a total order from a partial order
-- find the complete ranking from the pair wise rankings

things  :: T1 String String () 
battles :: T3 (String, String, Bool) String String Bool ()
things  = t1 (Table 0 const) (\x () -> x :: String)
battles = t3 (Table 1 const) (\x y z () -> (x :: String, y :: String, z::Bool))

instance Term String
data StringVar = P | Q | R | S | T | U | V deriving (Eq,Ord,Show,Typeable)
instance Term StringVar where
  type Entity StringVar = String
  term = var
  
 