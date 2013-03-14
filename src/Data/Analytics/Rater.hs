{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeSynonymInstances, FlexibleInstances #-}
module Data.Analytics.Rater where
import Data.Monoid
import Data.Foldable
import Control.Applicative
import Control.Monad.Logic
import Data.Analytics.Datalog
import Data.Text
import Data.Typeable
import Data.Data
import Data.Analytics.Datalog.Evaluation.Naive
import Control.Monad.State
import Control.Lens
    
data Thing = Thing {
        name     :: String
    }    
    
data Hand = L | R deriving(Show, Eq, Typeable, Data, Ord)
    
data Battle = Battle {
        left   :: Thing,
        right  :: Thing,
        winner :: Hand
    }    
    
-- try to infer a total order from a partial order
-- find the complete ranking from the pair wise rankings

things  :: T1 String String () 
battles :: T3 (String, String, Hand) String String Hand ()
things  = t1 (Table 0 const) (\x () -> x :: String)
battles = t3 (Table 1 const) (\x y z () -> (x :: String, y :: String, z::Hand))

instance Term String
data StringVar = P | Q | S | T | U | V deriving (Eq,Ord,Show,Typeable)

instance Term StringVar where
  type Entity StringVar = String
  term = var

data HandVar = W deriving (Eq,Ord,Show,Typeable) 
 
instance Term Hand
instance Term HandVar where
  type Entity HandVar = Hand
  term = var 
 
test :: Monad m => DatalogT m (Logic (String,String, Hand))
test = do   
    things "pizza"
    things "beer"
    things "sex"
    things "drugs"
    things "rock"
    things "roll"
    
    battles "pizza" "beer" L
    battles "beer" "sex"   R
    battles "drugs" "beer" R
    battles "rock" "beer"  L
    battles "roll" "rock"  R
    battles "drugs" "rock" R
    
    query $ row (battles U "beer" W)
  
  
  
    
test' :: [(String, String, Hand)]
test' = Data.Foldable.toList $ evalState ?? Env 2 mempty mempty $ 
            stratified $ test    
    

    
    
    
    
    
    
    
    
    
    
 