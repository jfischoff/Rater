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
    deriving(Show, Eq, Typeable, Ord)
    
instance Term Thing
    
data TV = X | Y | Z deriving (Eq,Ord,Show,Typeable)
instance Term TV where type Entity TV = Thing; term = var
    
data Battle = Battle {
        left   :: Thing,
        right  :: Thing,
        nothing :: ()
    }    
    deriving(Show, Eq, Typeable, Ord)
    
-- try to infer a total order from a partial order
-- find the complete ranking from the pair wise rankings

things  :: T1 String String () 
things  = t1 (Table 0 const) (\x () -> x :: String)

battles :: T2 Battle Thing Thing ()
battles = t2 (Table 1 const) Battle



  
endpoint :: T1 Thing Thing ()
endpoint = t1 (Table 3 const) (\x () -> x)  

tc :: T2 Battle Thing Thing ()
tc   = t2 (Table 4 const) Battle
 
--test :: Monad m => DatalogT m (Logic (String,String, Hand))
test = do  
    T1 cyclic <- table (\x () -> x :: Thing) const
    T1 acyclic <- table (\x () -> x :: Thing) const
     
    battles (Thing "sex") (Thing "beer")
    
    tc X Y :- battles X Y
    tc X Z :- tc X Y <* battles Y Z
    
    cyclic X :- tc X X
    acyclic X :- (battles X Y <|> battles Y X) <* no (cyclic X)
    endpoint X :- acyclic X
    
    query $ row (tc (Thing "sex") X) <* no (battles X (Thing "beer"))
  

--test' :: [(String, String, Hand)]
test' = Data.Foldable.toList $ evalState ?? Env 2 mempty mempty $ 
            stratified $ test    
    

    
    
    
    
    
    
    
    
    
    
 