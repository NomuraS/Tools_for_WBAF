module WBAF where 
import Data.List
import Utility 
-- import Input_WBAF
import Data.Ratio
import Data.Tree
import System.IO
import System.Process
import System.IO
import System.Process
-- import Debug.Trace
-- import Data.Char
-- import Data.String
-- import Network.HTTP.Client
-- import Algebra.Graph.Labelled
-- import Data.EdgeTree
-- import Data.LabeledTree
-- import Text.Parsec
-- import Text.Parsec.String

------------

type Nodes a= [Tree a] 
type Arg a = a 
type ATT a= [(Arg a,Arg a)]
type SUP a= [([Arg a],Arg a)]
type NormalRel a= [(Arg a,Arg a)]
type Weight a=(Arg a->Float)
type ArgPATH a= [a]

data WBAF a = WBAF [Arg a] (ATT a) (SUP a) (Weight a)
  -- deriving (Eq,Show)

-- instance (Eq a)=> Show (Arg a) where
--   show  ar = 
    -- let
    --   ff x = intercalate "" $nub$sort$concat x 
    -- in
    --       "mAR : "++ (unwords$ map ff a ) ++",\n" 


instance Show (a -> b) where
  show _  =  "<function>"

instance Eq (a -> b) where
   _ ==_  =  1==1

    -- let
    --   ff x = intercalate "" $nub$sort$concat x 
    -- in
    --       "AR : "++ show a ++",\n"++ 
    --       "ATT : "++ show b++",\n"++ 
    --       "SUP : "++ show s++",\n"++
    --       "Weight :" ++ unwords(["w(" ++ x ++")=" ++ show ( w x) ++"\n"| x<-a])

-- instance (Eq a, Show a)=> Show (WBAF a) where
--   show (WBAF ar tt sup wei)  = 
--     let
--       ff x = intercalate "" $nub$sort$concat x 
--     in
--           "AR : "++ (unwords$ map ff ar ) ++",\n" 
--           ++"ATT : "++ unwords [ "("++ff x++","++ff y ++")" |(x,y)<-tt] ++",\n"
--           ++"SUP : "++ show sup++",\n"
--           ++"Weight :\n " ++ (unwords [ "w(" ++ ( ff x) ++")=" ++show (wei x)++"\n"| x<-ar])

instance (Eq a, Show a)=> Show (WBAF a) where
  show (WBAF ar tt sup wei)  = 
    let
      ff x = intercalate "" $nub$sort$concat x 
    in
            "AR : "++ (show  ar) ++",\n " 
          ++"ATT : "++ show tt ++",\n "
          -- ++"ATT : "++ unwords ["("++ show x++","++ show y ++")" |(x,y)<-tt] ++",\n "
          ++"SUP : "++ show sup++",\n "
          ++"Weight :"++ " \n " ++ (unwords [ "w(" ++ (show  x) ++")=" ++ show (wei x)++" \n "| x<-ar])

