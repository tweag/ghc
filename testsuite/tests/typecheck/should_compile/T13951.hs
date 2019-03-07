{-# LANGUAGE PolyKinds, Rank2Types #-} 
module T13951 where                                    
                                                                                
viewL :: forall k. Bool -> (forall x. k x -> Int) -> Int
viewL False _ = 0
viewL True  r = r undefined
