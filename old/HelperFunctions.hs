module HelperFunctions where

unjust (Just x) = x
unjust _ = error "Unjust got nothing."

fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"