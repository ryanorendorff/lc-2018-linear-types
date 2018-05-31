module Num (
    (+.),
    (-.),
    (*.),
    (/.),
) where

import Unsafe.Linear as UL

------------------------------------------------------------------------
--                    Poor man's linear Num class functions           --
------------------------------------------------------------------------

(+.) :: Num a => a ->. a ->. a
(+.) = UL.toLinear2 (+)

(-.) :: Num a => a ->. a ->. a
(-.) = UL.toLinear2 (-)

(*.) :: Num a => a ->. a ->. a
(*.) = UL.toLinear2 (*)

(/.) :: Fractional a => a ->. a ->. a
(/.) = UL.toLinear2 (/)

infixl 6 +.
infixl 6 -.
infixl 7 *.
infixl 7 /.

