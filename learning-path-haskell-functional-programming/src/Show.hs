module Show where
  
data Quaternion = Q {
  qR :: Double,
  qI :: Double,
  qJ :: Double,
  qK :: Double
}

-- This custom type class instance will give:
-- (1.0 + 2.0i + 3.0j + 4.0k)
-- whereas a default of "deriving Show" would give:
-- Q {qR = 1.0, qI = 2.0, qJ = 3.0, qK = 4.0}
instance Show Quaternion where
  show q = "(" ++
    show (qR q) ++ " + " ++
    show (qI q) ++ "i + " ++
    show (qJ q) ++ "j + " ++
    show (qK q) ++ "k)"

instance Num Quaternion where
  q0 + q1 = Q (qR q0 + qR q1) (qI q0 + qI q1) (qJ q0 + qJ q1) (qK q0 + qK q1)
  q0 * q1 = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

newtype PrettyQuaternion = PrettyQuaternion {
  unPrettyQuaternion :: Quaternion
}

instance Show PrettyQuaternion where
  show q =
    let q' = unPrettyQuaternion q in
     "(" ++
      show (qR q') ++ " + " ++
      show (qI q') ++ "i + " ++
      show (qJ q') ++ "j + " ++
      show (qK q') ++ "k)"

newtype UglyQuaternion = UglyQuaternion {
  unUglyQuaternion :: Quaternion
}

instance Show UglyQuaternion where
  show q =
    let q' = unUglyQuaternion q in
      show (qR q') ++ ", " ++
      show (qI q') ++ ", " ++
      show (qJ q') ++ ", " ++
      show (qK q')

main :: IO ()
main = do
  print $ Q 1 2 3 4
  print $ Q 1 2 3 4 + Q 10 20 30 40
  print $ PrettyQuaternion (Q 1 2 3 4)
  print $ UglyQuaternion (Q 1 2 3 4)