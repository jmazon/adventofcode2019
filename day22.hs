-- Day 22: Slam Shuffle
{-# LANGUAGE DataKinds,TypeOperators #-}

import GHC.TypeLits (KnownNat)
import Data.Semigroup
import Data.Modular

data Affine n = Affine !n !n
instance Num n => Semigroup (Affine n) where
  (Affine a1 b1) <> (Affine a2 b2) = Affine (a1*a2) (a2*b1+b2)
instance Num n => Monoid (Affine n) where mempty = Affine 1 0

apply :: Num n => Affine n -> n -> n
apply (Affine a b) n = a*n + b

inverse :: (Integral i,KnownNat n) => Affine (Mod i n) -> Affine (Mod i n)
inverse (Affine a b) = Affine (inv a) (-b * inv a)

main :: IO ()
main = do
  shuffle <- map words . lines <$> getContents

  let shuffle1 = foldMap technique shuffle
  print (apply shuffle1 2019 :: ℤ/10007)

  let shuffle2 = inverse (foldMap technique shuffle)
  print (apply (stimes 101741582076661 shuffle2) 2020 :: ℤ/119315717514047)

technique :: (Read i,Integral i,KnownNat n) => [String] -> Affine (Mod i n)
technique [_,"into",_, _] = Affine (-1) (-1)
technique ["cut",     _d] = Affine 1 (-read _d)
technique [_,"with",_,_d] = Affine (read _d) 0
technique        ws       = error $ "Unknown technique: " ++ unwords ws
