module Data.Euclideo where


class Num a => Euclideo a where
   cocienteResto :: a -> a -> (a,a)
   cociente :: a -> a -> a
   resto    :: a -> a -> a
   
   cociente x y = q where (q,_) = cocienteResto x y
   resto    x y = r where (_,r) = cocienteResto x y


instance Euclideo Int where
   cocienteResto = divMod