module Main where
import System.Random

-- funcion -> guess-> code
sc_black :: [(Int,Int)] -> [(Int,Int)] -> Int
sc_black [][] = 0
sc_black (x:xs) (y:ys) = if fst x == fst y then 1 + sc_black xs ys else sc_black xs ys


-- funcion -> black-> guess -> code
sc_white :: ( (Int,Int) -> [(Int,Int)]  -> Int ) -> Int -> [(Int,Int)] -> [(Int,Int)] -> Int
sc_white _ w [] _ = -w
sc_white func w (x:xs) y = func x y  + sc_white func w xs y

score :: Int -> Int -> [Int]
score a b = [a,b]

coinc :: (Int,Int) -> [(Int,Int)] -> Int
coinc _ [] = 0
coinc x (y:ys) = if fst x == fst y then 1 + coinc x ys else coinc x ys

del_N_elem :: Int -> [(Int,Int)] -> [(Int,Int)]
del_N_elem n xs =
				let 
					(ys,zs) = splitAt n xs   
				in  ys ++ (tail zs)

-- [Int] lista de aleatorios []	.... keep funcion de valores mantenido de CFG para generar...listas pa mantener			
keep :: Int -> [Int] -> [(Int,Int)] -> [(Int,Int)]
keep 0 _ _ = []
keep v (y:ys) xs = 
				 let 
					a = xs !! y
				 in [a] ++ keep (v-1) ys xs