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


--- ....shift ... lista para combinar				 
shift :: Int -> [Int] -> [(Int,Int)] -> [(Int,Int)]
shift 0 _ _ = []
shift v (y:ys) xs = 
				 let 
					a = xs !! y
				 in [a] ++ shift (v-1) ys xs

-- score -> cfg -> npg				 
n_pot_guess :: [Int] -> [(Int,Int)] -> [(Int,Int)]
n_pot_guess sc cfg =
					let
						black = sc !! 0
						white = sc !! 1
						list_keep = keep black [1,0,2,3] cfg
						list_shift = shift white [0,3,2,1] cfg	
						npg = sort_NPG list_keep list_shift
					in npg
				 
-- lista de keep -> lista shift -> lista ordenado
sort_NPG :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
sort_NPG k s
	
	| length(k) == 1 && length(s)==2 = let 
										a = s!!1
										b = k!!0
										c = s!!0
										d = (4,3)
										list@[x,y,z,xs]=[ a, b, c, d]
										in list
