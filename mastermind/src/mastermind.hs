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


mastermind :: [(Int,Int)] -> IO()
mastermind code = do
	guess <- getLine
	respond (sc_black ((converter guess)) (code)) (sc_white (coinc) ( sc_black ((converter guess)) (code)) ((converter guess) ) (code)) 1

-- pegs black -> pegs white -> opportunitties
respond :: Int -> Int -> Int -> IO ()
respond guess n _| guess < n = do
    putStrLn "Too low!"
    mastermind n
respond guess n _| guess > n = do
    putStrLn "Too high!"
    mastermind n
respond _ _ _ | otherwise = putStrLn "Just right!"
			
guess:: IO() = do
	linea <- getLine
			
converter :: String -> [(Int,Int)]
converter [] = []
converter (x:xs) = let
					y = getInt x
					a = (y,0)
					in [a] ++ converter xs

getInt:: Char -> Int
getInt c
	| c == '1' = 1
	| c == '2' = 2
	| c == '3' = 3
	| c == '4' = 4
	| c == '5' = 5
	| c == '6' = 6
					
main = do
		let 
			pool = [1,2,3,4,5,6]
			poblacion = [ [(a,0), (b,1), (c,2), (d,3) ] | a<-pool, b<-pool, c<-pool, d<-pool ]
		print $ show(length(poblacion));
		target <- randomRIO(0,200)
		let code = poblacion !! target
		
		let 
			black = sc_black [(1,0),(5,1),(3,2),(6,3)] [(1,0),(5,1),(3,2),(6,3)]
			white = sc_white coinc black [(1,0),(5,1),(3,2),(6,3)] [(1,0),(5,1),(3,2),(6,3)]
			sc = score black white
			l_keep = keep 3 [1,0,2,3] [(1,0),(5,1),(3,2),(6,3)]
			npg = n_pot_guess [1,2] [(1,0),(2,1),(3,2),(3,3)]
		print code
		print npg
		print $ show( black );
		print $ show( white );
		
		linea <- getLine
		let adiv = converter linea
		print adiv