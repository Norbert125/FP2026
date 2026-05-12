import Data.List              (sortBy, nub)
import Data.Ord               (comparing)
import Data.Char              (toLower)
import Control.Monad          (when)
import qualified Data.ByteString.Char8 as BS

-- ===== Segédfüggvény =====

splitOn :: Char -> String -> [String]
splitOn _ ""  = [""]
splitOn c (x:xs)
  | x == c    = "" : rest
  | otherwise = (x : head rest) : tail rest
  where rest = splitOn c xs

-- ===== I. Feladat: Betegek =====

data Beteg = Beteg
  { bNev   :: String
  , bVerny :: [(Int, Int)]
  , bSzEv  :: Int
  } deriving (Show)

parsePar :: String -> (Int, Int)
parsePar s = let [a, b] = splitOn '/' s in (read a, read b)

parseBeteg :: String -> Beteg
parseBeteg line =
  let [nev, evStr, vStr] = splitOn ';' line
  in Beteg nev (map parsePar (splitOn ',' vStr)) (read evStr)

rendezBetegek :: [Beteg] -> [Beteg]
rendezBetegek = sortBy (comparing (map toLower . bNev))

binarisKeres :: [Beteg] -> String -> Maybe Beteg
binarisKeres [] _ = Nothing
binarisKeres xs nev =
  let mid = length xs `div` 2
      b   = xs !! mid
  in case compare (map toLower nev) (map toLower (bNev b)) of
       EQ -> Just b
       LT -> binarisKeres (take mid xs) nev
       GT -> binarisKeres (drop (mid + 1) xs) nev

magasVp :: (Int, Int) -> Bool
magasVp (s, d) = s > 160 || d > 140

magasVpDb :: Beteg -> Int
magasVpDb = length . filter magasVp . bVerny

-- ===== II. Feladat: Filmek (BST) =====

data Film = Film
  { fcim     :: String
  , fRendezo :: String
  , fEv      :: Int
  , fKoltseg :: Int
  } deriving (Show)

data BST a = Ures | Csomp a (BST a) (BST a)

-- egyenlő kulcsok jobbra kerülnek (duplikátumok kezelése)
bstBeszur :: Ord b => (a -> b) -> a -> BST a -> BST a
bstBeszur _     x Ures = Csomp x Ures Ures
bstBeszur kulcs x (Csomp y bal jobb)
  | kulcs x < kulcs y = Csomp y (bstBeszur kulcs x bal) jobb
  | otherwise         = Csomp y bal (bstBeszur kulcs x jobb)

bstKeres :: Ord b => (a -> b) -> b -> BST a -> [a]
bstKeres _     _ Ures = []
bstKeres kulcs v (Csomp x bal jobb)
  | v < kulcs x = bstKeres kulcs v bal
  | v > kulcs x = bstKeres kulcs v jobb
  | otherwise   = x : bstKeres kulcs v jobb  -- duplikátumok jobbra vannak

bstInorder :: BST a -> [a]
bstInorder Ures = []
bstInorder (Csomp x bal jobb) = bstInorder bal ++ [x] ++ bstInorder jobb

parseFilm :: String -> Film
parseFilm line =
  let [c, r, ev, k] = splitOn ';' line
  in Film c r (read ev) (read k)

maxKoltsegu :: [Film] -> [Film]
maxKoltsegu [] = []
maxKoltsegu fs = filter ((== maxK) . fKoltseg) fs
  where maxK = maximum (map fKoltseg fs)

-- ===== III. Feladat: Filmek (ByteString) =====

data Film3 = Film3
  { f3Ev        :: BS.ByteString
  , f3Cim       :: BS.ByteString
  , f3Hossz     :: BS.ByteString
  , f3Tipus     :: BS.ByteString
  , f3Nepsz     :: BS.ByteString
  , f3Dijazott  :: BS.ByteString
  , f3Szinesz   :: BS.ByteString
  , f3Szineszno :: BS.ByteString
  , f3Rendezo   :: BS.ByteString
  } deriving (Show)

parseFilm3 :: BS.ByteString -> Film3
parseFilm3 line =
  let ps    = BS.split '\t' line
      get i = if i < length ps then ps !! i else BS.pack "Unknown"
  in Film3 (get 0) (get 1) (get 2) (get 3) (get 4) (get 5) (get 6) (get 7) (get 8)

ugyanabbanAzEvben :: Film3 -> Film3 -> Bool
ugyanabbanAzEvben a b = f3Ev a == f3Ev b

evCsoportok :: [Film3] -> [[Film3]]
evCsoportok fs = map csop (nub (map f3Ev fs))
  where csop ev = filter ((== ev) . f3Ev) fs

szineszFilmjei :: BS.ByteString -> [Film3] -> [BS.ByteString]
szineszFilmjei nev = map f3Cim . filter ((== nev) . f3Szinesz)

-- ===== Main =====

main :: IO ()
main = do
  -- I. Betegek
  putStrLn "=== I. Feladat: Betegek ==="
  betegSor <- readFile "betegek.txt"
  let betegek   = map parseBeteg . filter (not . null) . lines $ betegSor
      rendezett = rendezBetegek betegek
  putStrLn "Rendezett betegek:"
  mapM_ (putStrLn . bNev) rendezett
  let kerNev = "Kovacs Janos"
  case binarisKeres rendezett kerNev of
    Nothing -> putStrLn $ kerNev ++ " nem talalhato."
    Just b  -> do
      putStrLn $ "\nBeteg: " ++ bNev b ++ ", szuletesi ev: " ++ show (bSzEv b)
      putStrLn $ "Vernyomas ertekek: " ++ show (bVerny b)
      putStrLn $ "Magas vernyomas szama: " ++ show (magasVpDb b)

  -- II. Filmek BST
  putStrLn "\n=== II. Feladat: Filmek (BST) ==="
  filmSor <- readFile "filmek.txt"
  let filmek = map parseFilm . filter (not . null) . lines $ filmSor
      filmFa = foldl (flip (bstBeszur fEv)) Ures filmek
  let ev = 2010
  putStrLn $ show ev ++ "-ban megjelent filmek:"
  mapM_ (putStrLn . fcim) (bstKeres fEv ev filmFa)
  putStrLn "\nInorder bejarat (ev szerint novekvo):"
  mapM_ (\f -> putStrLn $ fcim f ++ " (" ++ show (fEv f) ++ ")") (bstInorder filmFa)
  putStrLn "\nLegnagyobb koltsegu film(ek):"
  mapM_ (putStrLn . fcim) (maxKoltsegu filmek)

  -- III. Filmek ByteString
  putStrLn "\n=== III. Feladat: Filmek (ByteString) ==="
  tartalom <- BS.readFile "film.txt"
  let film3ok = map parseFilm3 . filter (not . BS.null) . BS.lines $ tartalom
  when (length film3ok >= 2) $ do
    let (f1, f2) = (head film3ok, film3ok !! 1)
    putStrLn $ "Az 1. es 2. film ugyanabban az evben keszult: "
             ++ show (ugyanabbanAzEvben f1 f2)
  let rendFilmek = sortBy (comparing f3Ev) film3ok
  BS.writeFile "rendFilm.txt" . BS.unlines . map f3Cim $ rendFilmek
  putStrLn "rendFilm.txt megirva."
  let unknown     = BS.pack "Unknown"
      szineszek   = nub . filter (/= unknown) . map f3Szinesz   $ film3ok
      szinesznoik = nub . filter (/= unknown) . map f3Szineszno $ film3ok
  BS.writeFile "szineszek.txt" . BS.unlines $ szineszek
  putStrLn "szineszek.txt megirva."
  putStrLn $ "Szineszek szama: "  ++ show (length szineszek)
  putStrLn $ "Szinesznok szama: " ++ show (length szinesznoik)
  let csoportok = evCsoportok film3ok
  putStrLn $ "Ev-csoportok szama: " ++ show (length csoportok)
  let kerSzinesz = BS.pack "Brad Pitt"
      filmjei    = szineszFilmjei kerSzinesz film3ok
  putStrLn $ "\n" ++ BS.unpack kerSzinesz ++ " filmjei:"
  mapM_ (putStrLn . BS.unpack) filmjei
