import Data.List              (sortBy)
import Data.Char              (isDigit, isUpper, isAlpha, ord)
import Data.Map.Strict        (Map)
import qualified Data.Map.Strict as Map

-- ===== Segédfüggvény =====

splitOn :: Char -> String -> [String]
splitOn _ ""  = [""]
splitOn c (x:xs)
  | x == c    = "" : rest
  | otherwise = (x : head rest) : tail rest
  where rest = splitOn c xs

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- ===== I. Feladat: Szöveg formázás =====
-- Az {.,!?;} halmazbeli írásjelek után szigorúan egy szóköz legyen.

formatText :: String -> String
formatText [] = []
formatText (c:cs)
  | c `elem` ".,!?;" = c : ' ' : formatText (dropWhile (== ' ') cs)
  | otherwise        = c : formatText cs

-- ===== II. Feladat: IBAN =====

rendezIBAN :: [String] -> [String]
rendezIBAN = sortBy compare

binarisKeresIBAN :: [String] -> String -> Bool
binarisKeresIBAN [] _ = False
binarisKeresIBAN xs s =
  let mid = length xs `div` 2
      m   = xs !! mid
  in case compare s m of
       EQ -> True
       LT -> binarisKeresIBAN (take mid xs) s
       GT -> binarisKeresIBAN (drop (mid + 1) xs) s

ervenyesKarakterek :: String -> Bool
ervenyesKarakterek = all (\c -> isDigit c || isUpper c)

-- Átcsoportosítás: első 4 karakter a végre kerül
atcsoportosit :: String -> String
atcsoportosit s = drop 4 s ++ take 4 s

-- Helyettesítés: A->10, B->11, ..., Z->35
helyettesit :: String -> String
helyettesit []     = []
helyettesit (c:cs)
  | isAlpha c = show (ord c - ord 'A' + 10) ++ helyettesit cs
  | otherwise = c : helyettesit cs

ellenorizMod :: String -> Bool
ellenorizMod s = (read s :: Integer) `mod` 97 == 1

ervenyesIBAN :: Map String Int -> String -> Bool
ervenyesIBAN hosszMap iban =
  length iban >= 4 &&
  ervenyesKarakterek iban &&
  maybe False (== length iban) (Map.lookup (take 2 iban) hosszMap) &&
  ellenorizMod (helyettesit (atcsoportosit iban))

-- ibanLength.txt formátuma: "CC hossz" (pl. "GB 22")
parseIBANHossz :: String -> (String, Int)
parseIBANHossz line =
  let ws = words line
  in (head ws, read (ws !! 1))

-- ===== III. Feladat: Személyek és névnapok =====

data Datum = Datum
  { nap   :: Int
  , honap :: Int
  , ev    :: Int
  } deriving (Show)

data Szemely = Szemely
  { vnev    :: String
  , knev    :: String
  , szdatum :: Datum
  } deriving (Show)

-- Bemeneti formátum: "YYYY-MM-DD"
parseDatum :: String -> Datum
parseDatum s =
  let [y, m, d] = splitOn '-' s
  in Datum (read d) (read m) (read y)

-- Bemeneti formátum: "vezeteknev,keresztnev,YYYY-MM-DD"
parseSzemely :: String -> Szemely
parseSzemely line =
  let [v, k, dStr] = splitOn ',' line
  in Szemely (trim v) (trim k) (parseDatum (trim dStr))

pad2 :: Int -> String
pad2 n
  | n < 10    = "0" ++ show n
  | otherwise = show n

-- Zeller-féle kongruencia (Gergely-naptár)
-- h: 0=Szombat, 1=Vasárnap, 2=Hétfő, ..., 6=Péntek
hetNapja :: Datum -> String
hetNapja (Datum d m y) =
  let (m', y') = if m <= 2 then (m + 12, y - 1) else (m, y)
      k        = y' `mod` 100
      j        = y' `div` 100
      h        = (d + (13 * (m' + 1)) `div` 5 + k + k `div` 4 + j `div` 4 - 2 * j) `mod` 7
      napok    = ["Szombat", "Vasárnap", "Hétfő", "Kedd", "Szerda", "Csütörtök", "Péntek"]
  in napok !! ((h `mod` 7 + 7) `mod` 7)

-- nevnapok.txt formátuma: "HH.NN. Nev1, Nev2" (pl. "01.01. Újév")
parseNevnap :: String -> (String, String)
parseNevnap line =
  let (dat, rest) = break (== ' ') (trim line)
  in (dat, trim rest)

keresNevnap :: [(String, String)] -> Int -> Int -> String
keresNevnap nevnapok h n =
  let kulcs = pad2 h ++ "." ++ pad2 n ++ "."
  in case lookup kulcs nevnapok of
       Just nv -> nv
       Nothing -> "ismeretlen"

-- ===== Main =====

main :: IO ()
main = do
  -- I. Szöveg formázás
  putStrLn "=== I. Feladat: Szöveg formázás ==="
  szoveg <- readFile "szoveg.txt"
  let formatalt = formatText szoveg
  writeFile "szoveg_formatalt.txt" formatalt
  putStrLn "szoveg_formatalt.txt megírva."

  -- II. IBAN
  putStrLn "\n=== II. Feladat: IBAN ==="
  ibanSor  <- readFile "iban.txt"
  hosszSor <- readFile "ibanLength.txt"
  let ibanok   = filter (not . null) . lines $ ibanSor
      rendezve = rendezIBAN ibanok
      hosszMap = Map.fromList . map parseIBANHossz . filter (not . null) . lines $ hosszSor
  putStrLn "Rendezett IBAN kódok (első 5):"
  mapM_ putStrLn (take 5 rendezve)
  let keresett = "GB82WEST12345698765432"
  putStrLn $ "\n" ++ keresett ++ " megtalálható: " ++ show (binarisKeresIBAN rendezve keresett)
  let okIban = filter (ervenyesIBAN hosszMap) ibanok
  writeFile "okIban.txt" (unlines okIban)
  putStrLn $ "Érvényes IBAN-ok: " ++ show (length okIban) ++ " → okIban.txt megírva."

  -- III. Személyek
  putStrLn "\n=== III. Feladat: Személyek ==="
  szemelysor <- readFile "szemelyek.txt"
  nevnapsor  <- readFile "nevnapok.txt"
  let szemelyek = map parseSzemely . filter (not . null) . lines $ szemelysor
      nevnapok  = map parseNevnap  . filter (not . null) . lines $ nevnapsor
  mapM_ (\sz -> do
    let d      = szdatum sz
        hetNap = hetNapja d
        nevnap = keresNevnap nevnapok (honap d) (nap d)
    putStrLn $ vnev sz ++ " " ++ knev sz
             ++ " – születési nap: " ++ hetNap
             ++ ", névnap: " ++ nevnap
    ) szemelyek
