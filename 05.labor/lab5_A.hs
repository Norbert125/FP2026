-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
-- - meghatározza egy lista pozitív elemeinek összegét,
-- - egy lista páros elemeinek szorzatát,
-- - n-ig a négyzetszámokat.
-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe

import Data.Char

-- tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit . toLower)
-- tokenize =  map (irasjelHelyettesit . toLower)

irasjelHelyettesit :: Char -> Char
irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '

lenghtLista ls = map length ls

myMinimum2 :: (Num b, Enum b, Ord a) => [a] -> (a, [b])
myMinimum2 ls = (m, map snd $ filter fg $ zip ls [0,1..])
    where
        m = minimum ls
        fg k = fst k == m


-- main = do
--     let lista = "egy PrOBA szoveg. ez egy masik proBa! az Tobbfele irasJEL ::Hasznalat"
--     let r1 = tokenize lista
--     let r2 = lenghtLista r1
--     -- let m1 = minimum r2
--     putStrLn "a szavak hossza: " 
--     print r2
--     let m1 = myMinimum2 r2
--     print m1
--     let r3 = zip r1 r2
--     print r3



-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.

talalat x ls = l1
        where
            zipls = zip ls [0..]
            l1 = map snd $ filter(\y-> fst y == x) zipls


-- main = do
--     let a = 5
--     let l1 = [3,13,5,6,7,12,5,8,5]
--     let t1 = talalat 5 l1
--     let t2 = talalat 'e' "Bigeri-v:izeses"
--     -- print t1
--     let c1 = concatMap((<> " ") . show) t1
--     putStrLn $ show a <> " talalat pozicioi: " <> c1
--     -- print t2

-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
-- ls = [("golya",120,"MS"),("fecske",85,"CJ"),("cinege",132,"MS")]

ps ls = sum [t2 | (t1,t2,t3) <- ls]

ps2 ls r = sum [t2 | (t1,t2,t3) <- ls1]
    where 
        ls1 = filter (\(t1,t2,t3)-> t3 == r ) ls

main = do
    let ls = [("golya",120,"MS"),("fecske",85,"CJ"),("cinege",132,"MS")]
    let res = ps ls
    let madarLs = concatMap(<> " ") [t1 | (t1,t2,t3) <- ls]
    putStrLn $ madarLs <> " populacio szama: " ++ show res
    let ls1 = filter (\(t1,t2,t3)-> t3 == "MS" ) ls
    let madarLs2 = concatMap(<> " ") [t1 | (t1,t2,t3) <- ls1]
    let res2 = ps2 ls "MS" 
    putStrLn $ madarLs <> " " <> "MS" <> "-ben pop szam: " ++ show res2 
    putStrLn $ madarLs2 <> " " <> "MS" <> "-ben pop szam" ++ show res2
    
   

-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
