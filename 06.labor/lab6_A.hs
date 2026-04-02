-- # 6. labor

-- I. Írjunk egy-egy Haskell függvényt, amely beolvass a billentyűzetről két természetes számot és kiírja a képernyőre

-- - a két szám közötti számok összegét,
main1 = do 
    putStr "x1="
    x1 <- readLn :: IO Int
    putStr "x2="
    x2 <- readLn :: IO Int
    putStrLn ("x1=" ++ show x1 ++", x2= "++ show x2)


main1_2 = do
    putStr "x1="
    x1 <- getline
    putStr "x2="
    x2 <- getline
    let x1Szam = read x1 :: Int
    let x2Szam = read x2 :: Int
    putStrLn ("x1=" ++ show x1Szam ++", x2= "++ show x2Szam)


osszeg ls = sum ls;
-- - a két szám közötti prímszámok összegét,
osztok x = [i | i <- [1..x], mod x i == 0]
primszam x = [1,x] == osztok x
primSzamosszeg ls = sum . filter primszam ls
-- - a két szám közötti azon számokat, amelyeknek legtöbb valódi osztója van.

-- II. Írjunk egy-egy Haskell függvényt, amely beolvassa a billentyűzetről az n természetes számot és kiírja a képernyőre

-- - n-ig a Fibonacci számok listáját ($n > 50$), úgy hogy a számok közé szóközt ír,
-- - n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír,
-- - az n kettes számrendszerbeli alakját, úgy hogy minden negyedik bit után egy szóközt ír,
-- - az n 16-os számrendszerbeli alakját, úgy hogy minden két szimbólum után egy szóközt ír, illetve az a, b, c, d, e, f szimbólumokat használja a 10-nél nagyobb számjegyek kódolására,
-- - az n értékének megfelelően a következő sorokat:

--   ```
--   (0, 0)
--   (0, 1) (1, 0)
--   (0, 2) (1, 1) (2, 0)
--   ...
--   (0, n) (1, n-1), ..., (n, 0)
--   ```
-- - az n értékének megfelelően az összes természetes szám kettes számrendszerbeli alakját,
--   például: $$n = 6:\ 0,\ 1,\ 10,\ 11,\ 100,\ 101,\ 110$$.

-- III. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről olvas be egész számokat egy listába, majd kiírja a képernyőre, hogy

-- - hányszor szerepel egy adott egész szám a listában,
-- - melyek azok az egész számok, amelyek kisebbek a listában megadott számok átlagértékénél,
-- - minden listabeli elem hányszor szerepel a listában, azaz készítsünk elem előfordulási statisztikát.

-- IV. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről végjelig olvas be karakterláncokat, és

-- - meghatározza a legnagyobbat,
-- - meghatározza a legnagyobb elemek indexét,
-- - az adatok rendezett sorrendjét.
