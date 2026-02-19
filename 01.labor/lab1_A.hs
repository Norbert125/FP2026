--I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a + b

kulombseg :: Double -> Double -> Double
kulombseg a b = (-) a b

szorzat a b = a * b

hanyados1 a b = a/ b

hanyados2 a b = div a b

osztmar a b = mod a b
-- egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> x = (-b) / a
elsoF a b = (-b) / a
-- egy szám abszulút értékét,
abszolut a = if a < 0 then -a else a

abszolut2 a 
 | a < 0 = -a
 | otherwise = a 
-- egy szám előjelét,
elojel a = if a < 0 then "neagtiv" else if a > 0 then "positiv" else "nulla"
elojel2 a
 | a < 0 = "negativ"
 | a > 0 = "pozitiv"
 | otherwise = "nulla"
-- két argumentuma közül a maximumot,
nagyobb a b = if a > b then a else if a < b then b else a
nagyobb2 a b 
  | a > b = a
  | a < b = b
  | otherwise = a
-- két argumentuma közül a minimumot,
kisebb a b = if a < b then a else if a > b then b else a
-- egy másodfokú egyenlet gyökeit,
-- a*(x**2) + b*x + c -> a,b,c bemeneti arg.
--delta = b**2 - 4*a*c
--gy1 = (-b + sqrt delta) / 2*a
--gy2 = (-b = sqrt delta) / 2*a 

masodF a b c 
  | delta  < 0 = error "komplex szamok"
  | otherwise = (gy1,gy2)
  where
    delta = b**2 - 4*a*c
    gy1 = (-b + sqrt delta) / (2*a)
    gy2 = (-b - sqrt delta) / (2*a)
-- hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
 -- Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- az n szám faktoriálisát (3 módszer),
-- az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).
